# Description : Calculating Area under the Curve
# Website : http://bayesianbiologist.com/2013/10/10/calculating-auc-the-hard-way/
# Extra info : http://fastml.com/what-you-wanted-to-know-about-auc/

## Descrete integration for AUC calc
## Î”x.y1 + 1/2Î”x.Î”y  <- summation of trapezoids
desc_integrate<-function(x,y)
{
  f<-cbind(x,y)
  ## Sort by x, then by y (assending)
  f<-f[order(f[,1],f[,2]),] 
  dint<-0
  x<-f[,1]
  y<-f[,2]
  dint<-sapply(2:length(x),function(i){
    (x[i]-x[i-1])*y[i-1] + 0.5*(x[i]-x[i-1]) * (y[i]-y[i-1])})
  dint<-sum(dint)
  return(dint)
}

## This is a handy generic, put it somewhere you dink.
add_error_bars<-function(data,error,dimensions=1,...)
{
  for(i in 1:length(data[,1]))
  {
    # y axis is 1st dimension
    arrows(data[i,1],data[i,2],data[i,1],error[i,1],angle=90,...)
    arrows(data[i,1],data[i,2],data[i,1],error[i,2],angle=90,...)
    
    if(dimensions==2)
    {
      arrows(data[i,1],data[i,2],error[i,3],data[i,2],angle=90,...)
      arrows(data[i,1],data[i,2],error[i,4],data[i,2],angle=90,...)
    }
  }
}


####################################################################
## Calculate the AUC and optionally plot the ROC
##  **Usage**
## d: a vector of logicals (0,1)
## pred: a vector of predicted values on range [0,1]
## plot: logical - plot or not
## error_bars: logical - add error bars or not
## ci: atomic vector - confidence interval width for error bars
## res: atomic vector - resolution of the thresholds to test
####################################################################
AUC<-function(d,pred,plot=FALSE,error_bars=FALSE,ci=0.95,res=100,add=FALSE,...)
{
  n<-length(d)
  dt<-seq(0,1,length.out=res)
  tp<-numeric(res)
  fp<-numeric(res)
  fn<-numeric(res)
  
  error<-array(dim=c(res,4)) # <tp upper, tp lower, fp upper, fp lower>
  sapply(1:res,function(i)
  {
    tp[i]<<- sum( d[pred > dt[i] ] )/ sum(d)
    fp[i]<<- sum( d[pred > dt[i] ] == 0 )/ sum(!d)
    fn[i]<<- sum( d[pred < dt[i] ] )/ sum(d) 
    
    
    #Calculate CI based on the beta distribution
    alpha_tp<-sum( d[pred > dt[i] ] ) + 1
    beta_tp<- sum(d) - sum( d[pred > dt[i] ] ) + 1
    error[i,1]<<-qbeta((1-ci)/2,alpha_tp,beta_tp)   #ci% bounds based on beta dist
    error[i,2]<<-qbeta(1-(1-ci)/2,alpha_tp,beta_tp)
    
    alpha_fp<- sum( d[pred > dt[i] ] == 0 ) + 1
    beta_fp<- sum(!d) - sum( d[pred > dt[i] ] == 0 ) + 1
    error[i,3]<<-qbeta((1-ci)/2,alpha_fp,beta_fp)   #ci% bounds based on beta dist
    error[i,4]<<-qbeta(1-(1-ci)/2,alpha_fp,beta_fp)
  })
  
  # Which threshold value minimises
  # the sum of the error rates.
  opt_thresh<-dt[which.min(fp+fn)]
  
  # Ensure collisions at 0,0 and 1,1
  fp<-c(1,fp,0)
  tp<-c(1,tp,0)
  
  # Integrate the ROC
  auc<-desc_integrate(fp,tp)
  
  if(plot)
  {
    if(add)
    {
      lines(fp,tp,type='b',pch=20)
    }else{
      plot(fp,tp,type='b',pch=20,xlim=c(0,1),ylim=c(0,1),...)
      text( 0.8,0.2,paste('AUC =',round(auc,3)) )
      abline(0,1,lty=2)
    }
    
    if(error_bars)
      add_error_bars(cbind(fp[2:(res+1)],tp[2:(res+1)]),error,dimensions=2,length=0.01)
  }
  return(list(auc=auc,opt_thresh=opt_thresh))
}

bs<-function(p)
{
  U<-runif(length(p),0,1)
  outcomes<-U<p
  return(outcomes)
}

# Simulate some binary outcomes #
n <- 100
x <- runif(n,-3,3)
p <- 1/(1+exp(-x))
y <- bs(p)

# Using my overly verbose code at https://gist.github.com/cjbayesian/6921118
AUC(d=y,pred=p,res=500,plot=TRUE)

## The hard way (but with fewer lines of code) ##
N <- 10000000
r_pos_p <- sample(p[y==1],N,replace=TRUE)
r_neg_p <- sample(p[y==0],N,replace=TRUE)

# Monte Carlo probability of a randomly drawn 1 having a higher score than
# a randomly drawn 0 (AUC by definition):

rAUC <- mean(r_pos_p > r_neg_p)
print(rAUC)