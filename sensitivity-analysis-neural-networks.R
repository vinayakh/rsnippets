# Description : Sensitivity Analysis of Neural Networks
# Website : http://beckmw.wordpress.com/2013/10/07/sensitivity-analysis-for-neural-networks/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("clusterGeneration","nnet")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#define number of variables and observations
set.seed(2)
num.vars<-8
num.obs<-10000

#define correlation matrix for explanatory variables
#define actual parameter values
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms1<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms1) + rnorm(num.obs,sd=20)
parms2<-runif(num.vars,-10,10)
y2<-rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#prep data and create neural network
rand.vars<-data.frame(rand.vars)
resp<-apply(cbind(y1,y2),2, function(y) (y-min(y))/(max(y)-min(y)))
resp<-data.frame(resp)
names(resp)<-c('Y1','Y2')
mod1<-nnet(rand.vars,resp,size=8,linout=T)

# source('https://gist.github.com/fawda123/6860630/raw/e50fc6ef30b8269660b4e65aeec7ce02beb9b551/lek_fun.r')
lek.fun<-function(mod.in,var.sens=NULL,resp.name=NULL,steps=100,split.vals=seq(0,1,by=0.2),val.out=F){
  
  require(ggplot2)
  require(reshape)
  
  #sort out exp and resp names based on object type of call to mod.in
  if(is.null(mod.in$call$formula)){
    if(is.null(resp.name)) resp.name<-colnames(eval(mod.in$call$y))
    if(is.null(var.sens)) var.sens<-colnames(eval(mod.in$call$x))
    mat.in<-eval(mod.in$call$x)
  }
  else{
    forms<-eval(mod.in$call$formula)
    dat.names<-model.frame(forms,data=eval(mod.in$call$data))
    if(is.null(resp.name)) resp.name<-as.character(forms)[2]
    if(is.null(var.sens)) 
      var.sens<-names(dat.names)[!names(dat.names) %in% as.character(forms)[2]]
    mat.in<-dat.names[,!names(dat.names) %in% as.character(forms)[2]]
  }	
  
  #gets predicted output for nnet based on matrix of explanatory variables
  #selected explanatory variable is sequenced across range of values
  #all other explanatory variables are held constant at value specified by 'fun.in'
  pred.sens<-function(mat.in,mod.in,var.sel,step.val,fun.in,resp.name){
    
    mat.out<-matrix(nrow=step.val,ncol=ncol(mat.in),dimnames=list(c(1:step.val),colnames(mat.in)))
    
    mat.cons<-mat.in[,!names(mat.in) %in% var.sel]
    mat.cons<-apply(mat.cons,2,fun.in)
    mat.out[,!names(mat.in) %in% var.sel]<-t(sapply(1:step.val,function(x) mat.cons))
    
    mat.out[,var.sel]<-seq(min(mat.in[,var.sel]),max(mat.in[,var.sel]),length=step.val)
    
    out<-data.frame(predict(mod.in,new=as.data.frame(mat.out)))
    names(out)<-resp.name
    x.vars<-mat.out[,var.sel]
    data.frame(out,x.vars)
    
  }
  
  #use 'pred.fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek.vals<-sapply(
    var.sens,
    function(vars){
      sapply(
        split.vals,
        function(splits){
          pred.sens(
            mat.in,
            mod.in,
            vars,
            steps,
            function(val) quantile(val,probs=splits),
            resp.name
          )
        },
        simplify=F
      )
    },
    simplify=F	
  )
  
  #melt lek.val list for use with ggplot
  lek.vals<-melt.list(lek.vals,id.vars='x.vars')
  lek.vals$L2<-factor(lek.vals$L2,labels=split.vals)
  names(lek.vals)<-c('Explanatory','resp.name','Response','Splits','exp.name')
  
  #return only values if val.out = T
  if(val.out) return(lek.vals)
  
  #ggplot object
  p<-ggplot(lek.vals,aes(x=Explanatory,y=Response,group=Splits)) + 
    geom_line(aes(colour=Splits,linetype=Splits,size=Splits)) + 
    facet_grid(resp.name~exp.name) +
    scale_linetype_manual(values=rep('solid',length(split.vals))) +
    scale_size_manual(values=rep(1,length(split.vals)))
  
  return(p)
  
}

lek.fun(mod1)

lek.fun(mod1,var.sens=c('X2','X5'),split.vals=seq(0,1,by=0.05))

p1<-lek.fun(mod1)
class(p1)
p1 +
  theme_bw() +
  scale_colour_brewer(palette="PuBu") +
  scale_linetype_manual(values=rep('dashed',6)) +
  scale_size_manual(values=rep(1,6))

head(lek.fun(mod1,val.out=T))
mod2<-lm(Y1~.,data=cbind(resp[,'Y1',drop=F],rand.vars))
lek.fun(mod2)