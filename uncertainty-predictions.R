# Description : Graphing the uncertainty of predictions
# Website : http://statistical-research.com/the-uncertainty-of-predictions/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("LearnBayes")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)


library(LearnBayes)
alpha = 0.05
y = rnorm(100,0, 1)
x1= y+rnorm(100,1,1)
x2= y+rnorm(100,2,2)

##### Least-squares fit
fit=lm(y~x1+x2,x=TRUE,y=TRUE)

new = data.frame(x1 = seq(-5, 5, 0.5), x2=seq(-3, 7, 0.5))

pred.w.plim = predict(fit, new, interval = "prediction")
pred.w.clim = predict(fit, new, interval = "confidence")
matplot(new$x1, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), col=c(1,2,2,3,3), type = "l", 
        ylab = "predicted y", xlab= "x1", main="Confidence vs. Prediction Intervals")
legend("topleft", c("Confidence","Prediction"), lwd=2, lty=c(2,3), col=c(2,3), cex=.7)

##### Sampling from posterior
theta.sample=blinreg(fit$y,fit$x,5000)


######### Model checking via posterior predictive distribution
pred.draws=blinregpred(fit$x,theta.sample)
pred.sum=apply(pred.draws,2,quantile,c(alpha/2,1-alpha/2))

par(mfrow=c(1,1))
ind=1:ncol( pred.sum )
ind.odd=seq(1,ncol( pred.sum ), by=2)
ind.even=seq(2,ncol( pred.sum ), by=2)
matplot(rbind(ind,ind),pred.sum,type="l",lty=1,col=1,
        xlab="Precinct Identifier",ylab="Candidate Vote", main="Posterior Predictive Distribution"
        , xaxt='n')
axis(side=1, at=ind.odd, tcl = -1.0, lty = 1, lwd = 0.5, labels=ind.odd, cex.axis=.75)
axis(side=1, at=ind.even, tcl = -0.7, lty = 1, lwd = 0.5, labels=rep("",length(ind.even)), cex.axis=.75)
points(ind,y,pch=19, cex=.4)

out=(y>pred.sum[2,] | y<pred.sum[1,])
text(ind[out], y[out], label=ind[out], pos = 1, cex=1, col=2)