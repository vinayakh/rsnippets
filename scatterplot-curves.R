# Description : Comparison of models and adding curves to scatterplots
# Website : http://statistical-research.com/thats-smooth/

toInstall <- c("graphics","splines")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
library(ggplot2)

R = matrix(cbind(1,.99, .99,1),nrow=2)
U = t(chol(R))
nvars = dim(U)[1]
numobs = 1000
set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,10,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))
names(raw) = c("response","predictor1")
raw$predictor1.3 = raw$predictor1^3
raw$predictor1.2 = raw$predictor1^2
fit = lm(raw$response ~ raw$predictor1.3)

plot(raw$response ~ raw$predictor1.3, pch=16, cex=.4, xlab="Predictor", ylab="Response", main="Simulated Data with Slight Curve")
abline(fit)

x = with(cars, speed)
y = with(cars, dist)
eval.length = 50

# This LOESS shows two different R function arriving at the same solution.
# Careful using the LOESS defaults as they differ and will produce different solutions.
fit.loess = loess.smooth(x, y, evaluation = eval.length,
                         family="gaussian", span=.75, degree=1)
fit.loess2= loess(y ~ x, family="gaussian",
                  span=.75, degree=1)

## Set a simple 95% CI on the fit.loess model
new.x = seq(min(x),max(x), length.out=eval.length)
ci = cbind(
  predict(fit.loess2, data.frame(x=new.x)),
  predict(fit.loess2, data.frame(x=new.x))+
    predict(fit.loess2, data.frame(x=new.x), se=TRUE)$se.fit*qnorm(1-.05/2),
  predict(fit.loess2, data.frame(x=new.x))-
    predict(fit.loess2, data.frame(x=new.x), se=TRUE)$se.fit*qnorm(1-.05/2)
)

## Linear Model
fit = lm(y ~ x )

## Polynomial
fit.3 = lm(y ~ poly(x,3) )

## Natural Spline
fit.ns.3 = lm(y ~ ns(x, 3) )
## Smoothing Spline
fit.sp = smooth.spline(y ~ x, nknots=15)

plot(x,y, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), pch=16, cex=.5,
     ylab = "Stopping Distance (feet)", xlab= "Speed (MPH)", main="Comparison of Models"
     , sub="Splines")
## Add additional models on top of graph. It can get cluttered with all the models.

## LOESS with Confidence Intervals
matplot(new.x, ci, lty = c(1,2,2), col=c(1,2,2), type = "l", add=T)
## Linear
lines(new.x, predict(fit, data.frame(x=new.x)), col='orange', lty=3)
## Polynomial
lines(new.x, predict(fit.3, data.frame(x=new.x)), col='light blue', lty=4)
## Natural Spline
lines(new.x, predict(fit.ns.3, data.frame(x=new.x)), col='green', lty=5)
## Smoothing Spline
lines(fit.sp, col='blue', lty=6)
## Kernel Curve
lines(ksmooth(x, y, "normal", bandwidth = 5), col = 'purple', lty=7)
legend("topleft",c("Linear","Polynomial","Natural Spline","Smoothing Spline","Kernel"),
       col=c('black','light blue','green','blue','purple'), lty=c(3,4,5,6,7), lwd=2)