# Description : Endogenous Spatial lags for Linear regression models
# Website : http://diffuseprior.wordpress.com/2013/08/18/endogenous-spatial-lags-for-the-linear-regression-model/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("spdep","ggplot2","reshape")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(spdep)
library(ggplot2)
library(reshape)

rm(list=ls())
n = 225
data = data.frame(n1=1:n)
# coords
data$lat = rep(1:sqrt(n), sqrt(n))
data$long = sort(rep(1:sqrt(n), sqrt(n)))
# create W matrix
wt1 = as.matrix(dist(cbind(data$long, data$lat), method = "euclidean", upper=TRUE))
wt1 = ifelse(wt1==1, 1, 0)
diag(wt1) = 0
# row standardize
rs = rowSums(wt1)
wt1 = apply(wt1, 2, function(x) x/rs)
lw1 = mat2listw(wt1, style="W")

rx = 0.25
rho = 0.4
b1 = 0.5
b2 = -0.5
b3 = 1.75
alp = 0.2

inv1 = invIrW(lw1, rho=rx, method="solve", feasible=NULL)
inv2 = invIrW(lw1, rho=rho, method="solve", feasible=NULL)

sims = 1000
beta1results = matrix(NA, ncol=4, nrow=sims)
beta2results = matrix(NA, ncol=4, nrow=sims)
beta3results = matrix(NA, ncol=4, nrow=sims)
rhoresults = matrix(NA, ncol=3, nrow=sims)

for(i in 1:sims){
  u1 = rnorm(n)
  x2 = inv1 %*% u1
  u2 = rnorm(n)
  x3 = inv1 %*% u2
  v1 = rnorm(n)
  e1 = alp*v1 + rnorm(n) 
  data1 = data.frame(cbind(x2, x3),lag.listw(lw1, cbind(x2, x3)))
  names(data1) = c("x2","x3","wx2","wx3")
  
  data1$y1 = inv2 %*% (b1 + b2*x2 + b3*x3 + rho*v1 + e1)
  
  data1$wy1 = lag.listw(lw1, data1$y1)
  data1$w2x2 = lag.listw(lw1, data1$wx2)
  data1$w2x3 = lag.listw(lw1, data1$wx3)
  data1$w3x2 = lag.listw(lw1, data1$w2x2)
  data1$w3x3 = lag.listw(lw1, data1$w2x3)
  
  m1 = coef(lm(y1 ~ x2 + x3, data1))
  m2 = coef(lm(y1 ~ wy1 + x2 + x3, data1))
  m3 = coef(lagsarlm(y1 ~ x2 + x3, data1, lw1))
  m4 = coef(stsls(y1 ~ x2 + x3, data1, lw1))
  
  beta1results[i,] = c(m1[1], m2[1], m3[2], m4[2])
  beta2results[i,] = c(m1[2], m2[3], m3[3], m4[3])
  beta3results[i,] = c(m1[3], m2[4], m3[4], m4[4])
  rhoresults[i,] = c(m2[2],m3[1], m4[1]) 
}

apply(rhoresults, 2, mean) ; apply(rhoresults, 2, sd)
apply(beta1results, 2, mean) ; apply(beta1results, 2, sd)
apply(beta2results, 2, mean) ; apply(beta2results, 2, sd)
apply(beta3results, 2, mean) ; apply(beta3results, 2, sd)

colnames(rhoresults) = c("OLS2","ML","2SLS")
colnames(beta1results) = c("OLS1","OLS2","ML","2SLS")
colnames(beta2results) = c("OLS1","OLS2","ML","2SLS")
colnames(beta3results) = c("OLS1","OLS2","ML","2SLS")

rhoresults = melt(rhoresults)
rhoresults$coef = "rho"
rhoresults$true = 0.4

beta1results = melt(beta1results)
beta1results$coef = "beta1"
beta1results$true = 0.5

beta2results = melt(beta2results)
beta2results$coef = "beta2"
beta2results$true = -0.5

beta3results = melt(beta3results)
beta3results$coef = "beta3"
beta3results$true = 1.75

data = rbind(rhoresults,beta1results,beta2results,beta3results)
data$Estimator = data$X2

ggplot(data, aes(x=value, colour=Estimator, fill=Estimator)) +
  geom_density(alpha=.3) +
  facet_wrap(~ coef, scales= "free") +
  geom_vline(aes(xintercept=true)) +
  scale_y_continuous("Density") +
  scale_x_continuous("Effect Size") +
  opts(legend.position = 'bottom', legend.direction = 'horizontal')