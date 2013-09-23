# Description : Bootstrap benchmark in R
# Website : 
http://climateecology.wordpress.com/2013/08/19/r-vs-python-speed-comparison-for-bootstrapping/
set.seed(101)

# generate data
x <- 0:100
y <- 2*x + rnorm(101, 0, 10)

# plot data
plot(x, y)

# run the regression
mod1 <- lm(y ~ x)
yHat <- fitted(mod1)

# get the residuals
errors <- resid(mod1)

# make a bootstrapping function
boot <- function(n = 10000){
  b1 <- numeric(n)
  b1[1] <- coef(mod1)[2]
  
  for(i in 2:n){
    residBoot <- sample(errors, replace=F)
    yBoot <- yHat + residBoot
    modBoot <- lm(yBoot ~ x)
    b1[i] <- coef(modBoot)[2]
  }
  
  return(b1)
}

# Run the bootstrapping function
system.time( bootB1 <- boot() )
mean(bootB1)