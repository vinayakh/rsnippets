# Description : Fitting a model by Maximum Likelihood
# Website : http://www.exegetic.biz/blog/2013/08/fitting-a-model-by-maximum-likelihood/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("stats4","bbmle")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Create a random normal distribution
set.seed(1001)
N <- 100
x <- rnorm(N, mean = 3, sd = 2)
mean(x)
sd(x)

# Create a MLE function
LL <- function(mu, sigma) {
  R = dnorm(x, mu, sigma)
  -sum(log(R))
}

library(stats4)
mle(LL, start = list(mu = 1, sigma=1))
dnorm(x, 1, -1)
mle(LL, start = list(mu = 1, sigma=1), method = "L-BFGS-B", lower = c(-Inf, 0), upper = c(Inf, Inf))
LL <- function(mu, sigma) {
  R = suppressWarnings(dnorm(x, mu, sigma))
  -sum(log(R))
}
mle(LL, start = list(mu = 1, sigma=1))

x <- runif(N)
y <- 5 * x + 3 + rnorm(N)

fit <- lm(y ~ x)
summary(fit)

plot(x, y)
abline(fit, col = "red")

LL <- function(beta0, beta1, mu, sigma) {
  R = y - x * beta1 - beta0
  R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
  -sum(R)
}

fit <- mle(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))
fit <- mle(LL, start = list(beta0 = 5, beta1 = 3, mu = 0, sigma=1))

fit <- mle(LL, start = list(beta0 = 4, beta1 = 2, mu = 0, sigma=1))
fit

summary(fit)
fit <- mle(LL, start = list(beta0 = 2, beta1 = 1.5, sigma=1), fixed = list(mu = 0),
             nobs = length(y))
summary(fit)

AIC(fit)
BIC(fit)
logLik(fit)

library(bbmle)
fit <- mle2(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma = 1))
summary(fit)
