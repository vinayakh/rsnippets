# Description : Drawing a 95% confidence interval in R
# Website : http://climateecology.wordpress.com/2013/08/05/drawing-a-95-confidence-interval-in-r/

# Script to find 95% confidence interval
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("mvtnorm", "ellipse")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE
       
library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()
set.seed(17)

# Set the covariance matrix
sigma2 <- matrix(c(5, 2, 2, 5), ncol=2)

# Set the means
mu <- c(5,5)

# Get the correlation matrix
P <- cov2cor(sigma2)

# Generate the data
p <- rmvnorm(n=50, mean=mu, sigma=sqrt(sigma2))

# Plot the data
plot(p)

# Plot the ellipse
lines( ellipse( P, centre = c(5,5)) , col='red')

evals <- eigen(P)$values
evecs <- eigen(P)$vectors

# Angles of a circle
a <- seq(0, 2*pi, len=100)

# Get critical value
c2 <- qchisq(0.95, 2)
c <- sqrt(c2)

# Get the distances
xT <- c * sqrt(evals[1]) * cos(a)
yT <- c * sqrt(evals[2]) * sin(a)

M <- cbind(xT, yT)

# Covert the coordinates
transM <- evecs %*% t(M)
transM <- t(transM)

lines(transM + mu)
