# Description : Contour plot of linear predictor
# Website : http://mbjoseph.github.io/blog/2013/08/18/vague_intxn/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# parameter estimates
beta0 <- -.004
beta1 <- 1.055
beta2 <- -.496
beta3 <- 2.002

# reader's guess: range of possible covariate values
x1 <- seq(-5, 5, .1)
x2 <- seq(-5, 5, .1)
X <- expand.grid(x1=x1, x2=x2)

# reader's attempt to know how the covariates relate to E(y)
mu <- with(X, beta0 + beta1*x1 + beta2*x2 + beta3*x1*x2)

require(ggplot2)
d <- data.frame(mu=mu, x1=X$x1, x2=X$x2)
p1 <- ggplot(d, aes(x1, x2, z=mu)) + theme_bw() +
  geom_tile(aes(fill=mu)) +
  stat_contour(binwidth=1.5) +
  scale_fill_gradient2(low="blue", mid="white", high="orange") +
  xlab("Covariate 1") + ylab("Covariate 2") +
  ggtitle("Contour plot of the linear predictor")
p1