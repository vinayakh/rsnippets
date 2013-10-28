# Description : Regression Regularisation Example
# Website : http://rsnippets.blogspot.in/2013/05/regression-regularization-example.html

doInstall <- TRUE
toInstall <- c("parallel", "lasso2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

run <- function(job) {
  require(lasso2)
  
  gen.data <- function(v, n) {
    data.set <- data.frame(replicate(v, runif(n)))
    # true y is equal to sum of x
    data.set$y <- rowSums(data.set)
    names(data.set) <- c(paste("x", 1:v, sep = ""), "y")
    return(data.set)
  }
  
  v <- 50
  n <- 60
  
  data.set <- gen.data(v, n)
  # add noise to y in training set
  data.set$y <- data.set$y + rnorm(n)
  new.set <- gen.data(v, 10000)
  model.lm <- lm(y ~ ., data.set)
  model.aic <- step(model.lm, trace = 0)
  model.bic = step(model.lm, trace = 0, k = log(n))
  model.lasso <- l1ce(y ~ ., data.set,
                      sweep.out = NULL, standardize = FALSE)
  models = list(model.lm, model.aic, model.bic, model.lasso)
  results <- numeric(length(models))
  for (j in seq_along(models)) {
    pred <- predict(models[[j]], newdata = new.set)
    results[j] <- mean((pred - new.set$y) ^ 2)
  }
  return(results)
}
cl <- makeCluster(4)
system.time(msd <- t(parSapply(cl, 1:100, run))) # 58.07 seconds
stopCluster(cl)

colnames(msd) = c("lm", "aic", "bic", "lasso")
par(mar = c(2, 2, 1, 1))
boxplot(msd)
for (i in 1:ncol(msd)) {
  lines(c(i - 0.4, i + 0.4), rep(mean(msd[, i]), 2),
        col = "red", lwd = 2)
  
}
