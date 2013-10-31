# Description : Model fitting
# Website : http://rsnippets.blogspot.in/2013/05/model-fitting-exam-problem.html


doInstall <- TRUE
toInstall <- c("rpart", "nnet")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

run <- function(n) {
  x <- runif(n)
  y <- 10 * x + rnorm(n)
  new.x <- data.frame(x = seq(0, 1, len = 10000))
  models <- list(linear = lm(y ~ x),
                 tree   = rpart(y ~ x),
                 nnet2  = nnet(y ~ x, size = 2,
                               trace = F, linout = T),
                 nnet10 = nnet(y ~ x, size = 10,
                               trace = F, linout = T))
  sapply(models, function(model) {
    pred <- predict(model, newdata = new.x)
    sum((pred - 10 * new.x$x) ^ 2)
  })
}

set.seed(1)
for (n in c(20, 200)) {
  cat("--- n =", n, "---\n")
  print(summary(t(replicate(100, run(n)))))
}