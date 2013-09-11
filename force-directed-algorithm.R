# Description : Force-directed algorithm
# Website : http://rsnippets.blogspot.com/2013/09/visualizing-optimization-process.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("animation")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(animation)
b.init <- function(balls) {
  best.fit <<- +Inf
  pos <- runif(2 * balls)
  names(pos) <- paste(c("x", "y"), rep(1:balls, each = 2),
                      sep = "")
  return(pos)
}

b.plot <- function(pos) {
  par(mar = c(2, 2, 1.5, 0.5))
  # extract x and y coordinates of balls
  x <- pos[c(T, F)]
  y <- pos[c(F, T)]
  # plot balls
  plot(x, y, cex = 5, pch = 20, main = b.fit(pos),
       xlim = c(min(x) - 0.1, max(x) + 0.1),
       ylim = c(min(y) - 0.1, max(y) + 0.1))
  # and lines connecting them
  for (i in 1:(length(x) - 1)) {
    for (j in (i + 1):length(x)) {
      lines(x[c(i, j)], y[c(i, j)])
    }
  }
}

b.fit <- function(pos) {
  # our goal is to make distance between all balls equal to 0.5
  max(abs(dist(t(matrix(pos, 2))) - 0.5))
}

f <- function(pos) {
  fit <- b.fit(pos)
  # update plot if improvement is found
  if (fit < best.fit) {
    best.fit <<- fit
    b.plot(pos)
  }
  return(fit)
}

runa <- function() {
  set.seed(1)
  pos <- b.init(8)
  for (i in 1:20) {
    b.plot(pos)
  }
  pos <- optim(pos, f, method = "BFGS", control = list(maxit = 1000))$par
  for (i in 1:20) {
    b.plot(pos)
  }
}

ani.options(interval = 0.05)
saveGIF(runa())