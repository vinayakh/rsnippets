# Description : Benchmarking matrix creation
# Website : http://is-r.tumblr.com/post/34157529988/benchmarking-matrix-creation

# A method to make a matrix by repeating a vector

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("rbenchmark", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

simpleLoop <- function(vec = 1:10, nc = 5){ # Expected to be slow
  out <- c()
  for(ii in 1:nc){
    out <- cbind(out, vec)
  }
  return(out)
}

preAllocated <- function(vec = 1:10, nc = 5){ # Expected to be faster
  out <- matrix(NA, ncol = nc, nrow = length(vec))
  for(ii in 1:nc){
    out[, ii] <- vec
  }
  return(out)
}

addSweep <- function(vec = 1:10, nc = 5){ # "Sweeping" the vector through
  out <- matrix(0, ncol = nc, nrow = length(vec))
  sweep(out, 1, vec, "+")
}

repVector <- function(vec = 1:10, nc = 5){ # Expected to be fastest
  replicate(nc, vec)
}

outerProduct <- function(vec = 1:10, nc = 5){ # Just figured this one out,
  outer(vec, rep(1, nc)) # matrix algebra!
}

simpleLoop(1:15, 15) # Examples
repVector(1:15, 15)
outerProduct(1:15, 15) # etc...

setVector <- 1:100 # These parameters pass onto benchmark test
setColumns <- 100

# Benchmark testing
benchmarkResults <- within(benchmark(
  outerProduct = outerProduct(setVector, setColumns),
  repVector = repVector(setVector, setColumns),
  addSweep = addSweep(setVector, setColumns),
  preAllocated = preAllocated(setVector, setColumns),
  simpleLoop = simpleLoop(setVector, setColumns),
  replications = 10 ^ (1:4),
  columns = c('test', 'replications', 'elapsed'),
  order = c('test', 'replications')),
{ average = elapsed / replications })
benchmarkResults

# Plot, showing that our matrix algebra method is slightly faster:
zp1 <- ggplot(benchmarkResults,
              aes(x = average, y = test, colour = log(replications, 10)))
zp1 <- zp1 + geom_path(aes(group = replications))
zp1 <- zp1 + geom_point(size = 5)
print(zp1)