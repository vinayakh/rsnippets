# Description : Parallel computation using the foreach package
# Website : http://www.exegetic.biz/blog/2013/08/the-wonders-of-foreach/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("foreach","numbers", "doMC","rbenchmark","doSNOW","boot")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate Data
max.eig <- function(N, sigma) {
  d <- matrix(rnorm(N**2, sd = sigma), nrow = N)
  E <- eigen(d)$values
  abs(E)[[1]]
}

# Check data
max.eig(5, 1)
max.eig(5, 1)

# Vectorise
E = sapply(1:10000, function(n) {max.eig(5, 1)})
summary(E)
hist(E)

# Vary Dimensions
sapply(1:5, function(n) {max.eig(n, 1)})
sapply(1:5, function(n) {sapply(1:3, function(m) {max.eig(n, m)})})

# Load and use the foreach library
library(foreach)
times(10) %do% max.eig(5, 1)
foreach(n = 1:5) %do% max.eig(n, 1)
foreach(n = 1:5, .combine = c) %do% max.eig(n, 1)
foreach(n = 1:5) %:% foreach(m = 1:3) %do% max.eig(n, m)
foreach(n = 1:5, .combine = rbind) %:% foreach(m = 1:3) %do% max.eig(n, m)
foreach(n = 1:5, .combine = cbind) %:% foreach(m = 1:3) %do% max.eig(n, m)
foreach(n = 1:5, m = 1:5) %do% max.eig(n, m)

# Filtering
library(numbers)
foreach(n = 1:10000, .combine = c) %:% when (isPrime(n)) %do% n

# Going Parallel
foreach(n = 1:5) %dopar% max.eig(n, 1)
# Should give the following error
# Warning message:
# executing %dopar% sequentially: no parallel backend registered 

# Multicore
library(doMC)
registerDoMC(cores=4)

# Benchmark
library(rbenchmark)
benchmark(foreach(n = 1:50) %do% max.eig(n, 1),
          foreach(n = 1:50) %dopar% max.eig(n, 1))

# Cluster
library(doSNOW)
cluster = makeCluster(4, type = "SOCK")
registerDoSNOW(cluster)
benchmark(foreach(n = 1:50) %do% max.eig(n, 1),
          foreach(n = 1:50) %dopar% max.eig(n, 1))
stopCluster(cluster)

# MPI
cluster = makeCluster(20, type = "MPI")
registerDoSNOW(cluster)
benchmark(foreach(n = 1:100) %do% max.eig(n, 1),
          foreach(n = 1:100) %dopar% max.eig(n, 1))


random.data <- matrix(rnorm(1000000), ncol = 1000)
bmed <- function(d, n) median(d[n])
library(boot)
sapply(1:100, function(n) {sd(boot(random.data[, n], bmed, R = 10000)$t)})

clusterExport(cluster, c("random.data", "bmed"))

results = clusterApply(cluster, 1:100, function(n) {
  library(boot)
  sd(boot(random.data[, n], bmed, R = 10000)$t)
})

head(unlist(results))

# The foreach implementation is a little neater.
results = foreach(n = 1:100, .combine = c) %dopar% {
  library(boot); sd(boot(random.data[, n], bmed, R = 10000)$t)
}
head(results)
stopCluster(cluster)