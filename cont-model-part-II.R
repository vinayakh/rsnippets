# Description : Cont Model Part II
# Website : http://rsnippets.blogspot.in/2013/10/cont-model-part-ii.html

cont.run <- function(burn.in, reps, n, d, l ,s) {
  tr <- rep(0, n)
  sig <- rnorm(reps, 0, d)
  r <- rep(0, reps)
  for (i in 1:reps) {
    r[i] <- (sum(sig[i] > tr) - sum(sig[i] < (-tr))) / (l * n)
    tr[runif(n) < s] <- abs(r[i])
  }
  return(r[burn.in:reps])
}

set.seed(1)
sim.points <- 100
d <- runif(sim.points, 0.002, 0.01)
l <- runif(sim.points, 5, 10)
s <- runif(sim.points, 0.01, 0.1)
m <- runif(sim.points, 1, 2) # comparison multiplier
seeds <- runif(sim.points)   # common random numbers seeds

range(mapply(function(d, l, s, m, seed) {
  set.seed(seed)
  r1 <- cont.run(1000, 10000, 1000, d, l ,s)
  set.seed(seed)
  r2 <- cont.run(1000, 10000, 1000, d / m, l * m ,s)
  range(r1 / m - r2)
  
}, d, l, s, m, seeds)) # -2.775558e-17  1.387779e-17

library(lattice)

data.set <- read.table("data/sim_output.txt", head = T,
                       colClasses = rep("numeric", 4))
data.set$dl <- data.set$d * data.set$l
data.set$cs <- cut(data.set$s, seq(0.01, 0.1, len = 10))
data.set$cdl <- cut(data.set$dl, seq(0, 0.2, len = 11))
sum.data <- aggregate(k ~ cdl + cs, data = data.set, mean)
trellis.par.set(regions=list(col=topo.colors(100)))
levelplot(k~cdl+cs, data=sum.data,scales=list(x=list(rot=90)),
          xlab = "d * l", ylab = "s")

cont.run.vol <- function(burn.in, reps, n, d, l ,s) {
  tr <- rep(0, n)
  sig <- rnorm(reps, 0, d)
  r <- rep(0, reps)
  t <- rep(0, reps)
  for (i in 1:reps) {
    r[i] <- (sum(sig[i] > tr) - sum(sig[i] < (-tr))) / (l * n)
    t[i] <- (sum(sig[i] > tr) + sum(sig[i] < (-tr))) / n
    tr[runif(n) < s] <- abs(r[i])
  }
  c(kurtosis(r[burn.in:reps]), mean(t[burn.in:reps]))
}
library(e1071)
sim.points <- 100
d <- runif(sim.points,0.001,0.01)
l <- runif(sim.points,5,20)
s <- runif(sim.points,0.01,0.1)
data.set <- mapply(function(d, l, s) {
  cont.run.vol(1000, 10000, 1000, d, l ,s)
}, d, l, s)
data.set <- t(data.set)
colnames(data.set) <- c("kurtosis", "volume")
data.set <- data.set[, 2:1]
par(mar=c(4, 4, 1, 1))
plot(data.set)