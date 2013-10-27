# Description : Cont model and ggplot2
# Website : http://rsnippets.blogspot.in/2013/10/cont-model-back-after-year.html

doInstall <- TRUE
toInstall <- c("e1071","mgcv","ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

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

sim.points <- 60000
d <- runif(sim.points,0.001,0.01)
l <- runif(sim.points,5,20)
s <- runif(sim.points,0.01,0.1)
k <- mapply(function(d, l, s) {
  kurtosis(cont.run(1000, 10000, 1000, d, l ,s))
}, d, l, s)

data.set <- read.table("data/sim_output.txt", head = T,
                       colClasses = rep("numeric", 4))
data.set$cd <- cut(data.set$d, seq(0.001, 0.01, len = 10))
data.set$cl <- cut(data.set$l, seq(5, 20, len = 16))
data.set$cs <- cut(data.set$s, seq(0.01, 0.1, len = 10))
data.set$p.excess <- as.numeric(data.set$k > 0)
sum.data <- aggregate(p.excess ~ cd + cl + cs, data = data.set, mean)

for (i in levels(sum.data$cs)[c(1:9, 8:2)]) {
  print(ggplot() +
          geom_point(data = sum.data[sum.data$cs == i,],
                     aes(x = cl, y = cd, colour = p.excess),
                     shape = 15, size = 10) +
          scale_colour_gradient(low = "blue", high = "red") +
          theme(panel.background = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                text=element_text(colour = "black", size = 14),
                axis.text.x=element_text(angle = -90)) +
          ggtitle(paste("cs:", i)))
}