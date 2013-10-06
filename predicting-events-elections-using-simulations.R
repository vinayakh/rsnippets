# Description : Predicting events using simulations
# Wesbite : http://is-r.tumblr.com/post/35266021903/five-thirty-hate

doInstall <- TRUE
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Coin and Die tosses
Coin <- c()
Die <- c()

for (i in 1:1000) {
  Coin[i] <- sample(c("Heads", "Tails"), 1)
  Die[i] <- sample(c(1,2,3,4,5,6), 1)
}
table(Coin, Die)

# NBA Playoffs data
Tiger <- c()
LeBron <- c()
for (i in 1:1000) {
  LeBron[i] <- sample(c( rep("Makes FT", 77), rep("Misses FT", 23)), 1)
  Tiger[i] <- sample(c( rep("Fairway", 49), rep("Not FW", 51)), 1)
}
table(LeBron, Tiger)

# Some more

N <- 1:3
SE <- c(.272, .19, .16)
HI <- c(1.2, 1.0, .98)
LO <- c( .13, .29, .36)
AVG <- c(.667, .667, .667)
Shots <- c(1,0,1)
for (i in 4:50) {
  Shots[i] <- sample(c(1,0,1), 1)
  N[i] <- i
  SE[i] <- sqrt(mean(Shots)/N[i])
  HI[i] <- mean(Shots) + 1.96 * SE[i]
  LO[i] <- mean(Shots) - 1.96 * SE[i]
  AVG[i] <- mean(Shots)
}
qplot(N, AVG, ymin=LO, ymax=HI, geom="pointrange", colour=-AVG, size=I(.9)) +theme_bw() + scale_colour_gradientn(colours=c("red", "blue")
) + geom_hline(y=.43) +labs(title = "Your Recruit Over 50 Shots \n") + ylab("Pct 3pt Shot Made \n")

# Elections data
Ohio <- c()
California <- c()
Alabama <- c()
for ( i in 1:1000) {
  Ohio[i] <- sample (c("Obama", "Romney"), 1)
  California[i] <- sample(c(rep("Obama", 9), "Romney"))
  Alabama[i] <- sample( c(rep("Obama", 1), rep("Romney", 9)), 1)
}
ftable(Ohio, California, Alabama)
