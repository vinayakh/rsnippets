# Description : N armed bandit simulation
# Website : http://random-miner.blogspot.ca/2013/08/n-armed-bandit-simulation.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE

get.testbed = function(arms = 10, plays = 500, u = 0, sdev.arm = 1, sdev.rewards = 1){
  optimal = rnorm(arms, u, sdev.arm)
  rewards = sapply(optimal, function(x)rnorm(plays, x, sdev.rewards))
  list(optimal = optimal, rewards = rewards)
}

play.slots = function(arms = 10, plays = 500, u = 0, sdev.arm = 1, sdev.rewards = 1, eps = 0.1){
  testbed = get.testbed(arms, plays, u, sdev.arm, sdev.rewards)
  optimal = testbed$optimal
  rewards = testbed$rewards
  optim.index = which.max(optimal)
  slot.rewards = rep(0, arms)
  reward.hist = rep(0, plays)
  optimal.hist = rep(0, plays)
  pulls = rep(0, arms)
  probs = runif(plays)
  # vetorizar
  for (i in 1:plays){
    #idx = ifelse(probs[i] < eps, sample(arms, 1), which.max(slot.rewards))
    idx = if (probs[i] < eps) sample(arms, 1) else which.max(slot.rewards)
    reward.hist[i] = rewards[i, idx]
    if (idx == optim.index)
      optimal.hist[i] = 1
    slot.rewards[idx] = slot.rewards[idx] + (rewards[i, idx] - slot.rewards[idx])/(pulls[idx] + 1)
    pulls[idx] = pulls[idx] + 1
  }
  list(slot.rewards = slot.rewards, reward.hist = reward.hist, optimal.hist = optimal.hist, pulls = pulls)
}

do.simulation = function(N = 100, arms = 10, plays = 500, u = 0, sdev.arm = 1, sdev.rewards = 1, eps = c(0.0, 0.01, 0.1)){
  n.players = length(eps)
  col.names = paste('eps', eps)
  rewards.hist = matrix(0, nrow = plays, ncol = n.players)
  optim.hist = matrix(0, nrow = plays, ncol = n.players)
  colnames(rewards.hist) = col.names
  colnames(optim.hist) = col.names
  for (p in 1:n.players){
    for (i in 1:N){
      play.results = play.slots(arms, plays, u, sdev.arm, sdev.rewards, eps[p])
      rewards.hist[, p] = rewards.hist[, p] + play.results$reward.hist
      optim.hist[, p] = optim.hist[, p] + play.results$optimal.hist
    }
  }
  rewards.hist = rewards.hist/N
  optim.hist = optim.hist/N
  optim.hist = apply(optim.hist, 2, function(x)cumsum(x)/(1:plays))
  ### Plot helper ###
  plot.result = function(x, n.series, colors, leg.names, ...){
    for (i in 1:n.series){
      if (i == 1)
        plot.ts(x[, i], ylim = 2*range(x), col = colors[i], ...)
      else
        lines(x[, i], col = colors[i], ...)
      grid(col = 'lightgray')
    }
    legend('topleft', leg.names, col = colors, lwd = 2, cex = 0.6, box.lwd = NA)
  }
  ### Plot helper ###
  #### Plots ####
  require(RColorBrewer)
  colors = brewer.pal(n.players, 'Set2')
  op <-par(mfrow = c(2, 1), mar=c(2,3,1,1), no.readonly = TRUE)
  plot.result(rewards.hist, n.players, colors, col.names, xlab = 'Plays', ylab = 'Average reward', lwd = 2)
  plot.result(optim.hist, n.players, colors, col.names, xlab = 'Plays', ylab = 'Optimal move %', lwd = 2)
  #### Plots ####
  par(op)
}
    
do.simulation(N = 500, plays = 1000, eps = c(0, 0.01, 0.1))
