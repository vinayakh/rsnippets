# Description : Individual-based models in R
# Website : http://menugget.blogspot.in/2013/10/a-first-attempt-at-individual-based.html

b <- 0.14 # probability of birth
d <- 0.08 # probability of death
K <- 100 # carrying capacity
N0 <- 50 # starting number of individuals
t <- 500 # time of simulation

#create starting individual w attributes ("alive", "age", "color")
set.seed(1)
ind <- vector(mode="list", N0)
for(i in seq(ind)){
  ind[[i]]$alive <- 1
  ind[[i]]$age <- 0
  ind[[i]]$color <- c("blue", "red")[round(runif(1)+1)]
}

#make empty vectors to record population statistics
time <- seq(t+1)

pop <- NaN * time # population size
pop[1] <- N0

frac.blue <- NaN * time # fraction of population that is blue
cols <- sapply(ind, function(x) x$color)
frac.blue[1] <- sum(cols  == "blue") / length(cols)

med.age <- NaN * time
ages <- sapply(ind, function(x) x$age)
med.age[1] <- median(ages)


#simulation
save.alive.only <- TRUE # optional cropping of "ind" to include alive individuals only 
t1 <- Sys.time()
for(i in seq(t)){ # loop for each time increment
  
  is.alive <- which(sapply(ind, function(x) x$alive) == 1)
  for(j in is.alive){ #loop for each alive individual
    birth <- runif(1) <= (b * (1 - length(is.alive)/K)) # calculate a birth probability for each individual that is alive
    if(birth){
      len.ind <- length(ind)
      ind[[len.ind+1]] <- list(alive=1, age=0, color=ind[[j]]$color) # create offspring, inherits color of parent
    }
    death <- runif(1) <= d # calculate a death probability for each individual 
    if(death){
      ind[[j]]$alive <- 0 # if death, reset alive = 0
    } else { #else, advance age + 1
      ind[[j]]$age <- ind[[j]]$age + 1 # advance age of parent
    }
  }
  
  #optional cropping of list "ind"
  if(save.alive.only){
    is.dead <- which(sapply(ind, function(x) x$alive) == 0)
    if(length(is.dead) > 0) ind <- ind[-is.dead]
  }
  
  #Population stats
  is.alive <- which(sapply(ind, function(x) x$alive) == 1)
  pop[i+1] <- length(is.alive) 
  
  cols <- sapply(ind, function(x) x$color)
  frac.blue[i+1] <- sum(cols[is.alive]  == "blue") / length(is.alive)
  
  ages <- sapply(ind, function(x) x$age)
  med.age[i+1] <- median(ages[is.alive])
  
  print(paste(i, "of", t, "finished", "[", round(1/t*100), "%]"))
}
t2 <- Sys.time()
dt <- t2-t1
dt


#plot populations
png("pops_vs_time.png", width=6, height=4, units="in", res=400)
par(mar=c(4,4,1,1))
pop.blue <- pop * frac.blue
pop.red <- pop * (1-frac.blue)
ylim=range(c(pop.blue, pop.red))
plot(time, pop.blue, t="l", lwd=2, col=4, ylim=ylim, ylab="Population size")
lines(time, pop.red, lwd=2, col=2)
legend("topleft", legend=c("blue pop.", "red pop."), lwd=2, col=c(4,2), bty="n")
dev.off()

#plot median age
png("med_age_vs_time.png", width=6, height=4, units="in", res=400)
par(mar=c(4,4,1,1))
plot(time, med.age, t="l", lwd=2, ylab="Median age")
dev.off()