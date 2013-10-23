# Description : Beta Distributions and elections
# Website : http://statistical-research.com/beta-distribution-and-the-nj-u-s-senate-election/

doInstall <- TRUE
toInstall <- c("MCMCpack")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

## Set up several of the recent polls but will only work with the most recent on
raw.1 = NULL
raw.1 = data.frame( rbind(
  Quinnipiac = c(.53,.41,899),
  RSC = c(.50,.39,729),
  FD= c(.45,.29,702),
  Mon = c(.53, .40,571)
)
)
raw.1 = rbind(raw.1, c(apply(raw.1[,1:2],2,weighted.mean,raw.1[,3]),sum(raw.1[,3])))
names(raw.1) = c("Cand1","Cand2","size")
raw.1$Other.und = 1-raw.1$Cand1-raw.1$Cand2
raw.1.no.und = data.frame(raw.1[5,1:2] + raw.1[5,1:2]/sum(raw.1[5,1:2])*raw.1[5,4],size=raw.1[5,3],Other.und=0)
raw = rbind(raw.1, raw.1.no.und)
###################################################################
## More than two candidates so Beta distribution won't work
## Function to randomly generate data from a dirichlet distribution
###################################################################
j= 4
prob.win = function(j,export=1){
  p=rdirichlet(100000,
               raw$size[j] *
                 c(raw$Cand1[j], raw$Cand2[j], 1-raw$Cand1[j]-raw$Cand2[j])+1
  )
  if(export==1){
    mean(p[,1]>p[,2])
  } else {
    return(p)
  }
}

( cand1.win.probs = sapply(1:nrow(raw),prob.win) )

sim.dir = prob.win(4,export=2) ## set simulated data for plotting and determining parameters
sim.dir.diff = sim.dir[,1]-sim.dir[,2] ## Get the marginal. From a Dirichlet the is distributed as a Beta.
sim.dir = cbind(sim.dir, sim.dir[,1]-sim.dir[,2])
## The shape parameters (shape1 and shape2) might need some manual adjusting and tweaking.
## In this case I ran the function a few time to set the start value close to the output
fit.distr.1 = fitdistr(sim.dir[,1], "beta",
                       start=list(shape1=302,shape2=270))
fit.distr.2 = fitdistr(sim.dir[,2], "beta",
                       start=list(shape1=229,shape2=343))
fit.distr.margin = fitdistr(sim.dir[,4], "beta",
                            start=list(shape1=5,shape2=5))
## Could also draw a histogram of simulated data
curve(dbeta(x,fit.distr.1$estimate[1],fit.distr.1$estimate[2]),
      ylim=c(0,20), xlim=c(.3,.6), col='blue', lty=1, lwd=2, ylab="Density", xlab="theta",
      main="Distribution of the NJ U.S. Senate Election 2013",
      sub=paste("Probability that Booker beats Lonegan: ", round(cand1.win.probs[6],2) ) ) ## Candidate 1
curve(dbeta(x,fit.distr.2$estimate[1],fit.distr.2$estimate[2]), add=T, col='red', lty=2, lwd=2) ## Candidate 2

abline(v=c(median(sim.dir[,1]), median(sim.dir[,2])), col=c('blue','red'), lwd=2, lty=c(1,2,3))
legend("topleft",c("Booker","Lonegan"), lwd=2, col=c('blue','red'), lty=c(1,2))
## Draw a histogram of simulated data
hist(sim.dir[,4], nclass=100, main="Histogram of the Candidate Differences", xlab="Candidate Difference")
abline(v=0, col=c('black'), lwd=2, lty=c(1))