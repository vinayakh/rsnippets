# Description : Simulation of wait timings in queues
# Website : http://statistical-research.com/waiting-in-one-line-or-multiple-lines/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("reshape2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(reshape2) ## For use in the acast() function
service.locations = 3 ## i.e. Number of cash registers
total.obs = service.locations*10 ## Just needed a number and wanted to make is divisible by the service locations
nsims = 10000 ## Number of simulation replicates
x = replicate(nsims, rexp(total.obs, 1/3))

## Set some loading variables

cum.sum = aggreg.multi.mat = aggreg.one.mat = NULL

for(j in 1:ncol(x)){
  ## Multiple lines assigned randomly (sequentially)
  wait.multiple = cbind( (seq(1:total.obs) %% service.locations)+1, x[,j] )
  aggreg.values = aggregate(wait.multiple[,2], by=list( wait.multiple[,1] ), sum)
  aggreg.multi.mat = rbind(aggreg.multi.mat, t( acast(aggreg.values, Group.1~., value.var="x") ) )
  
  ## One line and breaking off at the very end
  wait.bucket = matrix(NA, ncol=service.locations,nrow(x))
  queue = x[,j]
  
  ## Preload the first service locations
  for(d in (1:service.locations)){
    wait.bucket[d,d] = x[d,j]
  }
  
  ## Cumulative sum without NA then find min location as it will serve the next customer
  for(i in service.locations+1:(nrow(x)-service.locations) ){
    for(k in 1:service.locations){
      cum.sum[k] = sum(wait.bucket[1:i,k], na.rm=T)
      
    }
    wait.bucket[i,which(cum.sum==min(cum.sum), arr.ind=TRUE)] = x[i,j]
  }
  aggreg.one.mat = rbind(aggreg.one.mat, apply(wait.bucket, 2, sum, na.rm=T))
  
}

## View the graphs
my.hist.one.1 = hist( apply(aggreg.one.mat,1,max), nclass=100, plot=FALSE)
my.hist.one.2 = hist( apply(aggreg.one.mat,1,min), nclass=100, plot=FALSE)
max.counts = max(my.hist.one.1$counts, my.hist.one.2$counts)
par(mfrow=c(2,1))
hist( apply(aggreg.one.mat,1,max), nclass=100, xlim=c(0,60), co=3, main=expression(paste("Single-Line -- Distribution of Max Wait Time With EXP(",theta,"=3)")), xlab="Total Minutes Per Register")
hist( apply(aggreg.one.mat,1,min), nclass=100, xlim=c(0,60), col=2, main=expression(paste("Single-Line -- Distribution of Min Wait Time With EXP(",theta,"=3)")), xlab="Total Minutes Per Register")

my.hist.data.1 = hist( apply(aggreg.multi.mat,1,max), nclass=100, plot=FALSE)
my.hist.data.2 = hist( apply(aggreg.multi.mat,1,min), nclass=100, plot=FALSE)
max.counts = max(my.hist.data.1$counts, my.hist.data.2$counts)
par(mfrow=c(2,1))
hist( apply(aggreg.multi.mat,1,max), nclass=100, xlim=c(0,60), ylim=c(0,max.counts), co=3, main=expression(paste("Multiple Lines -- Distribution of Max Wait Time With EXP(",theta,"=3) ")), xlab="Total Minutes Per Register")
hist( apply(aggreg.multi.mat,1,min), nclass=100, xlim=c(0,60), col=2, main=expression(paste("Multiple Lines -- Distribution of Min Wait Time With EXP(",theta,"=3) ")), xlab="Total Minutes Per Register")

par(mfrow=c(2,1))
hist( apply(aggreg.one.mat,1,max)-apply(aggreg.one.mat,1,min), nclass=100, xlim=c(0,60), col=2, main="Single Line", xlab="Range Difference Between Max and Min in Minutes of Service Locations")
hist( apply(aggreg.multi.mat,1,max)-apply(aggreg.multi.mat,1,min), nclass=100, xlim=c(0,60), col=3, main="Multiple Lines", xlab="Range Difference Between Max and Min in Minutes of Service Locations")