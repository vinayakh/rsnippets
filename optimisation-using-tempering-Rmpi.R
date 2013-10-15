# Descripton : Stochastic Optimization in R by Parallel Tempering
# Website : http://www.lindonslog.com/programming/stochastic-optimization-r-rmpi-parallel-tempering/

# Installation instructions:
# You need to install lam4-dev and lam-runtime packages to make this work
# Does not work with other mpi packages such as mpich2

doInstall <- TRUE
toInstall <- c("Rmpi")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

mpi.spawn.Rslaves(nslaves=6)
mpi.remote.exec(rm(list=ls()))

energy<-function(theta){
  E=(theta-40)^2
  return(E);
}

logdensity<-function(theta){
  #Distribution one wishes to sample from here.
  rate=20;
  ldensity=-rate*energy(theta);
  return(ldensity)
}




temper<-function(niter,Bmin,swap.interval){
  rank=mpi.comm.rank();
  size=mpi.comm.size();
  swap=0;
  swaps.attempted=0;
  swaps.accepted=0;
  
  #Higher ranks run the higher "temperatures" (~smaller fractional powers)
  B=rep(0,size-1);
  for(r in 1:size-1){
    temp=(r-1)/(size-2);
    B[r]=Bmin^temp;
  }
  
  
  #Create a vector for proposal moves
  theta=rep(0,niter)
  
  for(t in 2:niter){
    
    prop=theta[t-1]+rnorm(1,0,0.1);
    
    #Calculate Log-Density at proposed and current position
    logdensity.current=logdensity(theta[t-1])
    logdensity.prop=logdensity(prop);
    
    #Calculate log acceptance probability
    lalpha=B[rank]*(logdensity.prop-logdensity.current)
    
    if(log(runif(1))<lalpha){
      #Accept proposed move
      theta[t]=prop;
      logdensity.current=logdensity.prop;
    }else{
      #Otherwise do not move
      theta[t]=theta[t-1];
    } 
    
    if(t%%swap.interval ==0){
      for(evenodd in 0:1){
        swap=0;
        logdensity.partner=0;
        #if rank even
        if(rank%%2 == evenodd%%2){
          rank.partner=rank + 1;
          #ranks range from 1:size-1. Cannot have a partner rank == size
          if(0<rank.partner && rank.partner<size){
            #On first iteration, evens receive from above odd
            #On second iteration, odds receive from above evens
            logdensity.partner<-mpi.recv.Robj(rank.partner,rank.partner);
            lalpha = (B[rank]-B[rank.partner])*(logdensity.partner-logdensity.current);
            swaps.attempted=swaps.attempted+1;
            if(log(runif(1))<lalpha){
              swap=1;
              swaps.accepted=swaps.accepted+1;
            }
            mpi.send.Robj(swap,dest=rank.partner,tag=rank)
          }
          if(swap==1){
            thetaswap=theta[t];
            mpi.send.Robj(thetaswap,dest=rank.partner,tag=rank)
            theta[t]=mpi.recv.Robj(rank.partner,rank.partner)
          }
        }else{
          rank.partner=rank-1;
          #ranks range from 1:size-1. Cannot have a partner rank ==0
          if(0<rank.partner && rank.partner<size){
            #On first iteration, odds send to evens below
            #On second iteration, evens sent to odds below
            mpi.send.Robj(logdensity.current,dest=rank.partner,tag=rank);
            swap=mpi.recv.Robj(rank.partner,rank.partner);
          }
          if(swap==1){
            thetaswap=theta[t];
            theta[t]=mpi.recv.Robj(rank.partner,rank.partner);
            mpi.send.Robj(thetaswap,dest=rank.partner,tag=rank);
          }
        }
      }
    }
  }
  return(theta)
}



#Send to slaves any initial variables that they require
niter=2000
Bmin=0.005
swap.interval=100
mpi.bcast.Robj2slave(niter)
mpi.bcast.Robj2slave(Bmin)
mpi.bcast.Robj2slave(swap.interval)
mpi.bcast.Robj2slave(energy)
mpi.bcast.Robj2slave(logdensity)
#Send to slaves and functions that they require
mpi.bcast.Robj2slave(temper)
#Check to see that the slaves have the correct function
mpi.remote.exec(temper)
#Run the tempering algorithm
mcmc=mpi.remote.exec(temper(niter,Bmin,swap.interval))

par(mfrow=c(3,2))
plot(mcmc[[1]],xlab=~theta)
plot(mcmc[[2]],xlab=~theta)
plot(mcmc[[3]],xlab=~theta)
plot(mcmc[[4]],xlab=~theta)
plot(mcmc[[5]],xlab=~theta)
plot(mcmc[[6]],xlab=~theta)

mpi.close.Rslaves()
mpi.quit()