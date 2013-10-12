# Description : Checking sequences of results for randomness
# Website : http://statistical-research.com/random-sequence-of-heads-and-tails-for-r-users/

flips = matrix(c('H','T','T','H','H','H','T','T','T','T','T','T','T','H','H','H','T','H','T','H','H','H','T','H','H','H','T','H','T','H'))

RunsTest = function(flip.seq){
  u = unique(flip.seq) # unique value (should be two)
  
  d = rep(-1, nrow(flip.seq)*ncol(flip.seq)) # recode as vector of -1, +1
  d[flip.seq==u[1]] = 1
  
  n = sum(d > 0) # count +1's
  m = sum(d < 0) # count -1's
  
  dif = c(ifelse(d[1] < 0, 2, -2), diff( sign(d) )) # take the lag and find differences
  
  R = sum(dif==2 | dif==-2) # count up the number of runs
  
  ww.mu = 2*n*m / (n+m) + 1 # get the mean
  ww.var = (ww.mu-1)*(ww.mu-2)/(n+m-1) # get the variance
  sigma = sqrt(ww.var) # standard deviation
  
  # compute test statistics
  if((n+m) > 50){
    Z  = (R-ww.mu) / sigma
  } else if ((R-ww.mu) < 0){
    Z = (R-ww.mu+0.5) / sigma
  } else {
    Z = (R-ww.mu-0.5)/sigma
  }
  
  pval = 2*(1-pnorm(abs(Z))) # compute a two-sided p-value 
  ret.val = list(Z=Z, p.value=pval)
  return(ret.val)
}

runs.test = RunsTest(flips)
runs.test