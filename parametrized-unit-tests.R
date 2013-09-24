# Description : Parameterised unit tests (metaprogramming)
# Website : http://memosisland.blogspot.in/2013/09/a-technique-for-doing-parametrized-unit.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("rgdal","colorspace","maps","mapdata","maptools")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(quantmod)
library(RUnit)

vecSD <- function(vec, expectedSD) {
  sdComputed <- sd(vec)
  checkEquals(sdComputed, expectedSD, tol=1e-6)
}

vecSDmeta <- function(vecList, expectedSDvec) {
  nFunctions <- length(expectedSDvec)
  for(i in 1:nFunctions) {
    fName <- paste("vecSDunit", i, sep="")
    print(fName)
    assign(fName, eval(
      substitute(
        function() {
          sdComputed <- sd(vec)
          checkEquals(sdComputed, expectedSD, tol=1e-3)
        },
        list(vec=vecList[[i]], expectedSD=expectedSDvec[i])
      )
    ),
           envir=parent.frame()
    )
  }
}

data.env <- new.env()
getSymbols(c("MSFT", "GOOG", "AAPL"), src='yahoo', env=data.env,  from='2005-01-01', to='2012-12-31')
openGoog <- data.env$GOOG[1:dim(data.env$GOOG)[1], 1]
openMS   <- data.env$MSFT[1:dim(data.env$MSFT)[1], 1]
openAP   <- data.env$AAPL[1:dim(data.env$AAPL)[1], 1]
vecList  <- list(
  vGoog = as.vector(openGoog),
  vMS   = as.vector(openMS),
  vAP   = as.vector(openAP)
)
expectedSDvec <- c(126.5066, 3.391194, 169.4987) # this is expected

vecSDmeta(vecList, expectedSDvec)
vecSDunit1()
vecSDunit2()
vecSDunit3()