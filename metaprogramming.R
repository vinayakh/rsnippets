# Description : Metaprogramming in R
# Website : http://memosisland.blogspot.com/2013/09/metaprogramming-in-r-with-example.html

myFun <- function(vec) {
  numElements <- length(which(vec > threshold))
  numElements
}

genMyFuns <- function(thresholds) {
  ll <- length(thresholds)
  print("Generating functions:")
  for(i in 1:ll) {
    fName <- paste("myFun.", i, sep="")
    print(fName)
    assign(fName, eval(
      substitute(
        function(vec) {
          numElements <- length(which(vec > tt));
          numElements;
        }, 
        list(tt=thresholds[i])
      )
    ),
           envir=parent.frame()
    )
  }
}

genMyFuns(c(7, 9, 10))
myFun.1(1:20)
myFun.2(1:20)
myFun.3(1:20)