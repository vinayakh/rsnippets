# Description : Running microbenchmarks and improving speed with complier package
# Website : http://wiekvoet.blogspot.com/2013/10/carry-over-balanced-designs-for-8.html

toInstall <- c("microbenchmark","compiler")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

nextpos <- function(desmat) which(desmat==0,arr.ind=TRUE)

gendesign <- function(n=6) {
  nr <- as.integer(n)
  nc <- nr
  
  desmat <- matrix(0L,nrow=nr,ncol=nc)
  desmat[1,] <- 1L:nc
  desmat[,1] <- 1L:nr
  
  carover <- matrix(TRUE,nrow=nr,ncol=nc)
  for (i in 1L:(nc-1L))  carover[i,i+1] <- FALSE
  todo <- nextpos(desmat)
  
  desobject <- list(desmat=desmat,carover = carover,nc=1L:n,n=n,
                    index =1L,npos=nrow(todo),
                    row=todo[,1L],
                    col=todo[,2L])
  desresult <- list()
  addpoint(desobject,desresult)
}

modify <- function(desobject,row,col,i,previous) {
  desobject$desmat[row,col] <- i
  desobject$carover[previous,i] <- FALSE
  desobject$index <- desobject$index + 1L
  desobject}

addpoint <- function(desobject,desresult) {
  if (desobject$index>desobject$npos) {
    l <- length(desresult)
    desresult[[l+1]] <- desobject$desmat
    #  cat('#')
    return(desresult)
  } 
  row <- desobject$row[desobject$index]
  col <- desobject$col[desobject$index]
  previous <- desobject$desmat[row,col-1L]
  avoid <- c(desobject$desmat[row,],
             desobject$desmat[,col])
  nc <- desobject$nc[!is.element(desobject$nc,avoid) ]
  nc <- nc[desobject$carover[previous,nc]]
  for (i in nc) {
    desresult <- addpoint(modify(desobject,row,col,i,previous)
                          ,desresult)
  }
  desresult
}

# From compiler library
enableJIT(3)

# Run microbenchmarks
microbenchmark(gendesign(7),times=10L)
microbenchmark(gendesign(3),gendesign(4),gendesign(5),gendesign(6))
system.time(gendesign(7))
system.time(gendesign(8))