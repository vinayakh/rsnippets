# Description : Using compiler package to speedup code
# Website : http://www.r-statistics.com/2012/04/speed-up-your-r-code-using-a-just-in-time-jit-compiler/

##### Functions #####

is.compile <- function(func)
{
  # this function lets us know if a function has been byte-coded or not
  #If you have a better idea for how to do this - please let me know...
  if(class(func) != "function") stop("You need to enter a function")
  last_2_lines <- tail(capture.output(func),2)
  any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
}

# old R version of lapply
slow_func <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  if (!is.list(X))
    X <- as.list(X)
  rval <- vector("list", length(X))
  for(i in seq(along = X))
    rval[i] <- list(FUN(X[[i]], ...))
  names(rval) <- names(X)          # keep `names' !
  return(rval)
}

# Compiled versions
require(compiler)
slow_func_compiled <- cmpfun(slow_func)

fo <- function() for (i in 1:1000) slow_func(1:100, is.null)
fo_c <- function() for (i in 1:1000) slow_func_compiled(1:100, is.null)

system.time(fo())
system.time(fo_c())

fo_compiled <- cmpfun(fo)
system.time(fo_compiled()) # doing this, will not change the speed at all:

is.compile(slow_func)
is.compile(fo)
is.compile(fo_compiled)

enableJIT(3)
system.time(fo())

is.compile(fo) 
is.compile(slow_func)

# Run this at the beginning to speedup code
# require(compiler)
# enableJIT(3)