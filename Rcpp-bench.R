# Description : demo of using Rcpp and speed comparisons
# Website : http://blog.revolutionanalytics.com/2013/07/deepen-your-r-experience-with-rcpp.html

# Script to compare C++ and R
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("Rcpp")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE

# C++ Function in Rcpp wrapper
cppFunction('
            double wmean(NumericVector x, NumericVector w) {
            int n = x.size();
            double total = 0, total_w = 0;
            for(int i = 0; i < n; ++i) {
            total += x[i] * w[i];
            total_w += w[i];
            }
            return total / total_w;
            }
            ')

# Naive R function
wmeanR <- function(x, w) {
  total <- 0
  total_w <- 0
  for (i in seq_along(x)) {
    total <- total + x[i] * w[i]
    total_w <- total_w + w[i]
  }
  total / total_w
}

x <- rnorm(10000000)
w <- rnorm(10000000)

system.time(wmean(x,w))

system.time(wmeanR(x,w))

# The proper way to compute a simple weighted mean in R
# using a built in function from the base stats package
system.time(weighted.mean(x,w))

## Benchmark Stats
## Run on Asus EEEcp
# > system.time(wmean(x,w))
# user  system elapsed 
# 0.076   0.000   0.077 
# > system.time(wmeanR(x,w))
# user  system elapsed 
# 165.112   1.168 167.311 
# > system.time(weighted.mean(x,w))
# user  system elapsed 
# 2.160   0.340   2.516 
