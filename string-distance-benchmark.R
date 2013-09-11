# Description : Benchmark and comparison of different implementation of string comparison algorithms
# Website : http://www.markvanderloo.eu/yaRb/2013/09/07/a-bit-of-benchmarking-with-string-distances/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("rbenchmark", "RecordLinkage", "stringdist")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(rbenchmark)
library(RecordLinkage)
library(stringdist)

# Naive simple benchmark with Lv algo
x <- sapply(sample(5:25,1e5,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))
y <- sapply(sample(5:25,1e5,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))

benchmark(d1 <- stringdist(x,y,method='lv') , d2 <- levenshteinDist(x,y), replications=10)

# Naive benchmark with longer strings
x <- sapply(sample(25:250,1e5,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))
y <- sapply(sample(25:250,1e5,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))

benchmark(d1 <- stringdist(x,y,method='lv'), d2 <- levenshteinDist(x,y), replications=10)

# Benchmark with the usebytes option
x <- sapply(sample(5:25,1e5,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))
y <- sapply(sample(5:25,1e5,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))

benchmark(d1 <- stringdist(x,y,method='lv'), d2 <- stringdist(x,y,method='lv',useBytes=TRUE), d3 <- levenshteinDist(x,y), replications=10)

# Benchmark with the Jw algo
benchmark(d1 <- stringdist(x, y, method = "jw"), d2 <- stringdist(x, y, method = "jw", useBytes = TRUE), d3 <- jarowinkler(x, y), replications=10)

x <- sapply(sample(50:250,1e4,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))
y <- sapply(sample(50:250,1e4,replace=TRUE), function(n) paste(sample(letters,n,replace=TRUE),collapse=""))

# Benchmark on even longer strings
benchmark(d1 <- stringdist(x,y,method='lv'), d2 <- stringdist(x,y,method='lv',useBytes=TRUE), d3 <- levenshteinDist(x,y), replications=10)

# Effects on encoding on the two functions
# the function from RecordLinkage
levenshteinDist("ü","u")

# stringdist
stringdist("ü","u",method='lv')
stringdist("ü","u",method='lv',useBytes=TRUE)