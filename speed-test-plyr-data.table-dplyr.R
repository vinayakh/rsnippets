# Description : Speed Comparison of plyr, data.table, dplyr
# Website : http://www.r-statistics.com/2013/09/a-speed-test-comparison-of-plyr-data-table-and-dplyr/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("plyr","data.table")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

set.seed(42)
types <- c("A", "B", "C", "D", "E", "F")
obs <- 4e+07
one <- data.frame(id = as.factor(seq(from = 1, to = 80000, by = 1)), 
                  percent = round(runif(obs, min = 0, max = 1), digits = 2), 
                  type = as.factor(sample(types, obs, replace = TRUE)))
print(object.size(one), units = "GB")
summary(one)

library(plyr)

## Test 1 (plyr): Use ddply and subset one with [ ] style indexing from
## within the ddply call.
typeSubset <- c("A", "C", "E")
system.time(test1 <- ddply(one[one$type %in% typeSubset, ], .(id), summarise, 
                           percent_total = sum(percent)))

## Test 2 (plyr):, Use ddply but subset one outside of the ddply call
two <- subset(one, type %in% typeSubset)
system.time(test2 <- ddply(two, .(id), summarise, percent_total = sum(percent)))

## Test 3 (plyr): For a simple sum, an alternative is to use plyr's count
## function
system.time(test3 <- count(two, "id", "percent"))

library(data.table)
## Test 4 (data.table): Speed test for package data.table
## Define the data table
three <- data.table(two, key = c("id"))
tables()  # check that the key columns are correct
## Operate on it
system.time(test4 <- three[, list(percent_total = sum(percent)), by = key(three)])

devtools::install_github("assertthat")
devtools::install_github("dplyr")
library(dplyr)

## Test 5 (dplyr): Speed test for package dplyr
fourDf <- group_by(two, id)
system.time(test5 <- summarise(fourDf, percent_total = sum(percent)))

sessionInfo()