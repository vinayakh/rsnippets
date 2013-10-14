# Description : Rolling means and other functions from Zoo
# Website : http://is-r.tumblr.com/post/37024322796/rolling-means-and-other-functions-with-zoo

doInstall <- TRUE
toInstall <- c("zoo")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

ftseIndex <- EuStockMarkets[, 4]
plot(ftseIndex, col = "GRAY")

# Calculate 10-day rolling mean, quickly:
smoothIndex <- rollmean(x = ftseIndex, # original series
                        k = 30, # width of the rolling window
                        fill = NA) # Pads head and/or tail with NA
length(ftseIndex) == length(smoothIndex)
lines(smoothIndex, col = "RED")

# If there are NA values in the original series, you'll need rollapply()
ftseIndex[c(40, 90, 300)] <- NA
smoothIndex2 <- rollapply(data = ftseIndex, # original series
                          width = 90, # width of the rolling window
                          FUN = mean, na.rm = T, # Any arbitrary function
                          fill = NA) # Padding
lines(smoothIndex2, col = "GREEN")