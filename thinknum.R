# Description : Plotting public datsets using Thinknum
# Website : http://www.econometricsbysimulation.com/2013/09/thinknum-new-interactive-public.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("Thinknum","ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library("Thinknum")
goog <- Thinknum("goog")
plot(goog, type="l")



# In order to pass arguments to R in the same manner as the ThinkNum website I have written a small function which can read multiple calls to Thinknum and returns them as either a wide or tall data.frame

mThinknum <- function(command, tall=F) {
  
  require("Thinknum")
  
  # Break the command into seperate calls
  think.list <- strsplit(command, ";")[[1]]
  
  # Look through each element of the think.list
  for (i in 1:length(think.list)) {
    
    cat(paste("Reading:",think.list[i])) # Display feedback
    
    if (!tall) {
      if (i==1) returner <- Thinknum(think.list[1])
      if (i>1)  returner <- merge(returner,Thinknum(think.list[i])
                                  , by.x="date_time", by.y="date_time")
    }
    if (tall) {
      tempdat <- Thinknum(think.list[i])
      names(tempdat) <- c("date_time", "value")
      
      if (i==1) returner <- data.frame(tempdat, call=think.list[i])
      if (i>1) returner <- rbind(returner, data.frame(tempdat, call=think.list[i]))
      names(returner) <- c("date_time", "value", "call")
    }
    
    cat(paste(rep(" ", max(1,20-nchar(think.list[i]))), collapse=""))
    # Insert spaces
    cat(paste("Dimensions:", paste(dim(returner), collapse="x"), "\n"))
    # Show dimensions
  }
  
  # Ensure the return file has appropriate column names
  if (!tall) names(returner) <- c("date_time", think.list)
  
  data.frame(returner)
}

test <- mThinknum("^spx;sma(^spx,30);sma(goog*2,30);(goog*2)")
head(test)

# Let's try plotting with the base package

plot(x=c(min(test$date_time), max(test$date_time)),
     y=c(min(test[,2:5]),max(test[,2:5])), type="n",
     main="Plot in R")

lines(test$date_time, test[,2], type="l")
lines(test$date_time, test[,3], type="l", col="red")
lines(test$date_time, test[,4], type="l", col="blue")
lines(test$date_time, test[,5], type="l", col="darkgreen")

# Let's do the same but now let's use ggplot2

# In order to do this let's use the tall option for mThinknum
test2 <- mThinknum("^spx;sma(^spx,180);sma(goog*2,180);(goog*2)", tall=T)

# I have extended the running average to be 180 days rather than 30 because the scale is so large.
head(test2)
library("ggplot2")

ggplot(data=test2[test2$date_time>as.Date("2004-01-01"),],
       aes(x=date_time, y=value, group=call)) +
  geom_line(aes(colour = call))