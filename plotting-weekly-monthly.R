# Description : Plottig weekly ad othly totals in R
# Website : http://www.mollietaylor.com/2013/08/plot-weekly-or-monthly-totals-in-r.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2","scales")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(ggplot2)
library(scales)

# load data:
log <- data.frame(Date = c("2013/05/25","2013/05/28","2013/05/31","2013/06/01","2013/06/02","2013/06/05","2013/06/07"),
                  Quantity = c(9,1,15,4,5,17,18))
log
str(log)

# convert date variable from factor to date format:
log$Date <- as.Date(log$Date,
                    "%Y/%m/%d") # tabulate all the options here
str(log)

# create variables of the week and month of each observation:
log$Month <- as.Date(cut(log$Date,
                         breaks = "month"))
log$Week <- as.Date(cut(log$Date,
                        breaks = "week",
                        start.on.monday = FALSE)) # changes weekly break point to Sunday
log

# graph by month:
ggplot(data = log,
       aes(Month, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") # custom x-axis labels

# graph by week:
ggplot(data = log,
       aes(Week, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = "1 week") # custom x-axis labels