# Description : To R from spreadsheets. Basic R
# Website : http://www.burns-stat.com/first-step-towards-r-spreadsheets/

superbowl <- read.table("http://www.portfolioprobe.com/R/blog/dowjones_super.csv", sep=",", header=TRUE)
plot(DowJonesSimpleReturn ~ Winner, data=superbowl)
plot(100 * DowJonesSimpleReturn ~ Winner, data=superbowl, col="powderblue", ylab="Return (%)")

tail(airquality)
Ct <- with(airquality, (Temp - 32) / 1.8)
newAir <- within(airquality, Ctemp <- (Temp - 32) / 1.8)
head(newAir)
