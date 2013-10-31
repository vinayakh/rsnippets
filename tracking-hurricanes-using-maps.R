# Description : Tracking hurricanes using maps
# Website : http://statistical-research.com/tracking-the-hurricanes/

doInstall <- TRUE
toInstall <- c("maps", "mapstools", "rgdal","OpenStreetMap","raster")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

year = 2013
hurricanes = read.table(file=paste("http://statistical-research.com/wp-content/uploads/2013/10/hurricanes.txt",sep=""), sep=",", fill=TRUE, header=T)
hurricanes = as.vector( subset(hurricanes, hurricanes$YEAR==year, select=c("NAME")) )

hurr.dat = list()
max.lon = max.lat = min.lat = min.lon = NULL
b.press = NULL

for(i in 1:nrow(hurricanes)){
  raw = read.table(file=paste("http://weather.unisys.com/hurricane/atlantic/",year,"/",hurricanes[i,],"/track.dat",sep=""), skip=2,fill=TRUE)
  colnames(raw) = c("Latitude","Longitude","Time","WindSpeed","Pressure","Status")
  raw$Pressure = as.character(raw$Pressure)
  raw$Pressure[raw$Pressure=="-"] = NA
  raw$Pressure = as.numeric(raw$Pressure)
  
  hurr.dat[[i]] = cbind(raw$Latitude, raw$Longitude, raw$Pressure)
  b.press = c(b.press, min(raw$Pressure, na.rm=T))
  
  if(is.null(max.lat)){
    max.lat = max(raw$Latitude)
  } else if(max.lat < max(raw$Latitude)) {
    max.lat = max(raw$Latitude)
  }
  if(is.null(min.lat)){
    min.lat = min(raw$Latitude)
  } else if (min.lat > min(raw$Latitude)){
    min.lat = min(raw$Latitude)
  }
  if(is.null(max.lon)){
    max.lon = max(raw$Longitude)
  } else if (max.lon < max(raw$Longitude)){
    max.lon = max(raw$Longitude)
  }
  if(is.null(min.lon)){
    min.lon = min(raw$Longitude)
  } else if (min.lon > min(raw$Longitude)){
    min.lon = min(raw$Longitude)
  }
  
}
xlim <- c(min.lon-5,max.lon+10)
ylim <- c(min.lat-5,max.lat+10)
state.list <- c('new york','new jersey','virginia','massachusetts','connecticut','delaware','pennsylvania','maryland','north carolina','south carolina','georgia','florida',
                'new hampshire','maine','district of columbia','west virginia','vermont')
my.map <- map("state", region=state.list, interior = FALSE, xlim=xlim, ylim=ylim)
map("state", region=state.list, boundary = TRUE, col="gray", add = TRUE,xlim=xlim)
map("world", boundary = TRUE, col="gray", add = TRUE,xlim=xlim)

for(j in 1:nrow(hurricanes)){
  lines(x=hurr.dat[[j]][,2],y=hurr.dat[[j]][,1],col=j,cex=0.75)
  points(x=hurr.dat[[j]][,2],y=hurr.dat[[j]][,1],pch=15,cex=0.4, col=j)
  text(hurr.dat[[j]][1,2],
       hurr.dat[[j]][1,1],hurricanes[j,])
}

title("Path of 2013 Hurricane Season")

box()
hist(b.press, xlim=c(920,1020), main="Histogram of Barometric Pressure for 2013",
     xlab="Barometric Pressure (mb)", ylab="Frequency")
abline(v=940, col='blue',lwd=3)
text(958,.5,"<&amp;lt;2012 Hurricane Sandy")