# Description : Using GeoJSON to render maps on github
# Website : http://procomun.wordpress.com/2013/09/20/r-geojson-and-github/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("sp","rgdal")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(sp)
library(rgdal)

setwd(tempdir())
download.file('https://raw.github.com/oscarperpinan/solar/gh-pages/data/SIAR.csv', 'siar.csv', method='wget')
siar <- read.csv('siar.csv')
summary(siar)
siarSP <- SpatialPointsDataFrame(siar[,c(6, 7)], siar[,-c(6,7)])
writeOGR(siarSP, 'siar.geojson', 'siarSP', driver='GeoJSON')