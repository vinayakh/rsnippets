# Description : Cumulative windfield maxima from Hurricane Katrina
# Website : http://rpubs.com/ricobert1/7372

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("rgdal","colorspace","maps","mapdata","maptools")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(rgdal)
library(colorspace)
library(maps)
library(mapdata)
library(maptools)

urls = c("ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0825/2100/AL122005_0825_2100shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/0000/AL122005_0826_0000shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/0300/AL122005_0826_0300shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/0600/AL122005_0826_0600shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/0900/AL122005_0826_0900shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/1200/AL122005_0826_1200shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/1500/AL122005_0826_1500shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/1800/AL122005_0826_1800shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0826/2100/AL122005_0826_2100shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/0000/AL122005_0827_0000shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/0300/AL122005_0827_0300shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/0600/AL122005_0827_0600shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/0900/AL122005_0827_0900shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/1200/AL122005_0827_1200shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/1500/AL122005_0827_1500shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/1800/AL122005_0827_1800shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0827/2100/AL122005_0827_2100shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/0000/AL122005_0828_0000shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/0300/AL122005_0828_0300shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/0600/AL122005_0828_0600shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/0900/AL122005_0828_0900shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/1200/AL122005_0828_1200shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/1500/AL122005_0828_1500shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/1800/AL122005_0828_1800shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0828/2100/AL122005_0828_2100shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/0000/AL122005_0829_0000shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/0300/AL122005_0829_0300shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/0600/AL122005_0829_0600shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/0900/AL122005_0829_0900shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/1200/AL122005_0829_1200shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/1500/AL122005_0829_1500shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/1800/AL122005_0829_1800shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0829/2100/AL122005_0829_2100shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0830/0000/AL122005_0830_0000shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0830/0300/AL122005_0830_0300shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0830/0600/AL122005_0830_0600shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0830/0900/AL122005_0830_0900shape.tar.gz", 
         "ftp://ftp.aoml.noaa.gov/hrd/pub/hwind/PostAnalysis/2005/AL122005/0830/1200/AL122005_0830_1200shape.tar.gz")

for (i in 1:length(urls)) {
  file = basename(urls)[i]
  download.file(urls[i], paste(getwd(), "/", file, sep = ""), quiet = T)
  untar(file)
}

files = list.files(pattern = "*.shp$", recursive = TRUE, full.names = TRUE)
spdf.data = readOGR(dsn = getwd(), layer = gsub("^.*/(.*).shp$", "\\1", files[1]))

columns.to.keep <- c("ID", "LONGITUDE", "LATITUDE", "SFC_SPD", "SFC_DIR")
spdf.data <- spdf.data[columns.to.keep]

for (i in 2:length(files)) {
temp.data <- readOGR(dsn = getwd(), layer = gsub("^.*/(.*).shp$", "\\1", 
files[i]), verbose = F)
temp.data <- temp.data[columns.to.keep]
spdf.data <- spRbind(spdf.data, temp.data)
}

hpt = spsample(spdf.data, type = "hexagonal", n = 20000)
hpg = HexPoints2SpatialPolygons(hpt)
Wind.hexid = overlay(spdf.data, hpg)
Wind.split = split(spdf.data@data, Wind.hexid)
names(Wind.split) = sapply(hpg@polygons, function(x) x@ID)[as.numeric(names(Wind.split))]
Wind.max = sapply(Wind.split, function(x) max(x$SFC_SPD))
Wind.max = data.frame(Wind.max)
Wind.spdf = SpatialPolygonsDataFrame(hpg[rownames(Wind.max)], Wind.max)

box = bbox(Wind.spdf)
cl = map("worldHires", xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), plot = FALSE)
clp1 = map2SpatialLines(cl)  #, proj4string=CRS(ll))
cl = map("state", xlim = c(box[1], box[3]), ylim = c(box[2], box[4]), interior = T, 
boundary = F, plot = FALSE)
clp2 = map2SpatialLines(cl)  #, proj4string=CRS(ll))

spplot(Wind.spdf, "Wind.max", col = "transparent", at = c(17, 33, 43, 50, 58, 
70, 100), lwd = 0.1, col.regions = c("darkslategray1", rev(heat_hcl(5))), 
colorkey = list(space = "bottom", labels = list(at = c(17, 33, 43, 50, 58, 
70, 100)), cex = 0.9), xlim = c(box[1] * 0.97, box[3]), ylim = c(box[2], 
box[4] * 0.8), sub = list(label = expression(paste("Maximum observed wind speed [m", 
s^-1, "]")), cex = 1), panel = function(x, y, ...) {
panel.polygonsplot(x, y, ...)
sp.lines(clp1, col = "black", lwd = 0.2)
sp.lines(clp2, col = "black", lwd = 0.3)
})