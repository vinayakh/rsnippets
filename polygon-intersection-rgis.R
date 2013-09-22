# Description : Polygon Intersection with R GIS
# Website : http://thebiobucket.blogspot.in/2013/09/r-gis-polygon-intersection-with.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("rgeos","rgdal","dismo")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# code from http://thebiobucket.blogspot.co.at/2013/09/batch-downloading-zipped-shapefiles.html
url_shp_to_spdf <- function(URL) {
  
  require(rgdal)
  
  wd <- getwd()
  td <- tempdir()
  setwd(td)
  
  temp <- tempfile(fileext = ".zip")
  download.file(URL, temp)
  unzip(temp)
  
  shp <- dir(tempdir(), "*.shp$")
  lyr <- sub(".shp$", "", shp)
  y <- lapply(X = lyr, FUN = function(x) readOGR(dsn=shp, layer=lyr))
  names(y) <- lyr
  
  unlink(dir(td))
  setwd(wd)
  return(y)
}

library(rgeos)
library(dismo)

URLs <- c("http://gis.tirol.gv.at/ogd/umwelt/wasser/wis_gew_pl.zip",               # all water bodies in Tyrol
          "http://gis.tirol.gv.at/ogd/umwelt/wasser/wis_tseepeicher_pl.zip")       # only artificial..

y <- lapply(URLs, url_shp_to_spdf)
z <- unlist(unlist(y))
a <- getData('GADM', country = "AT", level = 2)

b <- a[a$NAME_2=="Innsbruck Land", ]                                               # political district's boundaries
c <- spTransform(b, z[[1]]@proj4string)                                            # (a ring polygon)   
z1_c <- gIntersection(z[[1]], c, byid = TRUE)                                     
z2_c <- gIntersection(z[[2]], c, byid = TRUE)

plot(c)
plot(z1_c, lwd = 5, border = "red", add = T)
plot(z2_c, lwd = 5, border = "green", add = T)
plot(z[[1]], border = "blue", add = T)              # I plot this on top, so it will be easier to identify
plot(z[[2]], border = "brown", add = T)
