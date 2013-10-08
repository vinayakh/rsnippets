# Description : Measuring the Gerrymandering using Spatstat package
# Website : http://is-r.tumblr.com/post/38619550409/measuring-the-gerrymander-with-spatstat

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("maptools", "rgdal", "ggplot2", "spatstat", "RColorBrewer","mapproj")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Taking an online compressed shapefile, and opening it in R
# From http://stackoverflow.com/a/3053883
# Downloaded the file to a local dir if you want the script to download,
# uncomment the code below
# temp <- tempfile() # 110th & 111th Congressional District Shapefiles
# download.file("http://www.census.gov/geo/cob/bdy/cd/cd110shp/cd99_110_shp.zip",
#              temp) # See http://www.census.gov/geo/www/cob/cd110.html#shp
temp <- "~/github/rsnippets/data/cd99_110_shp.zip"

unzip(temp) # Will put the unzipped files in the working directory
shapeFile <- readShapeSpatial("cd99_110.shp") # Load shapefile

mapObject <- fortify(shapeFile) # Convert to a data.frame
mapObject <- data.frame(mapObject, shapeFile@data[mapObject$id, ])
head(mapObject) # ^ Add some labels

# Reduce to just the Continental U.S.
mapObject <- mapObject[mapObject$long < -50, ]
mapObject <- mapObject[mapObject$long > -135, ]
mapObject <- mapObject[mapObject$lat < 50, ]
mapObject <- mapObject[mapObject$lat > 22, ]

# Basic map of congressional districts in the Continental U.S.
with(mapObject, plot(long, lat, pch = ".", asp = 1))

### Calculate district area and perimeter length ###

mapObject$piece <- as.character(mapObject$piece)
mapObject$stateCD <- with(mapObject, paste(STATE, CD))
mapObject$Area <- mapObject$Perimeter <- NA
uniqueCDs <- sort(unique(mapObject$stateCD))

for(cd in uniqueCDs){ # This loop will take some time
  print(cd)
  cdShape <- mapObject[mapObject$stateCD == cd, ]
  cdPoly <- SpatialPolygons(list(Polygons(lapply(split(cdShape[, c("long", "lat")],
                                                       cdShape$piece), Polygon), ID = "b")))
  owinObject <- try(as(cdPoly, "owin")) # owin is very finicky, and I won't try
  if(class(owinObject) == "try-error"){next()} # to troubleshoot polygons here.
  plot(owinObject)
  Sys.sleep(1e-10)
  mapObject[mapObject$stateCD == cd, "Area"] <- area.owin(owinObject)
  mapObject[mapObject$stateCD == cd, "Perimeter"] <- perimeter(owinObject)
}

# A simple ratio measure of district compactness:
mapObject$Compactness <- with(mapObject, Area / Perimeter)
plot(density(unique(mapObject$Compactness), na.rm = T))

### Plot ###
new_theme_empty <- theme_bw() # Create our own, mostly blank, theme
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines",
                                         valid.unit = 3L, class = "unit")
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

mapPlot <- ggplot(mapObject)
mapPlot <- mapPlot + geom_polygon(aes(x = long, y = lat, group = group,
                                      fill = log(Compactness)),
                                  colour = "white", lwd = 1/9)
mapPlot <- mapPlot + coord_map(project="conic", lat0 = 30)
mapPlot <- mapPlot + new_theme_empty
mapPlot <- mapPlot + ggtitle("A measure of district compactness")
mapPlot <- mapPlot + scale_fill_gradientn(colours = myPalette(100),
                                          na.value = "gray80") # I just can't quit the Spectral palette!
#print(mapPlot)
ggsave("District compactness map.png", mapPlot, h = 9, w = 16, type = "cairo-png")