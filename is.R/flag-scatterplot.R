# Description : Flag space: a scatter plot of raster images
# Website : http://is-r.tumblr.com/post/33700919594/flag-space-a-scatter-plot-of-raster-images

# Drawing a scatter plot of raster images
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("png", "source.gist", "MASS", "RCurl")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Some helper functions, lineFinder and makeTable
source.gist("818983")
source.gist("818986")

# In as few lines as possible, get URLs for .PNGs of each flag
importHTML <- readLines("http://en.wikipedia.org/wiki/World_flags")
importHTML[lineFinder("thumbborder", importHTML)]
pngURLs <- makeTable(makeTable(importHTML[lineFinder("thumbborder",
                                                     importHTML)], "src=\"//")[, 2], "\" width=\"")[, 1]
pngURLs <- paste0("http://", pngURLs)

# CAUTION: The following loop will download 204 .PNG images
# of flags from Wikipedia. Please be considerate, and don't run
# this part of the script any more than you need to.
pngList <- list()
for(ii in 1:length(pngURLs)){
  tempName <- paste("Flag", ii)
  tempPNG <- readPNG(getURLContent(pngURLs[ii])) # Downloads & loads PNGs
  pngList[[tempName]] <- tempPNG # And assigns them to a list.
}

# Very simple dimension reduction -- just the mean R, G, and B values
meanRGB <- t(sapply(pngList, function(ll){
  apply(ll[, , -4], 3, mean)
}))

# The dimensions of each item are equal to the pixel dimensions of the .PNG
flagDimensions <- t(sapply(pngList, function(ll){
  dim(ll)[1:2]
}))

# Similarity, through Kruskal non-metric MDS
flagDistance <- dist(meanRGB)
flagDistance[flagDistance <= 0] <- 1e-10

MDS <- isoMDS(flagDistance)$points
plot(MDS, col = rgb(meanRGB), pch = 20, cex = 2)

# Plot:
boxParameter <- 5000 #6000 # To alter dimensions of raster image bounding box
par(bg = gray(8/9))
plot(MDS, type = "n", asp = 1)
for(ii in 1:length(pngList)){ # Go through each flag
  tempName <- rownames(MDS)[ii]
  Coords <- MDS[tempName, 1:2] # Get coordinates from MDS
  Dims <- flagDimensions[tempName, ] # Get pixel dimensions
  rasterImage(pngList[[tempName]], # Plot each flag with these boundaries:
              Coords[1]-Dims[2]/boxParameter, Coords[2]-Dims[1]/boxParameter,
              Coords[1]+Dims[2]/boxParameter, Coords[2]+Dims[1]/boxParameter)
}