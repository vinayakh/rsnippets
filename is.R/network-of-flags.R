# Description : Network of Flags
# Website : http://is-r.tumblr.com/post/37901909845/everything-is-a-network-featuring-the-sna-package

# Drawing a scatter plot of raster images
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("png", "sna")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Go to https://www.gosquared.com/resources/2400-flags, download the ZIP,
# and put the 64 x 64 files into a directory of your choosing.
# Then setwd() to that directory:
setwd("data/flags/flat/64")
pngFiles <- list.files() # Get the file names

pngList <- list()
for(ii in pngFiles){
  tempName <- gsub(".png", "", ii)
  tempPNG <- readPNG(ii) # Loads PNGs
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

# Iterate through tie thresholds until we have a fully connected graph
# just for the purposes of this example
distanceList <- seq(1/100, max(flagDistance), len = 100)
for(dd in distanceList){
  flagNetwork <- (as.matrix(flagDistance) < dd) * 1
  howConnected <- connectedness(flagNetwork)
  if(howConnected >= 1){break()}
}

# Make a similarity network
flagNetwork <- (as.matrix(flagDistance) < dd) * 1
eigenvectorCentrality <- evcent(flagNetwork)
names(eigenvectorCentrality) <- rownames(flagNetwork)
as.matrix(sort(eigenvectorCentrality))

########
# Plot #
########

# Here, gplot is drawing a new random layout every time. By assigning it
# to graphCoordinates, we save the vertex locations for later use
graphCoordinates <- gplot(dat = flagNetwork, # Adjacency matrix
                          mode = "kamadakawai", # Layout algorithm
                          vertex.col = rgb(meanRGB), # Average flag color
                          pad = 0, # To reduce margins around graph
                          arrowhead.cex = 0, # Hide arrowheads
                          vertex.sides = 4, # Make vertices square
                          vertex.rot = 45)

# Namely, to plot the flag raster images over the graph vertices
boxParameter <- 400 # To alter dimensions of raster image bounding box
png("netTest.png", h = 1600, w = 1600, type = "cairo-png")
par(mai = c(0, 0, 0, 0)) # First plot the graph edges:
gplot(dat = flagNetwork, # Adjacency matrix
      coord = graphCoordinates, # From above
      arrowhead.cex = 0, # Hide arrowheads
      vertex.cex = 0, # Make vertices invisible
      edge.col = "#99999966") # Make edges gray and translucent

for(ii in 1:length(pngList)){ # Go through each flag
  Coords <- graphCoordinates[ii, 1:2] # Get coordinates from graph object
  Dims <- flagDimensions[ii, ] # Get pixel dimensions
  rasterImage(pngList[[ii]], # Plot each flag with these boundaries:
              Coords[1]-Dims[2]/boxParameter, Coords[2]-Dims[1]/boxParameter,
              Coords[1]+Dims[2]/boxParameter, Coords[2]+Dims[1]/boxParameter)
}
dev.off()
