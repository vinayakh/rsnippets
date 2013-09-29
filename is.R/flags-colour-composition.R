# Description : Finding color composition of flags
# Website : http://is-r.tumblr.com/post/34092273022/distribution-of-colors-by-flag

# Replicating / improving http://shaheeilyas.com/flags/

# Drawing a scatter plot of raster images
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("png", "reshape", "ggplot2", "MASS")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Go to https://www.gosquared.com/resources/2400-flags, download the ZIP,
# and put the 64 x 64 files into a directory of your choosing.
# Then setwd() to that directory:
setwd("~/github/rsnippets/is.R/flags-iso/flat/64/")
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

# Similarity, through Kruskal non-metric MDS
flagDistance <- dist(meanRGB)
flagDistance[flagDistance <= 0] <- 1e-10
MDS <- isoMDS(flagDistance)$points

# This is slow, it "flattens" the matrices of RGBalpha values
flagColors <- sapply(pngList, function(ll){
  do.call(c, lapply(ll, unlist))
})

# This converts those flattened frames to hex codes
flagColors <- apply(flagColors, 2, function(cc){
  colMat <- matrix(cc, ncol = 4)
  sort(table(hex <- rgb(colMat[, 1], colMat[, 2], colMat[, 3], colMat[, 4])))
})

# Sort of a hot mess, getting a data frame of pixel counts by color and flag
flagColors <- melt(flagColors)
flagColors$Alpha <- 1 # Pixel transparency indicator
flagColors$Alpha[substr(flagColors$Var.1, 8, 9) == "00"] <- 0
flagColors <- flagColors[flagColors$Alpha == 1, ] # Drop transparent pixels
flagColors$Var.1 <- substr(flagColors$Var.1, 1, 7)

hsvFrame <- t(rgb2hsv(col2rgb(flagColors$Var.1)))
flagColors$Orderer <- rowSums(sweep(hsvFrame, 2, c(1e5, 1e3, 1e1), "*"))
flagColors$L1 <- factor(flagColors$L1, # /\ Order colors by Hue, then Sat.,
                        rownames(MDS)[order(MDS[, 2])]) # then Value
flagColors <- arrange(flagColors, Orderer)
flagColors$RHS <- with(flagColors, # Find the right edge of each color band
                       unsplit(lapply(split(value, L1),
                                      cumsum), L1))
flagColors$Var.1 <- as.factor(flagColors$Var.1)

zp1 <- ggplot(flagColors,
              aes(xmax = RHS, xmin = RHS - value,
                  ymin = 0, ymax = 10,
                  fill = Var.1))
zp1 <- zp1 + facet_wrap(~ L1, ncol = 11)
zp1 <- zp1 + geom_hline(yintercept = 1:9, colour = "WHITE")
zp1 <- zp1 + geom_rect() # Drawing swaths of color
# This uses a nice trick we can talk about later...
zp1 <- zp1 + scale_fill_manual(breaks = levels(flagColors$Var.1),
                               values = as.character(levels(flagColors$Var.1)),
                               guide = "none")
zp1 <- zp1 + scale_x_continuous(breaks = NULL, expand = c(0, 0))
zp1 <- zp1 + scale_y_continuous(breaks = NULL, expand = c(0, 0))
print(zp1)