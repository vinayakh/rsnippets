# Description : Banking to n degrees
# Website : http://is-r.tumblr.com/post/33632821424/banking-to-n-degrees

# Banking lineplots to n degrees

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Some invented example data
myData <- data.frame(x = seq(1796, 2012, by = 4))
myData$y <- cumsum(rnorm(nrow(myData), 0, 10))

plot(myData, type = "l")

# Desired mean segment angle:
desiredDegrees <- 45 # Change this to see big differences in the result

# Calculate necessary aspect ratio
slopeFrame <- data.frame(apply(myData, 2, diff)) # Find "rise" and "run"
segmentDegrees <- atan(slopeFrame$y / slopeFrame$x) * 180/pi # Get angle
segmentLength <- sqrt(rowSums(slopeFrame ^ 2))
weightedMeanDegrees <- sum(abs(segmentDegrees) * segmentLength) /
  sum(segmentLength) # Weighted mean absolute segment angle
aspectRatio <- tan(abs(desiredDegrees) * pi/180) /
  tan(weightedMeanDegrees * pi/180) # Calculate aspect ratio

plot(myData, type = "l", asp = aspectRatio)
myData$trueDegrees <- c(segmentDegrees, 0)

zp1 <- ggplot(myData,
              aes(x = x, y = y, colour = trueDegrees))
zp1 <- zp1 + geom_line(size = 1)
zp1 <- zp1 + geom_line(size = 1/2)
zp1 <- zp1 + theme_bw()
print(zp1) # With default aspect ratio

zp2 <- zp1 + coord_equal(aspectRatio)
print(zp2) # With adjusted aspect ratio