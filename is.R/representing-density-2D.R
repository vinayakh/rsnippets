# Description : Representing Density in 2D
# Website : http://is-r.tumblr.com/post/36204333530/representing-density-in-two-dimensions

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate some randomly-distributed data
nObs <- 5000
myData <- data.frame(X = rnorm(nObs), Y = rnorm(nObs))
nClusters <- 7 # Cluster it
kMeans <- kmeans(myData, centers = nClusters)
myData$Cluster <- as.factor(kMeans$cluster)

# Plot points colored by cluster
zp1 <- ggplot(myData,
              aes(x = X, y = Y, colour = Cluster))
zp1 <- zp1 + geom_point()
print(zp1)

# Illustration of stat_density2d, encoding density into alpha
zp2 <- ggplot(myData,
              aes(x = X, y = Y))
zp2 <- zp2 + stat_density2d(aes(fill = Cluster, colour = Cluster,
                                alpha = ..level..),
                            geom = "polygon")
zp2 <- zp2 + scale_alpha(range = c(0, 1/2), guide = "none") # Narrow alpha range
zp2 <- zp2 + coord_equal()
print(zp2)

# Illustration of stat_density2d, encoding density into linewidth
zp3 <- ggplot(myData,
              aes(x = X, y = Y))
zp3 <- zp3 + stat_density2d(aes(colour = Cluster,
                                size = ..level..),
                            fill = "transparent",
                            geom = "polygon")
zp3 <- zp3 + scale_size(range = c(0, 2), guide = "none") # Narrow size/linewidth range
zp3 <- zp3 + coord_equal()
print(zp3)

# Combination of both
zp4 <- ggplot(myData,
              aes(x = X, y = Y))
zp4 <- zp4 + stat_density2d(aes(fill = Cluster, colour = Cluster,
                                alpha = ..level..,
                                size = ..level..),
                            geom = "polygon")
zp4 <- zp4 + scale_alpha(range = c(0, 1/2), guide = "none") # Narrow alpha range
zp4 <- zp4 + scale_size(range = c(0, 3/2), guide = "none") # Narrow size/linewidth range
zp4 <- zp4 + coord_equal()
print(zp4)