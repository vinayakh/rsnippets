# Description : From holey polygons to convex hulls
# Website : http://is-r.tumblr.com/post/33356702763/from-holey-polygons-to-convex-hulls

# Highlighting clusters, with chull()

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate some data
nn <- 500
myData <- data.frame(X = rnorm(nn),
                     Y = rnorm(nn))

plot(myData)

# K-means clustering of this data
setK = 6 # How many clusters?
clusterSolution <- kmeans(myData, centers = setK)

myData$whichCluster <- factor(clusterSolution$cluster)

# One plotting approach, just colored points:
zp1 <- ggplot(data = myData,
              aes(x = X, y = Y, colour = whichCluster))
zp1 <- zp1 + geom_point()
zp1 <- zp1 + coord_equal()
zp1 <- zp1 + scale_colour_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(setK))
print(zp1)

# Another approach (which does not work):
zp2 <- ggplot(data = myData,
              aes(x = X, y = Y, fill = whichCluster))
zp2 <- zp2 + geom_polygon(alpha = 1/2) # Wow! Look at those zany polygons!
zp2 <- zp2 + geom_point(colour = "BLACK")
zp2 <- zp2 + coord_equal()
zp2 <- zp2 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(setK))
print(zp2)

# Identify the convex hull, in three steps
# See also: http://www.jstatsoft.org/v40/i01/paper
splitData <- split(myData, myData$whichCluster)
appliedData <- lapply(splitData, function(df){
  df[chull(df), ] # chull really is useful, even outside of contrived examples.
})
combinedData <- do.call(rbind, appliedData)
# do.call is useful for combining anything post-*apply

# Now, try the previous plot again
zp3 <- ggplot(data = myData,
              aes(x = X, y = Y))
zp3 <- zp3 + geom_polygon(data = combinedData, # This is also a nice example of how to plot
                          aes(x = X, y = Y, fill = whichCluster), # two superimposed geoms
                          alpha = 1/2) # from different data.frames
zp3 <- zp3 + geom_point(size = 1)
zp3 <- zp3 + coord_equal()
zp3 <- zp3 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(setK))
print(zp3)