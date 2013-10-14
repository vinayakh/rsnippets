# Description : Flexibly outlining a set of points with alphahull
# Website :http://is-r.tumblr.com/post/37327158020/flexibly-outlining-a-set-of-points-with-alphahull

doInstall <- TRUE
toInstall <- c("alphahull")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate some sample data:
myData <- data.frame(x = rnorm(500), y = rnorm(500))
# With a unit-circle hole in the center:
myData <- myData[sqrt(rowSums(myData^2)) > 1, ]
plot(myData)

# Convex hull
cHullPoints <- chull(myData)
polygon(myData[cHullPoints, ])

aHullPoints <- ahull(myData, alpha = 1)
plot(aHullPoints) # Plot alpha hull
plot(aHullPoints, wlines = "both") # with Delaunay triangulation and Voronoi

# Area and lenght of alpha hull shape
areaahull(aHullPoints)
lengthahull(aHullPoints$arcs)

# See how the hull varies with alpha:
par(mfcol = c(4, 4), mai = c(0, 0, 0, 0))
lapply((1:16/10)^2, function(ii){
  plot(ahull(myData, alpha = ii))
  text(0, 0, ii) })
par(mfcol = c(1, 1), mai = c(1, 1, 1, 1))