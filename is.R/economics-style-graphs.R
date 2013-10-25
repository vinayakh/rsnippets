# Description : Economics Style Graph
# Website : http://is-r.tumblr.com/post/37631901708/economics-style-graphs-with-bezier-from-hmisc

doInstall <- TRUE
toInstall <- c("Hmisc", "ggplot2", "proxy", "grid")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Example usage
x <- c(4,6,4,5,6,7)
y <- 1:6
plot(x, y, "o", pch=20) # bezier() generates smoothed curves from these points
points(bezier(x, y), type="l", col="red")

# Randomly-generated curve:
x <- runif(10) # The evaluation argument dictates the length of the output
y <- runif(10) # vector. For our eventual purposes, it is useful to have a
plot(x, y, "o", pch=20) # pretty fine-grained vector
points(bezier(x, y, evaluation = 500), type="l", col="red")

x = c(1, 1, 1, 10) # Aiming for something specific here
y = c(10, 1, 1, 1) # namely, isoquant lines
plot(x, y, "o", pch=20)
points(bezier(x,y,20), type="l", col="red")
points(bezier(x+1,y+1), type="l", col="red")
points(bezier(x+2,y+2), type="l", col="red")

### Supply and Demand ###
# Replicating http://en.wikipedia.org/w/index.php?title=File:Supply-demand-right-shift-demand.svg&page=1
x <- c(1, 8, 9)
y <- c(1, 5, 9)
supply1 <- data.frame(bezier(x, y, evaluation = 500))
x <- c(1, 3, 9)
y <- c(9, 3, 1)
demand1 <- data.frame(bezier(x, y, evaluation = 500))
demand2 <- data.frame(bezier(x+2, y+2, evaluation = 500))

# Helper function to identify approximate curve intersections by brute force
approxIntersection <- function(path1, path2){
  distanceMatrix <- proxy::dist(path1, path2)
  whichMin <- which(distanceMatrix == min(distanceMatrix), arr.ind = TRUE)
  return((path1[whichMin[1], ]+path2[whichMin[2], ])/2)
} # This is where a long bezier() output vector is useful

intersectS1D1 <- approxIntersection(supply1, demand1)
intersectS1D2 <- approxIntersection(supply1, demand2)
intersections <- data.frame(rbind(intersectS1D1, intersectS1D2))

textAnnotations <- data.frame(label = c("S", "D1", "D2"),
                              x = c(8, 1, 5), # DF of line labels
                              y = c(8, 8, 8))

zp1 <- qplot(x = 0:10, y = 0:10, geom = "blank") # Draw an empty plot
zp1 <- zp1 + geom_path(data = supply1, aes(x = x, y = y), # Add supply curve
                       size = 1, colour = "BLUE")
zp1 <- zp1 + geom_path(data = demand1, aes(x = x, y = y), # Add demand 1
                       size = 1, colour = "RED")
zp1 <- zp1 + geom_path(data = demand2, aes(x = x, y = y), # Add demand 2
                       size = 1, colour = "RED")
zp1 <- zp1 + geom_point(data = intersections, # Add points at intersections
                        aes(x = x, y = y), size = 3)
zp1 <- zp1 + geom_segment(data = intersections, # Add dotted lines
                          aes(x = x, y = 0, xend = x, yend = y),
                          lty = 2)
zp1 <- zp1 + geom_segment(data = intersections, # Add dotted lines
                          aes(x = 0, y = y, xend = x, yend = y),
                          lty = 2)
zp1 <- zp1 + geom_text(data = textAnnotations, # Add curve labels
                       aes(x = x, y = y, label = label))
zp1 <- zp1 + annotate("segment", x = 3.5, xend = 4.5, y = 6, yend = 7, # Arrow
                      arrow = arrow(length = unit(3,"mm")), colour = gray(1/2))
zp1 <- zp1 + scale_x_continuous("Quantity", expand = c(0, 0), # Clean up axis
                                breaks = intersections$x,
                                labels = expression(Q[1], Q[2]))
zp1 <- zp1 + scale_y_continuous("Price", expand = c(0, 0), # Clean up axis
                                breaks = intersections$y,
                                labels = expression(P[1], P[2]))
zp1 <- zp1 + theme_classic() # New in ggplot2 0.9.3. Time to update!
zp1 <- zp1 + coord_equal() # Force fixed x-y relationship
zp1 <- zp1 + ggtitle("A rightward shift in the demand curve") # Title
print(zp1)