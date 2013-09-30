# Description : GGtutorial: Day 5 - Gradient Colors and Brewer Palettes
# Website : http://is-r.tumblr.com/post/34821021257/ggtutorial-day-5-gradient-colors-and-brewer-palettes

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "gridExtra")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#Load ggplot default data: Diamonds
library(ggplot2)
library(gridExtra)
data(diamonds)
head(diamonds)
diamonds <- diamonds[diamonds$color < "J",]
#http://127.0.0.1:25615/library/ggplot2/html/diamonds.html

#Gradient Colors - Good For Continuous Scales

G5 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + labs(title = "Base Plot \n" )

G6 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + scale_colour_gradientn(colours=rainbow(2)) + labs(title = " + scale_colour_gradientn(colours=rainbow(2)) \n" )

G7 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + scale_colour_gradientn(colours=c("red", "blue")) + labs(title = " + scale_colour_gradientn(colours=c(''red'', ''blue'')) \n" )

G8 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + scale_colour_gradientn(colours=c("white", "dodgerblue")) + labs(title = " + scale_colour_gradientn(colours=c(''white'', ''dodgerblue'')) \n" )

Gradient2 <- grid.arrange(G5, G6, G7, G8, ncol=2)