# Description : GGtutorial: Day 4 - More Colors
# Website : http://is-r.tumblr.com/post/34751840615/ggtutorial-day-4-more-colors

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

#Random Colors
my.colors <- sample(colors(), 7)
RANDOM <- ggplot(data = diamonds) + geom_point(aes( x = carat, y = price, color=factor(color))
) + facet_wrap(~color) + scale_color_manual(values = my.colors) + labs(title = "+ scale_color_manual(values=sample(colors(), 7)\n" )



#Custom Colors 1
colors()
custom.colors.1 <- c("red", "orange" , "yellow", "green", "blue", "blueviolet", "violet")
C1 <- ggplot(data = diamonds) + geom_point(aes( x = carat, y = price, color=factor(color))
) + facet_wrap(~color) + scale_color_manual(values = custom.colors.1) + labs(title = "+ scale_color_manual(values=custom.colors.1)\n" )


#Custom Colors 2
custom.colors.2 <- c("skyblue1", "skyblue2" , "skyblue3", "steelblue", "springgreen2", "springgreen3", "springgreen4")
C2 <- ggplot(data = diamonds) + geom_point(aes( x = carat, y = price, color=factor(color))
) + facet_wrap(~color) + scale_color_manual(values = custom.colors.2
) + labs(title = " + scale_color_manual(values = custom.colors.2) \n" )

#Rainbow Colors
rainbow(6)
ROYGBIV <- rainbow(6)
RAINBOW <- ggplot(data = diamonds) + geom_point(aes( x = carat, y = price, color=factor(color))
) + facet_wrap(~color) + scale_color_manual(values = ROYGBIV) + labs(title = " + scale_color_manual(values = rainbow(6)) \n" )

Example.1 <- grid.arrange(RANDOM, C1, C2, RAINBOW, ncol=2)