# Description : GGtutorial: Day 5 - Gradient Colors and Brewer Palettes (part2)
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

B1 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Set1") + labs(title = "Palette=''Set1''\n" )


B2 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Set2") + labs(title = "Palette=''Set2''\n" )


B3 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Paired") + labs(title = "Palette=''Paired''\n" )


B4 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Pastel2") + labs(title = "Palette=''Pastel2''\n" )


Brewers <- grid.arrange(B1, B2, B3, B4, ncol=2)


B5 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Blues") + labs(title = "Palette=''Blues''\n" )


B6 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Oranges") + labs(title = "Palette=''Oranges''\n" )


B7 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Reds") + labs(title = "Palette=''Reds''\n" )


B8 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Purples") + labs(title = "Palette=''Purples''\n" )



Brewers.2 <- grid.arrange(B5, B6, B7, B8, ncol=2)



B9 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="RdYlGn") + labs(title = "Palette=''RdYlGn''\n" )


B10 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="RdBu") + labs(title = "Palette=''RdBu''\n" )


B11 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="PRGn") + labs(title = "Palette=''PRGn''\n" )


B12 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="BrBg") + labs(title = "Palette=''BrBg''\n" )

Brewers.3 <- grid.arrange(B9, B10, B11, B12, ncol=2)