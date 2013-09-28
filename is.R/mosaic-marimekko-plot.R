# Description : Simplest possible marimekko/mosaic plot
# Website : http://is-r.tumblr.com/post/33290921643/simple-marimekko-mosaic-plots

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("vcd", "ggplot2", "RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

theme_set(theme_gray(base_size = 7))

# All you need to start with is individual count data, and a grouping variable
# here, a "count" of dollar salary, with individuals grouped by teams
baseballSalaries <- Baseball[, c("name2", "team87", "sal87", "years")]
# This data comes from the "vcd" package
baseballSalaries <- baseballSalaries[complete.cases(baseballSalaries), ]
# A handy function to know.
head(baseballSalaries)

### Need to calculate four things for each box ###
# Box width
baseballSalaries$team87 <- as.character(baseballSalaries$team87) # Just to make sure things work right
baseballSalaries$width <- with(baseballSalaries, by(sal87, team87, sum, na.rm = T)[team87])
# Box height
baseballSalaries$height <- with(baseballSalaries, sal87 / width)
# Right edge
baseballSalaries$right <- with(baseballSalaries, cumsum(sort(by(sal87, team87, sum, na.rm = T)))[team87])
# Also a handy function, for a cumulative sum
# Top edge
baseballSalaries <- baseballSalaries[order(baseballSalaries$height), ] # Order by height
# Lots of nested functions here, subjects of another Gist:
# 1 2 3 4
baseballSalaries$top <- with(baseballSalaries, unsplit(lapply(split(height, team87), cumsum), team87))
head(baseballSalaries) # See what's been added

### Plot ###
# To produce a nice x-axis, find the center of each column:
xBreaks <- c(with(baseballSalaries, by(right - width / 2, team87, mean)))
xLabels <- names(xBreaks)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

zp1 <- ggplot(baseballSalaries,
              aes(xmin = right - width,
                  xmax = right,
                  ymin = top - height,
                  ymax = top,
                  fill = log(years)))
zp1 <- zp1 + geom_rect(colour = "WHITE")
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
zp1 <- zp1 + scale_y_continuous(expand = c(0, 0))
zp1 <- zp1 + scale_x_continuous(expand = c(0, 0),
                                breaks = xBreaks, # Derived
                                labels = xLabels) # above.
# We could add titles, names, labels, etc., but this Gist is already long enough
print(zp1)

###################
# Another example #
###################

# Another marimekko/mosaic plot, this is more like a stacked,
# variable width, bar plot

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("reshape", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Canonical example of categorical data
HEC <- apply(HairEyeColor, c(1, 2), sum)
HEC <- melt(HEC)
HEC

### Need to calculate four things for each box ###
# Box width
HEC$Eye <- as.character(HEC$Eye) # Just to make sure things work right
HEC$width <- with(HEC, by(value, Eye, sum, na.rm = T)[Eye])
# Box height
HEC$height <- with(HEC, value / width)
# Right edge
HEC$right <- with(HEC, cumsum(sort(by(value, Eye, sum, na.rm = T)))[Eye])
# Also a handy function, for a cumulative sum
# Top edge
HEC <- HEC[order(HEC$height), ] # Order by height
# Lots of nested functions here, subjects of another Gist:
# 1 2 3 4
HEC$top <- with(HEC, unsplit(lapply(split(height, Eye), cumsum), Eye))
head(HEC) # See what's been added

### Plot ###
# To produce a nice x-axis, find the center of each column:
xBreaks <- c(with(HEC, by(right - width / 2, Eye, mean)))
xLabels <- names(xBreaks)

zp2 <- ggplot(HEC,
              aes(xmin = right - width,
                  xmax = right,
                  ymin = top - height,
                  ymax = top,
                  fill = Hair))
zp2 <- zp2 + geom_rect(colour = "WHITE")
zp2 <- zp2 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(nlevels(HEC$Hair)))
zp2 <- zp2 + scale_y_continuous(expand = c(0, 0))
zp2 <- zp2 + scale_x_continuous(expand = c(0, 0),
                                breaks = xBreaks, # Derived
                                labels = xLabels) # above.
print(zp2)