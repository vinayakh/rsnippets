# Description : Custom Legends in R
# Website : http://www.mollietaylor.com/2013/10/custom-legend-in-r.html

toInstall <- c("OIdata","classInt","RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# load state data from OIdata package:
data(state)

nclr <- 8 # number of bins
min <- 0 # theoretical minimum
max <- 100 # theoretical maximum
breaks <- (max - min) / nclr

# set up colors:
plotclr <- brewer.pal(nclr, "Oranges")
plotvar <- state$coal
class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, plotclr)

# map data:
# base
map("state", col = "gray80", fill = TRUE, lty = 0)
# data
map("state", col = colcode, fill = TRUE, lty = 0, add = TRUE)
# border
map("state", col = "gray", lwd = 1.4, lty = 1, add = TRUE)
# position
legend("bottomleft", legend = names(attr(colcode, "table")), title = "Percent", fill = attr(colcode, "palette"), cex = 0.56, bty = "n") 

# set up colors:
plotclr <- brewer.pal(nclr, "Oranges")
plotvar <- state$coal
class <- classIntervals(plotvar, nclr, style = "fixed", fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, plotclr)
NAColor <- "gray80"
plotclr <- c(plotclr, NAColor)

# map data:
# base
map("state", col = NAColor, fill = TRUE, lty = 0)
# data
map("state", col = colcode, fill = TRUE, lty = 0, add = TRUE)
# border
map("state", col = "gray", lwd = 1.4, lty = 1, add = TRUE)

# set legend text:
legendText <- c()
for(i in seq(min, max - (max - min) / nclr, (max - min) / nclr)) {
  if (i == max(seq(min, max - (max - min) / nclr, (max - min) / nclr))) {
    legendText <- c(legendText, paste(round(i,3), "\u2264 n \u2264", round(i + (max - min) / nclr,3)))
  } else
    legendText <- c(legendText, paste(round(i,3), "\u2264 n <", round(i + (max - min) / nclr,3))) 
}

# set legend text:
legendText <- c()
for(i in seq(min, max - (max - min) / nclr, (max - min) / nclr)) {
  if (i == max(seq(min, max - (max - min) / nclr, (max - min) / nclr))) {
    legendText <- c(legendText, paste(round(i,3), "\u2264 n \u2264", round(i + (max - min) / nclr,3)))
    if (!is.na(NAColor)) legendText <- c(legendText, "NA")
  } else
    legendText <- c(legendText, paste(round(i,3), "\u2264 n <", round(i + (max - min) / nclr,3))) 
}

# position
legend("bottomleft", legend = legendText, title = "Percent", fill = plotclr, cex = 0.56, bty = "n")