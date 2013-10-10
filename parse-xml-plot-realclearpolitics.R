# Description : Parse XML and plot data with faux axis
# Website : http://is-r.tumblr.com/post/36133159870/plotting-realclearpolitics-polling-trends-with-a-faux

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("XML", "ggplot2", "lubridate", "reshape2", "scales")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Find your XML file from those listed at
# http://cdn.realclearpolitics.com/epolls/charts/
URL <- "http://cdn.realclearpolitics.com/epolls/charts/1171.xml"
parsedXML <- xmlParse(URL) # First pass

dateSeries <- xpathSApply(parsedXML, path = "//series") # Check the structure
Date <- sapply(xmlChildren(dateSeries[[1]]), xmlValue) # of the XML. <series>
names(Date) <- sapply(xmlChildren(dateSeries[[1]]), # is the first child node.
                      xmlGetAttr, "xid")

graphSeries <- xpathSApply(parsedXML, path = "//graph") # <graph> is the
obamaSeries <- sapply(xmlChildren(graphSeries[[1]]), xmlValue) # second major
names(obamaSeries) <- sapply(xmlChildren(graphSeries[[1]]), # child node.
                             xmlGetAttr, "xid") # ^ The first graph line
romneySeries <- sapply(xmlChildren(graphSeries[[2]]), xmlValue) # is "Obama"
names(romneySeries) <- sapply(xmlChildren(graphSeries[[2]]), # The second
                              xmlGetAttr, "xid") # ^ is "Romney."

# Put all of these series into a data.frame
rcpData <- data.frame(xid = names(Date), stringsAsFactors = FALSE)
rcpData$Date <- mdy(Date[rcpData$xid], tz = "EST") # lubridate!
rcpData$Romney <- as.numeric(romneySeries[rcpData$xid])
rcpData$Obama <- as.numeric(obamaSeries[rcpData$xid])

write.csv(rcpData, "RealClearPolitics Polling Average.csv", row.names = F)

with(rcpData, plot(Date, Obama, ylim = c(40, 50), type = "l"))
with(rcpData, lines(Date, Romney, col = "RED"))

longRCP <- melt(rcpData, id.vars = c("xid", "Date"))

zp1 <- ggplot(longRCP) # v Add the two time series
zp1 <- zp1 + geom_line(aes(x = Date, y = value, colour = variable))
zp1 <- zp1 + geom_area(data = rcpData, # Add the difference
                       aes(x = Date, y = Obama - Romney))
print(zp1)

# Write a function that "takes a chunk" out of a series
scaleBreaker <- function(x, mn, mx){
  y <- x # ^ vector, low end, high end
  y[x > mn & x < mx] <- mn
  y[x >= mx] <- x[x >= mx] - mx + mn
  return(y)
}

# Make it into a scale, with the package scales
break_trans = function() trans_new("break", # min v v max
                                   function(x) scaleBreaker(x, 8, 41),
                                   function(x) x)

myBreaks <- myLabels <- c(-1:7, 30, 42:49)
myLabels[10] <- "break" # Set custom breaks in the ordinary way
zp1 <- zp1 + scale_y_continuous(breaks = myBreaks, labels = myLabels)
zp1 <- zp1 + coord_trans(y = "break") # Apply our custom scale
zp1 <- zp1 + geom_hline(yintercept = 30, lty = 4) # Highlight the axis break
print(zp1) # This is in the middle ^ of our axis break