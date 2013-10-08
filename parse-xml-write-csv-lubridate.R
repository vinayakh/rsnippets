# Description : Parse XML, extract dates and write csv
# Website : http://is-r.tumblr.com/post/36059986744/gathering-realclearpolitics-polling-trends-with-xml

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