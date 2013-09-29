# Description : Faceting as a preferable alternative to 3-D
# Website : http://is-r.tumblr.com/post/33829008995/faceting-as-a-preferable-alternative-to-3-d

# Better alternatives to 3-D
# Here are some examples of what we don't want to do:
# http://stackoverflow.com/questions/12765497/create-a-ribbon-plot

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Air passenger data. ts converted to long matrix:
myData <- data.frame(Year = c(floor(time(AirPassengers) + .01)),
                     Month = c(cycle(AirPassengers)),
                     Value = c(AirPassengers))
# Easy conversion code from: http://stackoverflow.com/a/4973859/479554

# Convert month numbers to names, using a built-in constant:
myData$Month <- factor(myData$Month)
levels(myData$Month) <- month.abb

# One possibility:
zp1 <- ggplot(myData,
              aes(x = Year, y = Value, colour = Month))
zp1 <- zp1 + geom_line()
print(zp1) # This is fine, if you can differentiate between the colors

# Another possibility:
zp2 <- ggplot(myData,
              aes(x = Year, y = Value))
zp2 <- zp2 + geom_line()
zp2 <- zp2 + facet_wrap(~ Month)
print(zp2) # This is fine, but it's hard to compare across facets

# A third possibility; plotting reference lines across each facet:
referenceLines <- myData # \/ Rename
colnames(referenceLines)[2] <- "groupVar"
zp3 <- ggplot(myData,
              aes(x = Year, y = Value))
zp3 <- zp3 + geom_line(data = referenceLines, # Plotting the "underlayer"
                       aes(x = Year, y = Value, group = groupVar),
                       colour = "GRAY", alpha = 1/2, size = 1/2)
zp3 <- zp3 + geom_line(size = 1) # Drawing the "overlayer"
zp3 <- zp3 + facet_wrap(~ Month)
zp3 <- zp3 + theme_bw()
print(zp3)