# Description : Elongating and stacking wide data
# Website : http://is-r.tumblr.com/post/35120359268/elongating-and-stacking-wide-data

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("reshape2", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate some data, which we'll load in a minute:
dataA <- matrix(rnorm(26*50), nrow = 26)
dataA <- t(apply(dataA, 1, cumsum))
colnames(dataA) <- 1960:2009
dataA <- data.frame(cName = LETTERS, cCode = 1:26*100, dataA)

dataB <- matrix(rnorm(26*50), nrow = 26)
dataB <- t(apply(dataB, 1, cumsum))
colnames(dataB) <- 1960:2009
dataB <- data.frame(cName = LETTERS, cCode = 1:26*100, dataB)

dataC <- matrix(rnorm(26*50), nrow = 26)
dataC <- t(apply(dataC, 1, cumsum))
colnames(dataC) <- 1960:2009
dataC <- data.frame(cName = LETTERS, cCode = 1:26*100, dataC)

# Save
write.csv(dataA, "population.csv", row.names = F)
write.csv(dataB, "gdp.csv", row.names = F)
write.csv(dataC, "democracy.csv", row.names = F)

### Reload, make "tall," and stack ###

# May have to write this list manually:
csvNames <- c("population", "gdp", "democracy")

allData <- list()

for(cn in csvNames){
  newData <- read.csv(paste0(cn, ".csv"), as.is = T)
  newData <- melt(newData, id.vars = c("cName", "cCode"))
  colnames(newData)[3] <- "Year" # Include the .CSV name as a variable
  newData$Variable <- cn
  allData[[cn]] <- newData # Assign to a list
}

allData <- do.call(rbind, allData) # List to data.frame
allData$Year <- as.numeric(gsub("X", "", allData$Year)) # Fix Years

head(allData)

zp1 <- ggplot(allData)
zp1 <- zp1 + geom_line(aes(x = Year, y = value,
                           colour = cName))
zp1 <- zp1 + facet_grid(Variable ~ .)
zp1 <- zp1 + scale_colour_discrete(guide = "none")
print(zp1)