# Description : Simple ggplot2 heatmap, with optimal seriation
# Website : http://is-r.tumblr.com/post/32449990608/optimal-seriation-for-your-matrices

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "reshape2", "RColorBrewer", "seriation")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Using U.S. Judge Rating Data
myData <- as.matrix(USJudgeRatings)

# For melt() to work seamlessly, myData has to be a matrix.
longData <- melt(myData)
head(longData)

# Define palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "PuOr")))
# Experimenting with a different palette /\

zp1 <- ggplot(longData,
              aes(x = Var1, y = Var2, fill = value))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
zp1 <- zp1 + theme(axis.text.x=element_text(angle=45, hjust = 1, size = 5))
print(zp1) # Here, the axes have their original order

# "Optimally" reorder both the rows and columns
optimalSeriation <- seriate(myData, method = "BEA_TSP")
# Most methods require a non-
# negative matrix

longData$Var1 <- factor(longData$Var1, names(unlist(optimalSeriation[[1]][])))
longData$Var2 <- factor(longData$Var2, names(unlist(optimalSeriation[[2]][])))

# The same plot, but with axes reordered according to optimal seriation
zp1 %+% longData