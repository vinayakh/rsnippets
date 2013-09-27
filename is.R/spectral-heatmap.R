# Description : Simple ggplot2 heatmap with colorBrewer "spectral" palette
# Website : http://is-r.tumblr.com/post/32387034930/simplest-possible-heatmap-with-ggplot2

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "reshape2", "RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate a random matrix
# This can be any type of numeric matrix,
# though we often see heatmaps of square correlation matrices.
nRow <- 9
nCol <- 16

myData <- matrix(rnorm(nRow * nCol), ncol = nCol)
rownames(myData) <- letters[1:nRow]
colnames(myData) <- LETTERS[1:nCol]

# Replace with numbers that actually have a relationship:
for(ii in 2:ncol(myData)){ myData[, ii] <- myData[, ii-1] + rnorm(nrow(myData)) }
for(ii in 2:nrow(myData)){ myData[ii, ] <- myData[ii-1, ] + rnorm(ncol(myData)) }

# For melt() to work seamlessly, myData has to be a matrix.
longData <- melt(myData)
head(longData, 20)

# Optionally, reorder both the row and column variables in any order
# Here, they are sorted by mean value
longData$X1 <- factor(longData$Var1, names(sort(with(longData, by(value, Var1, mean)))))
longData$X2 <- factor(longData$Var2, names(sort(with(longData, by(value, Var2, mean)))))

# Define palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

zp1 <- ggplot(longData,
              aes(x = X2, y = X1, fill = value))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
zp1 <- zp1 + coord_equal()
zp1 <- zp1 + theme_bw()
print(zp1) # Your plot will look different, depending on the seed