# Description : Transforming a color scale
# Website : http://is-r.tumblr.com/post/32796668721/transforming-a-color-scale

# Transforming a color scale

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "RColorBrewer", "scales")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate some data
nn <- 10000
myData <- data.frame(X = rnorm(nn),
                     Y = rnorm(nn))
myData$Z <- with(myData, X * Y)

# Make a standard scatterplot with Z encoded into point colour
zp1 <- ggplot(myData,
              aes(x = X, y = Y, colour = Z))
zp1 <- zp1 + geom_point()
zp1 <- zp1 + theme_bw()
zp1 <- zp1 + coord_equal()
zp1 <- zp1 + scale_colour_gradientn(colours = colorRampPalette(rev(brewer.pal(11, "Spectral")))(1000),
                                    breaks = c(-2:2 * 2))
zp1 <- zp1 + ggtitle("Untransformed")
print(zp1)

# Define a transformation
norm_trans <- function(){
  trans_new('norm', function(x) pnorm(x), function(x) qnorm(x))
}

# Apply transformed colour scale
zp2 <- zp1 + scale_colour_gradientn(colours = colorRampPalette(rev(brewer.pal(11, "Spectral")))(1000),
                                    trans = 'norm', # Apply the transformation here
                                    breaks = c(-2:2)) # Note the change in breaks
zp2 <- zp2 + ggtitle("Transformed")
print(zp2)