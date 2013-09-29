# Description : Correlation ellipses
# Website : http://is-r.tumblr.com/post/34352513559/plotting-correlation-ellipses

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ellipse")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Function to plot colored correlation ellipses
correlationEllipses <- function(cor){
  require(ellipse)
  ToRGB <- function(x){rgb(x[1]/255, x[2]/255, x[3]/255)}
  C1 <- ToRGB(c(178, 24, 43))
  C2 <- ToRGB(c(214, 96, 77))
  C3 <- ToRGB(c(244, 165, 130))
  C4 <- ToRGB(c(253, 219, 199))
  C5 <- ToRGB(c(247, 247, 247))
  C6 <- ToRGB(c(209, 229, 240))
  C7 <- ToRGB(c(146, 197, 222))
  C8 <- ToRGB(c(67, 147, 195))
  C9 <- ToRGB(c(33, 102, 172))
  CustomPalette <- colorRampPalette(rev(c(C1, C2, C3, C4, C5, C6, C7, C8, C9)))
  ord <- order(cor[1, ])
  xc <- cor[ord, ord]
  colors <- unlist(CustomPalette(100))
  plotcorr(xc, col=colors[xc * 50 + 50])
}

# An example, using canned data:
correlationEllipses(cor(mtcars))