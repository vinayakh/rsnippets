# Description : Interactive Color picker using locator
# Website : http://is-r.tumblr.com/post/35332733463/interactive-color-picker-using-locator

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "proxy")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

nIncrements <- 36
eachIncrement <- seq(0, 1, len = nIncrements)
colorSweep <- expand.grid(eachIncrement, eachIncrement, eachIncrement)
moduloRemainder <- with(colorSweep, Var1*(nIncrements-1)) %%
  floor(sqrt(nIncrements))
quotientFloor <- floor(with(colorSweep, Var1*(nIncrements-1)) /
                         floor(sqrt(nIncrements)))

colorLocations <- with(colorSweep, data.frame(x = moduloRemainder+Var2*8/9,
                                              y = quotientFloor+Var3*8/9))
hexColor <- with(colorSweep, hcl(Var1*360, Var2*150, Var3*100, fixup = FALSE))
colorLocations <- colorLocations[!is.na(hexColor), ]
hexColor <- hexColor[!is.na(hexColor)]

par(mai = c(0, 0, 0, 0)) # Plot color picker
plot(colorLocations$x, colorLocations$y, type = "n")
abline(h = -10:60/10, v = -10:60/10, col = gray(2/3))
points(colorLocations$x, colorLocations$y, col = hexColor, pch = 20)

# Use your mouse to click on the colors you want in your palette --
# click as many as you want, in the order you want, then:
# if in RStudio: hit [ESC]
# if in Windows RGui: Right click on graph, select [Stop]
whichColor <- locator(type = "o") # Click, then [ESC]/[Stop]!
colorProximities <- proxy::dist(data.frame(whichColor), colorLocations)
whichColor <- apply(colorProximities, 1, which.min)

myPalette <- colorRampPalette(hexColor[whichColor], space = "Lab")

zp1 <- ggplot(iris, # Plot some iris data
              aes(x = Sepal.Length, y = Sepal.Width,
                  colour = Petal.Width))
zp1 <- zp1 + geom_point(size = 4)
print(zp1) # Default ggplot2 continuous palette

zp1 <- zp1 + scale_colour_gradientn(colours = myPalette(99))
print(zp1) # Plotted with your hand-picked palette