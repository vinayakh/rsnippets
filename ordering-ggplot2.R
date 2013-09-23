# Description : Order by Demo
# Website : http://trinkerrstuff.wordpress.com/2013/08/14/how-do-i-re-arrange-ordering-a-plot-revisited/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplo2","gridExtra","devtools","plotflow")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
               lapply(toInstall, library, character.only = TRUE)

mtcars3 <-mtcars2 <-data.frame(car=rownames(mtcars), mtcars, row.names=NULL)
mtcars3$cyl  <-mtcars2$cyl <-as.factor(mtcars2$cyl)

library(ggplot2)
library(gridExtra)
x <-ggplot(mtcars2, aes(y=car, x=mpg)) +
  geom_point(stat="identity")

## Relevel the cars by mpg
y <-ggplot(mtcars2, aes(x=car, y=mpg)) + geom_bar(stat="identity") 

mtcars3$car <-factor(mtcars2$car, levels=mtcars2[order(mtcars$mpg), "car"])

x <-ggplot(mtcars3, aes(y=car, x=mpg)) +
  geom_point(stat="identity")

y <-ggplot(mtcars3, aes(x=car, y=mpg)) +
  geom_bar(stat="identity") +
  coord_flip()

grid.arrange(x, y, ncol=2)

## Relevel the carb by average mpg
(ag_mtcars <-aggregate(mpg ~ carb, mtcars3, mean))
mtcars3$carb <-factor(mtcars2$carb, levels=ag_mtcars[order(ag_mtcars$mpg), "carb"])

ggplot(mtcars3, aes(y=carb, x=mpg)) +
  geom_point(stat="identity", size=2, aes(color=carb)) + coord_flip()

grid.arrange(x, y, ncol=2)

ggplot(mtcars3, aes(y=car, x=mpg)) +
  geom_point(stat="identity") +
  facet_grid(cyl~., scales = "free", space="free")

# Usig Plotflow
library(devtools)
install_github("plotflow", "trinker")

library(plotflow)
dat <-aggregate(cbind(mpg, hp, disp)~carb, mtcars, mean)
dat$carb <-factor(dat$carb)

## compare levels (data set looks the same though)
dat$carb
order_by(carb, ~-hp + -mpg, data = dat)$carb

## Return just the vector with new levels
order_by(carb, ~ -hp + -mpg, dat, df=FALSE)
library(ggplot2)

## Reset the data from Section 1
dat2 <-data.frame(car=rownames(mtcars), mtcars, row.names=NULL)
ggplot(order_by(car, ~ mpg, dat2), aes(x=car, y=mpg)) +
  geom_bar(stat="identity") +
  coord_flip() + ggtitle("Order Pretty Easy")

## Ordered points with the order_by function
a <-ggplot(order_by(carb, ~ mpg, dat2, mean), aes(x=carb, y=mpg)) +
  geom_point(stat="identity", aes(colour=carb)) +
  coord_flip() + ggtitle("Ordered Dot Plots Made Easy")

## Reverse the ordered points
b <-ggplot(order_by(carb, ~ -mpg, dat2, mean), aes(x=carb, y=mpg)) +
  geom_point(stat="identity", aes(colour=carb)) +
  coord_flip() + ggtitle("Reverse Order Too!")

grid.arrange(a, b, ncol=1)

ggplot(order_by(gear, ~mpg, dat2, mean), aes(mpg, carb)) +
  geom_point(aes(color=factor(cyl))) +
  facet_grid(gear~., scales="free") + ggtitle("I'm Nested (Yay for me!)")