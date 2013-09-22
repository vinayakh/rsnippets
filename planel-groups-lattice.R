# Description : Using planel.groups in lattice
# Website : http://lamages.blogspot.in/2013/09/using-planelgroups-in-lattice.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("lattice")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(lattice)
my.settings <- list(
  superpose.symbol=list(pch=19),
  plot.line=list(col="black"),
  plot.polygon=list(col=adjustcolor(col="steelblue",
                                    alpha.f=0.5),
                    border="white")
)

xyplot(Sepal.Length ~ Sepal.Width, groups=Species, data=iris,
       main="Lattice example with panel.groups",
       par.settings = my.settings,
       panel=panel.superpose,
       panel.groups=function(x,y, group.number,...){
         specie <- levels(iris$Species)[group.number]
         if(specie %in% "setosa"){
           panel.xyplot(x,y,...)
           panel.abline(lm(y~x))
         }
         if(specie %in% "versicolor"){
           panel.barchart(x, y)
         }
         if(specie %in% "virginica"){
           panel.rug(x, y)
         }
       }
)