# Description : Example of GoogleVis Doughnut Chart
# Website : http://lamages.blogspot.com/2013/09/doughnut-chart-in-r-with-googlevis.html
# Data taken from http://en.wikipedia.org/wiki/Bundestag

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("googleVis")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

dat <- data.frame(party=c("CDU", "FDP", "CSU", "SPD","The Left", "The Greens"),
                  members.of.parliament=c(193, 93, 44, 146, 76, 68))

library(googleVis)
## Doughnut chart - a pie with a hole
doughnut <- gvisPieChart(dat,
                         options=list(
                           width=500,
                           height=500,
                           slices="{0: {offset: 0.2},
1: {offset: 0.2},
2: {offset: 0.2}}",
                           title='German parliament 2009 - 2013
(Goverment: CDU/FDP/CSU)',
                           legend='none',
                           colors="['black','orange', 'blue',
'red', 'purple', 'green']",
                           pieSliceText='label',
                           pieHole=0.5),
                         chartid="doughnut")
plot(doughnut)