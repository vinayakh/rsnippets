# Description : Analysis of Social networks.
# Website :http://blog.revolutionanalytics.com/2013/09/r-and-the-journal-of-computational-and-graphical-statistics.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("rgl","latentnet")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#Two-dimensional Euclidean latent space model with three clusters and random 
# receiver effects
library(latentnet)
data(sampson)
monks.d2G3r <- ergmm(samplike ~ euclidean(d=2,G=3)+rreceiver)
Z <- plot(monks.d2G3r, rand.eff="receiver", pie=TRUE, vertex.cex=2)
text(Z, label=1:nrow(Z))

#Three-dimensional Euclidean latent space model with three clusters and 
# random receiver effects
library(latentnet)
data(sampson)
monks.d3G3r <- ergmm(samplike ~ euclidean(d=3,G=3)+rreceiver)
plot(monks.d3G3r, rand.eff="receiver",use.rgl=TRUE, labels=TRUE)