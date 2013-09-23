# Description : Using Heatmaps to Uncover the Individual-Level Structure of Brand Perceptions 
# Website : http://joelcadwell.blogspot.in/2013/08/using-heatmaps-to-uncover-individual.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c(psych","gplots")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# use psych package to simulate Rasch model data
library(psych)

# need to set seed in order to obtain same result each time
set.seed(36321)

# 8 items with difficulty specified by vector d
# 200 respondents from normal distribution with mean 0 and variance 1
Toy<-sim.rasch(nvar=8, n=200, d=c(+2.0, +1.5, +1.0, +0.5, -0.5, -1.0, -1.5, -2.0))

# output is a list with binary 0 and 1 items in $items
ToyData<-Toy$items
colnames(ToyData)<-c("seat", "menu", "order", "prepare", "taste", "filling", "healthy","fresh")

# optional ordering of data matrix to see how pattern of 0s and 1s change with increasing total scores
item<-apply(ToyData,2,mean)
person<-apply(ToyData,1,sum)
ToyDataOrd<-ToyData[order(person),order(item)]
col_mean<-apply(ToyDataOrd,2,mean)*100
ToyDataOrd<-rbind(ToyDataOrd,col_mean)
row_sum<-apply(ToyDataOrd,1,sum)
ToyDataOrd<-cbind(ToyDataOrd,row_sum)
ToyDataOrd<-cbind(1:201,ToyDataOrd)
round(ToyDataOrd)

item<-apply(ToyData,2,mean)
person<-apply(ToyData,1,sum)
ToyDataOrd<-ToyData[order(person),order(item)]

library(gplots)
heatmap.2(ToyDataOrd, Rowv=FALSE, Colv=FALSE, 
          dendrogram="none", col=redblue(16), 
          key=T, keysize=1.5, density.info="none", 
          trace="none", labRow=NA)