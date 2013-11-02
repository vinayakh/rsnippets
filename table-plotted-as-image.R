# Description : Table as image in R
# Website : http://www.mollietaylor.com/2013/10/table-as-image-in-r.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("OIdata","gridExtra")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

data(birds)

# line breaks between words for levels of birds$effect:
levels(birds$effect) <- gsub(" ", "\n", levels(birds$effect))

xyTable <- table(birds$sky, birds$effect)

plot.new()
grid.table(xyTable,
           # change font sizes:
           gpar.coltext = gpar(cex = 1.2),
           gpar.rowtext = gpar(cex = 1.2))
