# Description : Adding a background to your ggplot
# Website : http://is-r.tumblr.com/post/33886259146/adding-a-background-to-your-ggplot

# Adding regions to the background of a ggplot
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("foreign", "plyr", "ggplot2", "RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# House DW-NOMINATE Data: http://voteview.org/dwnominate.asp
dwNominate <- read.dta("ftp://voteview.com/junkord/HL01111E21_PRES.DTA")

# Make a re-coded party variable
dwNominate$majorParty <- "Other"
dwNominate$majorParty[dwNominate$party == 100] <- "Democrat"
dwNominate$majorParty[dwNominate$party == 200] <- "Republican"
dwNominate$Year <- (dwNominate$cong - 1) * 2 + 1789
head(dwNominate)

# This makes a data.frame of the plurality party for each Congressional year
majorityParty <- ddply(dwNominate,
                       .(Year),
                       function(df){
                         names(sort(-table(df$majorParty)))[1]
                       })
colnames(majorityParty)[2] <- "majorParty" # These two lines ensure that the
majorityParty$dwnom1 <- 1 # majorityParty frame has all of the same colnames
# as we're using from dwNominate in the plot below, even if
# majorityParty$dwnom1 is just a placeholder equal to 1.

myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

zp1 <- ggplot(data = dwNominate, # The "foreground" data
              aes(x = Year, y = dwnom1))
zp1 <- zp1 + geom_rect(data = majorityParty, # The "background" data
                       aes(xmin = Year, # and aesthetics...
                           xmax = Year + 2,
                           fill = majorParty),
                       ymin = -2, ymax = 2,
                       alpha = 2/3)
zp1 <- zp1 + geom_jitter(size = 1/2) # The "foreground" geom
zp1 <- zp1 + theme_bw()
zp1 <- zp1 + ylim(-1, 1)
zp1 <- zp1 + scale_x_continuous(expand = c(0, 0))
zp1 <- zp1 + scale_fill_manual(values = myPalette(9)[c(8, 5, 2)])
print(zp1)