# Description : US Congressional ideology by state
# Website : http://is-r.tumblr.com/post/34288940225/congressional-ideology-by-state

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("foreign", "ggplot2", "plyr")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#get the data
dwNominate <- read.dta("ftp://voteview.com/junkord/HL01111E21_PRES.DTA")

#narrow data to cong>35
dwNominate = dwNominate[dwNominate$cong == 111 &
                          dwNominate$statenm != "USA", ]
# Recode / pool at-large state names
atLargeStates <- names(which(table(dwNominate$statenm) == 1))
dwNominate$statenm[dwNominate$statenm %in% atLargeStates] <- "At-large"
dwNominate$statenm <- factor(dwNominate$statenm,
                             with(dwNominate,
                                  names(sort(by(dwnom1, statenm, mean)))))

# Make a re-coded party variable
dwNominate$majorParty <- "Other"
dwNominate$majorParty[dwNominate$party == 100] <- "Democrat"
dwNominate$majorParty[dwNominate$party == 200] <- "Republican"
dwNominate$majorParty <- factor(dwNominate$majorParty,
                                c("Republican", "Democrat"))

backgroundData <- mutate(dwNominate, statenm = NULL)

head(dwNominate)
zp1 <- ggplot(data = dwNominate)
zp1 <- zp1 + geom_point(data = backgroundData,
                        aes(x = dwnom1, y = dwnom2),
                        colour = gray(2/3), alpha = 1/3)
zp1 <- zp1 + geom_point(aes(x = dwnom1, y = dwnom2, colour = majorParty))
zp1 <- zp1 + facet_wrap( ~ statenm, ncol = 4)
zp1 <- zp1 + scale_colour_brewer(palette = "Set1")
zp1 <- zp1 + theme_bw()
zp1 <- zp1 + scale_x_continuous(expand = c(0, 0))
zp1 <- zp1 + scale_y_continuous(expand = c(0, 0))
zp1 <- zp1 + theme(legend.position = "bottom")
print(zp1)