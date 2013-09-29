# Description : The distribution of ideology in the U.S. House (with plyr)
# Website : http://is-r.tumblr.com/post/33765462561/the-distribution-of-ideology-in-the-u-s-house-with

# Letting plyr do the work for us.
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("foreign", "plyr", "Hmisc", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# See: http://voteview.org/dwnominate.asp
dwNominate <- read.dta("ftp://voteview.com/junkord/HL01111E21_PRES.DTA")

# Make a re-coded party variable
dwNominate$majorParty <- "Other"
dwNominate$majorParty[dwNominate$party == 100] <- "Democrat"
dwNominate$majorParty[dwNominate$party == 200] <- "Republican"
head(dwNominate)

# Letting plyr do the work for us (most of the functions are from Hmisc)
aggregatedIdeology <- ddply(.data = dwNominate,
                            .variables = .(cong, majorParty),
                            .fun = summarise, # Allows the following:
                            Median = wtd.quantile(dwnom1, 1/bootse1, 1/2),
                            q25 = wtd.quantile(dwnom1, 1/bootse1, 1/4),
                            q75 = wtd.quantile(dwnom1, 1/bootse1, 3/4),
                            q05 = wtd.quantile(dwnom1, 1/bootse1, 1/20),
                            q95 = wtd.quantile(dwnom1, 1/bootse1, 19/20),
                            N = length(dwnom1),
                            .progress = "text") # Because we can.
aggregatedIdeology$majorParty <- factor(aggregatedIdeology$majorParty,
                                        levels = c("Republican", "Democrat", "Other"))
head(aggregatedIdeology) # All of our stats, calculated "by" our .variables

# Neat, simple, clean plot of ideological distributions
# Neat, simple, clean plot of ideological distributions
zp1 <- ggplot(aggregatedIdeology,
              aes(x = cong, y = Median,
                  ymin = q05, ymax = q95,
                  colour = majorParty, alpha = N))
zp1 <- zp1 + geom_linerange(aes(ymin = q25, ymax = q75), # Plot the 90% CI
                            size = 1) # it inherits x, y, colour and alpha
zp1 <- zp1 + geom_pointrange(size = 1/2) # Plot the IQR
zp1 <- zp1 + scale_colour_brewer(palette = "Set1")
zp1 <- zp1 + theme_bw()
print(zp1)