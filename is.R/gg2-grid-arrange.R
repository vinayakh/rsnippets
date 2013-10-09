# Description : GGtutorial: Day 2 - grid.arrange
# Website : http://is-r.tumblr.com/post/34628702940/ggtutorial-day-2-grid-arrange

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "gridExtra","reshape")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#Let's first define a function, SE, to represent the standard error of a
#variable:
library(gridExtra)
library(ggplot2)
library(reshape)
SE <- function(X) {
  (sd(X, na.rm=TRUE)/ sqrt(length(X)-1) )
}

ANES <- read.csv("data/ANES.csv")
head(ANES)
ANES$caseid <- 1:dim(ANES)[1]

TRUST <-melt(ANES, id=c("year", "dems"), na.rm=TRUE)
head(TRUST)
TRUST <- TRUST[TRUST$variable=="trust",]

#first thing I'll cast are the means of the "trust" variable over
#time, based on the respodnent's two-party identification

MEANS <- cast(TRUST, year+dems~variable, mean, na.rm=TRUE)
head(MEANS)

SEs <- cast(TRUST, year+dems~variable, SE)
head(SEs)

TRUST.plot <- cbind(MEANS$year,MEANS$dems,SEs$dems, MEANS$trust, SEs$trust,
                    (MEANS$trust - 1.96* SEs$trust), (MEANS$trust + 1.96* SEs$trust) )
TRUST.plot
TRUST.plot <- TRUST.plot[complete.cases(TRUST.plot),]

colnames(TRUST.plot) <- c("year", "dems", "dems.2", "means",
                          "SE", "lows", "highs")

DF <- as.data.frame(TRUST.plot)

T1 <- ggplot(data = DF) + geom_pointrange(aes(x = year, y = means,
                                              ymin = lows, ymax = highs, colour=factor(dems)) )
T1 <- T1 + labs(title="geom_pointrange, T1")
T1

# Part 2
Con1 <- ggplot(data = DF) + geom_pointrange(aes(x = year, y = means,
                                                ymin = lows, ymax = highs, colour=factor(dems)) )
Con1 <- Con1 + ylim(0.5,2.5) + labs(title="geom_pointrange")
Con1
Con2 <- ggplot(data = DF) + geom_errorbar(aes(x = year, y = means,
                                              ymin = lows, ymax = highs, colour=factor(dems), width=.5) )
Con2 <- Con2 + ylim(0.5,2.5) + labs(title="geom_errorbar, width=.5")
Con2
Con3 <- ggplot(data = DF) + geom_errorbar(aes(x = year, y = means,
                                              ymin = lows, ymax = highs, colour=factor(dems), width=.75), lty=2 )
Con3 <- Con3 + ylim(0.5,2.5) + labs(title="geom_errorbar, width=.75, lty=2")
Con3
Con4 <- ggplot(data = DF) + geom_errorbar(aes(x = year, y = means,
                                              ymin = lows, ymax = highs, colour=factor(dems)), size=1.5, lty=1 )
Con4 <- Con4 + ylim(0.5,2.5) + labs(title="geom_errorbar, size=1.5, lty=1")
Con4
MfrowGG <- grid.arrange(Con1, Con2, Con3, Con4, ncol=2)
