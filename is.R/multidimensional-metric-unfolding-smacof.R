# Description : Multidimensional Metric unfolding with smacof
# Website : http://is-r.tumblr.com/post/37782370200/multidimensional-metric-unfolding-with-smacof

doInstall <- TRUE
toInstall <- c("ggplot2", "smacof")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

ANES <- read.csv("data/2008_ANES_Thermometers.csv")
head(ANES)

# Multidimensional scaling of a rectangular thermometer rating matrix
thermometerFrame <- ANES[, -c(1:2)]
allThermsMissing <- rowMeans(is.na(thermometerFrame)) == 1
ANES <- ANES[!allThermsMissing, ] # Need to remove any all-NA rows
thermometerFrame <- thermometerFrame[!allThermsMissing, ]

MDS <- smacofRect(thermometerFrame)
plot(MDS)
ANES$D1 <- -MDS$conf.row[, 1]
ANES$D2 <- MDS$conf.row[, 2]

# Compare Republicans and Democrats:
zp1 <- ggplot(ANES)
zp1 <- zp1 + geom_point(aes(x = D1, y = D2,
                            colour = PID3, shape = PID3))
zp1 <- zp1 + scale_colour_manual(values = hsv(2:0/3, 1, 1))
print(zp1)

zp2 <- ggplot(ANES)
zp2 <- zp2 + geom_density(aes(x = D1, colour = factor(PID3),
                              fill = factor(PID3)), alpha = 1/3)
zp2 <- zp2 + scale_colour_manual(values = hsv(2:0/3, 1, 1))
zp2 <- zp2 + scale_fill_manual(values = hsv(2:0/3, 1, 1))
print(zp2)

# Make a more useful x-axis:
whichStimuli <- c("barackObama", "democraticParty", "liberals", "johnMcCain",
                  "theUSSupremeCourt", "conservatives", "republicanParty")
xBreaks <- MDS$conf.col[, 1]
names(xBreaks)[!names(xBreaks) %in% whichStimuli] <- ""

zp3 <- zp2 + scale_x_continuous(breaks = xBreaks, labels = names(xBreaks))
zp3 <- zp3 + scale_y_continuous(expand = c(0, 0))
zp3 <- zp3 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1))
print(zp3)