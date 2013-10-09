# Description : Troubleshooting legends in ggplot2
# Website : http://is-r.tumblr.com/post/35557848297/troubleshooting-legends-in-ggplot

toInstall <- c("ggplot2", "reshape")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
library(ggplot2)
library(reshape)

ANES <- read.csv("data/ANES.csv")
party.and.region <- ANES[,c(1, 12, 16)]
party.and.region.year <- melt(party.and.region, id=c("year", "south"), na.rm=TRUE)
party.region.time <- (cast(party.and.region.year, year+south~variable, mean, na.rm=T) )


P0 <- ggplot(data = party.region.time
) + geom_point(aes(x = year,
                   y = pid7,
                   size=(1),
                   colour=as.factor(south),
                   shape=factor(south))) +theme_bw()

P0

#First thing: Make our symbols larger...


P1 <- P0 + scale_area(range = 6)

P1

#Step Two: Change Y-axis Limits:

P2 <- P1 + ylim(1,3.5)
P2
#Step Three: Add custom colors/breaks...
P3 <- P2 + scale_colour_manual(values=c("darkblue", "darkred"),
                               labels=c("Non-South", "South"))

P3 <- P3 + scale_shape_discrete(name ="Region",
                                labels=c("Non-South", "South"))

P3

#Now, we still have a few problems (some of which are self-inflicted)
# One obvious thing is to get two legends into one: Color + Shape into
#A legend with just a single red triangle and blue dot...

P4 <- P3 + guides(colour = "legend",
                  size = "none",
                  shape = "legend") + guides(colour = guide_legend("Region"),
                                             shape = guide_legend("Region"))

P4

#This could have been avoided from the beginning by not using
#as.factor() for one aesthetic and factor() for the other...



P5 <- P4 + guides(colour = guide_legend("Region",
                                        override.aes = list(size = 6)))

P5

#Now for one more change, adding axes labels and a title.

P6 <- P5 + labs(list(title = "Regional Partisanship 1952-2008",
                     x = "ANES Year",
                     y = "Mean Partisanship"))

P6

#Now, those labels might not look perfect.
#One approach is to simply add a hard return
#into the labels using \n:

P7 <- P6 + labs( list( title = "Regional Partisanship 1952-2008 \n",
                       x = "\n ANES Year",
                       y = "Mean Partisanship \n"))

P7