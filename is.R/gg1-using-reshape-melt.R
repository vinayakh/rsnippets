# Description : GGtutorial: Day 1 - using reshape()
# Website : http://is-r.tumblr.com/post/34556058683/ggtutorial-day-1-using-reshape

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2","reshape")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#Day 1/10: using Reshape
#Christopher DeSante, Ph.D.
ANES <- read.csv("data/ANES.csv")
head(ANES)
ANES$caseid <- 1:dim(ANES)[1]
head(ANES)
dim(ANES)
#Melting...
#Essentially reshapes the data into a new DF based on
#whichever variables you specify in the id="" option;
# ANES is ( 49760 x 17 )

#Which variables you decide to melt by should be dictated by which
#variables you wish to analyze. If we are only interested in time
#trends in the ANES, we can just melt by year:


#gist 2:
library(ggplot2)
library(gridExtra)
library(reshape)
anes.year <- melt(ANES, id=c("year"), na.rm=TRUE)
head(anes.year)

#anes.year has dimensions (571970 x 3):
#The dimensions are such that for every N variables in the ID option,
#you return a dataframe that has N+2 columns:


#If, for example, we want to see differences by year by gender,
#we can melt by year and 'female'

#gist 3:
anes.year.gender <- melt(ANES, id=c("year", "female"), na.rm=TRUE)
head(anes.year.gender)
dim(anes.year.gender)
#anes.year.gender has dimensions (522313 x 4)


#So, step one is to melt according to variables you plan on using;
#the next step is to CAST the data in such a way that extracts useful
#quantities that you can then use to plot/display/summarize.

############################################
# Example 1: Means of variables over time: #
############################################

#For simplicity's sake, let's truncate the data.frame so that
#it only includes measures of partisanship and ideology as well
#as the year of the survey.
party.and.ideology<- ANES[,c(1, 12, 14)]
head(party.and.ideology)

#Now, let's melt the data using "year" as the identifying (id) variable;

#Step 2, let's CAST this data so that we can get the means over time;
#specifically the means for Ideology and partisanship for each year
#that those questions were asked.

#Step 1 : Melt
party.and.ideology.year <- melt(party.and.ideology, id=c("year"), na.rm=TRUE)
head(party.and.ideology.year)

#Step 1 : Cast
party.and.ideology.means.over.time <- (cast(party.and.ideology.year,
                                            year~variable, mean, na.rm=T) )

party.and.ideology.means.over.time
dim(party.and.ideology.means.over.time)
# Now we can plot a relatively complex computation very easily.

P1 <- ggplot(data = party.and.ideology.means.over.time
) + geom_point(aes(x = year, y = pid7))
P1 <- P1 + ylim(0,6) + labs(title="geom_point, P1")
P1
#Again, what this code does is take a data.frame of dimensions
# 49760 by 17 and in just a few lines of code it returns a 28 x 3
#matrix of means sorted by years. The nice thing about reshape is
#it is not limited to just means, but nearly any aggregating function
#you may want to use. More soon...


#We can see that's a nice table we might find very useful,
#especially if we wanted to plot things out of that data.

#######################################################
# Example 2: Casting/Melting with two variables: #
#######################################################

# Imagine instead we want to look at how Partisanship
# has shifted over these years between the South/Non-south
# If we begin with just our ANES data frame, this might seem
# like quite a daunting task...
# However, if we know how to melt/cast appropriately, it is easier...
# First step, again, is to restrict our data to only the columns we
# want to utilize in our analysis. This isn't necessary, but let's
# keep things as simple as possible for now.

head(ANES)
party.and.region <- ANES[,c(1, 12, 16)]
head(party.and.region)
#Note the additional changes to our id=():
party.and.region.year <- melt(party.and.region, id=c("year", "south"), na.rm=TRUE)
head(party.and.region.year)
#Note the additional changes to our formula (below):
party.region.time <- (cast(party.and.region.year, year+south~variable, mean, na.rm=T) )
party.region.time
dim(party.region.time)
P2 <- ggplot(data = party.region.time) + geom_point(aes(x = year,
                                                        y = pid7, colour=as.factor(south)))
P2 <- P2 + ylim(0,6) + labs(title="geom_point, P2")

#Again, this allows us to get to a relatively complex set of summary statistics
#by some variableS of interest (year + region) in such a way that ggplot can
# easily handle them.



#######################################################
# Example 3: Casting/Melting with three variables: #
#######################################################

#Let's try the same type of plot, but instead of just looking at party and
#region, let's add gender to the mix...

head(ANES)
party.gender.region <- ANES[,c(1, 4, 12, 16)]
head(party.gender.region)
#Note the additional changes to our id=():
party.gender.region.year <- melt(party.gender.region, id=c("year", "female", "south"), na.rm=TRUE)
head(party.gender.region.year)
#Note the additional changes to our formula (below):
party.gender.region.time <- (cast(party.gender.region.year, year+female+south~variable, mean, na.rm=T) )
party.gender.region.time

#What party.gender.region.time represents is a matrix with four means per year,
# based on gender and region. So, for each year there's one for men in the south,
# men in the non-south, as well as women in each of the two regions.
#Throwing this into ggplot gives us a nice way to choose how to display this information.

#given some NAs for gender, I'm just going to clean these up quickly
#using complete.cases()
party.gender.region.time <- party.gender.region.time[complete.cases(party.gender.region.time),]

dim(party.gender.region.time)
P3 <- ggplot(data = party.gender.region.time) + geom_point(aes(x = year, y = pid7, colour=factor(south) )) + facet_grid(~female)
P3 <- P3 + ylim(0,6) + labs(title="geom_point, P3")
P3
