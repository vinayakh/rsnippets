# Description : The Inverse Herfindahl–Hirschman Index as an "Effective Number of" Parties
# Website : http://is-r.tumblr.com/post/38140710276/the-inverse-herfindahl-hirschman-index-as-an-effective

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("plyr", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

ANES <- read.csv("data/ANES.csv")
head(ANES)
ANES$PID3 <- factor(ANES$pid7) # Convert to three-level Party ID:
levels(ANES$PID3) <- c("Dem", "Dem", "Dem", "Ind", "Rep", "Rep", "Rep")

# Using plyr to estimate the "Effective numbers of parties" by year and region
ENpid3 <- ddply(.data = ANES,
                .progress = "text",
                .variables = .(year, south),
                summarize, # Calculate an inverse HHI
                invHHI = sum(table(PID3))^2 / sum(table(PID3)^2))

zp1 <- ggplot(ENpid3)
zp1 <- zp1 + geom_line(aes(x = year, y = invHHI, colour = factor(south)))
zp1 <- zp1 + ggtitle("Effective Number of Parties-in-the-Electorate")
print(zp1)