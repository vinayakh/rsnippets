# Description : Plotting letters as shapes in ggplot2
# Website : http://is-r.tumblr.com/post/35050025650/plotting-letters-as-shapes-in-ggplot2

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("foreign", "ggplot2", "devtools","source.gist")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

source.gist("https://gist.github.com/818983")
source.gist("https://gist.github.com/818986")
source.gist("https://gist.github.com/818998")

# Get NOMINATE data from voteview.org
dwNominate <- read.dta("ftp://voteview.com/junkord/HL01111E21_PRES.DTA")
partyTable <- readLines("http://voteview.org/party3.htm")
partyTable <- partyTable[(lineFinder("PRE>", partyTable)[1]+1):
                           (lineFinder("PRE>", partyTable)[2]-1)] # Merge party names...
partyNumber <- as.numeric(substr(partyTable, 1, 7))
partyTable <- spaceTrim(substr(partyTable, 9, 1000))
names(partyTable) <- partyNumber
dwNominate$partyName <- partyTable[as.character(dwNominate$party)]

# Choose a Congress with a lot of parties:
dwNominate <- dwNominate[dwNominate$cong == 18 &
                           dwNominate$statenm != "USA", ] # 77 is also a good example.

zp1 <- ggplot(dwNominate,
              aes(x = dwnom1, y = dwnom2,
                  shape = partyName, colour = partyName))
zp1 <- zp1 + geom_point()
zp1 <- zp1 + scale_colour_brewer(palette = "Paired")
zp1 <- zp1 + theme_bw()
print(zp1) # So, lots of different shapes
# In fact, ggplot2 resists plotting more than 6 shapes.

# Denote parties by their initial letter.
dwNominate$partyInitial <- substr(dwNominate$partyName, 1, 1)

zp2 <- ggplot(dwNominate,
              aes(x = dwnom1, y = dwnom2,
                  label = partyInitial, colour = partyName))
zp2 <- zp2 + geom_text(size = 3)
zp2 <- zp2 + scale_colour_brewer(palette = "Paired")
zp2 <- zp2 + theme_bw()
print(zp2) # Initials in the plot, but "a" in the legend

# Convert from letters to UTF-8 encoded "shapes"
uniqueInitials <- c("a", "A", "c", "C", "j", "J", "R")
initialShapes <- unlist(lapply(uniqueInitials, utf8ToInt))

zp3 <- ggplot(dwNominate,
              aes(x = dwnom1, y = dwnom2,
                  shape = partyName, colour = partyName))
zp3 <- zp3 + geom_point(size = 3)
zp3 <- zp3 + scale_colour_brewer(palette = "Paired")
zp3 <- zp3 + theme_bw() # \/ Manually select letter-shapes
zp3 <- zp3 + scale_shape_manual(values = initialShapes)
print(zp3) # Initials in the plot and in the legend!