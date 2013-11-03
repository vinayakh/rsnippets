# Description : Anand Vs Carlsen matchup analysis
# Website : http://datadoodlers.blogspot.in/2013/10/anand-versus-carlsen-chennai-2013-what.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("reshape2","ggplot2","plyr")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

df <- read.csv("data/Anand-Carlsen.csv", stringsAsFactors=F, strip.white=T)
df <- arrange(df, Game) #use plyr's arrange to Sort by Game Number

elo <- read.csv("data/Anand-Carlsen-ELO.csv", stringsAsFactors=F, strip.white=T)
elo$Period <- as.Date(paste0(elo$Period, "-01"), format="%Y-%b-%d") #convert from string to Date format
#names(elo)

names(df)
head(df)

#For ease drop some columns we don't need right away
df <- df[, c(1,2,3, 5,7,8)]

#Who had White? Create a new column
df$Anand.white <- ifelse(df$WhiteVsBlack == "Anand vs Carlsen", 1, 0)

#Create to new column for the Winner.
df$Anand.won <- 0 # we will be overwriting this column in instances where Anand Won
for (g in 1:nrow(df)) {
  if(df$Result[g] == "Draw") {df$Anand.won[g] <- "Draw"}
  else if ((df$Anand.white[g]==1 & df$Result[g]==1) || (df$Anand.white[g]==0 & df$Result[g]==0)) {
    df$Anand.won[g] <- 1
  }
}




# Lifetime Scores
table(df$Anand.won)
nrow(df)

AW <- subset(df, Anand.white==1)
table(AW$Anand.won)

AB <- subset(df, Anand.white==0)
table(AB$Anand.won)


#####

### ELO Ratings
m.elo <- melt(elo[,c(1,2,4)], id=c("Period"), na.rm=T, value.name="Rating")
p <- NULL
p <- ggplot(data=m.elo, aes(Period, y=Rating)) + geom_line(aes(group=variable, color=variable), size=2)
p <- p + geom_text(data=m.elo[68, ], label="Anand", vjust=2, size=8)
p <- p + geom_text(data=m.elo[130, ], label="Carlsen", hjust=1, size=8) + theme(legend.position="none")
p <- p + labs(title="Anand vs Carlsen - ELO Ratings Over Time")
p


#' Given a data-frame slice, this function compute WLD for that time slice
#' This function is useful to call from ddply
WinLoseDraw <- function(df) {
  va.wins <- sum(na.omit(df$Anand.won == "1")) #how many times did Anand win
  va.loss <- sum(na.omit(df$Anand.won == "0")) #how many times did Anand Lose
  va.draw <- sum(na.omit(df$Anand.won == "Draw")) #how many times did Anand Draw
  c(va.wins, va.loss, va.draw)
}

### Win Loss Draw Distributions by Year
WLD <- ddply(df, .(Year), WinLoseDraw)
names(WLD) <- c("Year", "Win", "Loss", "Draw")
m.wld <- melt(WLD, id="Year")

vlines <- seq(2005.5, 2013.5, by=1)
pw <- ggplot(data=m.wld, aes(x=Year, y=value, fill=variable)) + geom_bar(stat="identity", position=position_dodge())
pw <- pw + scale_fill_manual(values=c("darkgreen", "red", "lightblue"))
pw <- pw + geom_vline(xintercept = vlines, color="darkred", size=0.3)
pw <- pw + scale_x_continuous(breaks=seq(2005, 2013))
pw <- pw + labs(title="Anand's Win-Loss-Draw by Year")+ theme(panel.background = element_blank())
pw
WLD

# 4. Consequence of CHOICE OF OPENINGS

#Creating a new column called win just to code the values as numeric
# This column is the same as Anand.won
df$win <- ifelse(df$Anand.won=="Draw", 0.5,
                 ifelse(df$Anand.won=="1", 1, 0)
)

wb_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Anand.white") {
    value[value==1] <- "Anand.White"
    value[value==0] <- "Anand.Black"
  }
  return(value)
}


#Sort the data frame by Openings that helped win (or lose)
df <- transform(df, ECO = reorder(ECO, win))

#Now we are ready to plot...
po <- NULL
po <- ggplot(data=df, aes(x=(ECO))) + geom_bar(aes(fill=Anand.won)) + coord_flip()
po <- po + labs(title="Consequence of Choice of Openings") + ylab("Number of Games") + xlab("Opening Chosen")
po

#po <- po + facet_grid(. ~ Anand.white, labeller = wb_labeller)
po <- po + geom_text(aes(y=3,label=df$Opening), size=4)
po <- po + annotate("text", x=df$ECO, y=2, label=df$Opening, size=3)
po