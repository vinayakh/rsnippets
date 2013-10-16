# Description : Text analysis with lme4
# Website : http://is-r.tumblr.com/post/38055963968/possibly-slightly-better-text-analysis-with-lme4

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("zoo", "tm", "ggplot2", "lme4", "arm", "Snowball","SnowballC")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# From: http://www.cnn.com/2012/10/03/politics/debate-transcript/index.html
Transcript <- readLines("data/Denver_Debate_Transcript.txt")
head(Transcript, 20)

Transcript <- data.frame(Words = Transcript, Speaker = NA, stringsAsFactors = FALSE)
Transcript$Speaker[regexpr("LEHRER: ", Transcript$Words) != -1] <- 1
Transcript$Speaker[regexpr("OBAMA: ", Transcript$Words) != -1] <- 2
Transcript$Speaker[regexpr("ROMNEY: ", Transcript$Words) != -1] <- 3
table(Transcript$Speaker)
Transcript$Speaker <- na.locf(Transcript$Speaker)

# Remove moderator:
Transcript <- Transcript[Transcript$Speaker != 1, ]

myCorpus <- Corpus(DataframeSource(Transcript))
inspect(myCorpus)

myCorpus <- tm_map(myCorpus, tolower) # Make lowercase
myCorpus <- tm_map(myCorpus, removePunctuation, preserve_intra_word_dashes = FALSE)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english")) # Remove stopwords
myCorpus <- tm_map(myCorpus, removeWords, c("lehrer", "obama", "romney"))
myCorpus <- tm_map(myCorpus, stemDocument) # Stem words

inspect(myCorpus)
docTermMatrix <- DocumentTermMatrix(myCorpus)

docTermMatrix <- inspect(docTermMatrix)
sort(colSums(docTermMatrix))
table(colSums(docTermMatrix))

termCountFrame <- data.frame(Term = colnames(docTermMatrix))
termCountFrame$Obama <- colSums(docTermMatrix[Transcript$Speaker == 2, ])
termCountFrame$Romney <- colSums(docTermMatrix[Transcript$Speaker == 3, ])

head(termCountFrame)

### New ###

tallCountFrame <- with(termCountFrame, data.frame(Term = c(rep(Term, Obama),
                                                           rep(Term, Romney))))
tallCountFrame$isRomney <- rep(c(0, 1), colSums(termCountFrame[, -1]))
tallCountFrame$Term <- colnames(docTermMatrix)[tallCountFrame$Term]

randomInterceptModel <- lmer(isRomney ~ (1 | Term) - 1,
                             family = "binomial", data = tallCountFrame)

# Convert lmer model to plot-able data.
coefficientFrame <- data.frame(Term = rownames(coef(randomInterceptModel)$Term))
coefficientFrame$Estimate <- coef(randomInterceptModel)$Term[, 1]
coefficientFrame$SE <- se.coef(randomInterceptModel)$Term[, 1]

coefficientFrame$Count <- colSums(docTermMatrix)[coefficientFrame$Term]
coefficientFrame$z0 <- with(coefficientFrame, plogis(Estimate))
coefficientFrame$z_1 <- with(coefficientFrame, plogis(Estimate-SE))
coefficientFrame$z1 <- with(coefficientFrame, plogis(Estimate+SE))
coefficientFrame$z_2 <- with(coefficientFrame, plogis(Estimate-2*SE))
coefficientFrame$z2 <- with(coefficientFrame, plogis(Estimate+2*SE))

coefficientFrame$Term <- factor(coefficientFrame$Term,
                                levels = coefficientFrame$Term[order(coefficientFrame$Estimate)])

cutoffCount <- tail(sort(colSums(docTermMatrix)), 100)[1]
zp1 <- ggplot(coefficientFrame[coefficientFrame$Count >= cutoffCount, ],
              aes(x = Term, y = z0,
                  ymin = z_2, ymax = z2))
zp1 <- zp1 + geom_linerange(size = 1/2)
zp1 <- zp1 + geom_linerange(aes(ymin = z_1, ymax = z1),
                            size = 1)
zp1 <- zp1 + geom_point(colour = "WHITE", shape = 15, alpha = 1, size = 10/9)
zp1 <- zp1 + scale_y_continuous("Romney use probability",
                                expand = c(0, 0))
zp1 <- zp1 + coord_flip()
zp1 <- zp1 + ggtitle(paste("p(Romney Said It | Term)\nterms that occur at least ", cutoffCount, " times", sep = ""))
zp1
