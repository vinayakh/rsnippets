# Description : Text Analysis with tm package
# Website : http://is-r.tumblr.com/post/37975717466/text-analysis-made-too-easy-with-the-tm-package

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("zoo", "tm", "ggplot2", "Snowball","SnowballC")
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

# Plot
zp1 <- ggplot(termCountFrame)
zp1 <- zp1 + geom_text(aes(x = Obama, y = Romney, label = Term))
print(zp1)