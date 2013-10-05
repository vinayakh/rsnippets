# Description : Draw a multicolored wordcloud using twitter terms
# Website : http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/

doInstall <- TRUE
toInstall <- c("twitteR","tm","wordcloud","RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# See the twitter setup procedure at 
# http://decisionstats.com/2013/09/11/using-twitter-data-with-r/
# Unfortunately the authprocess is not completely automated and require human intervention

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "redacted"
consumerSecret <- "redacted"

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake() #Pause here for the Handshake Pin Code
registerTwitterOAuth(twitCred) #Wait till you see True

# Searching and plotiing
bioinformatics <- searchTwitter("#bioinformatics", n=1500)
bioinformatics_text <- sapply(bioinformatics, function(x) x$getText())
bioinformatics_text_corpus <- Corpus(VectorSource(bioinformatics_text))
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, tolower)
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, removePunctuation)
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(bioinformatics_text_corpus)

# Reduce words and add color
pal2 <- brewer.pal(8,"Dark2")
wordcloud(bioinformatics_text_corpus,min.freq=2,max.words=100, random.order=T, colors=pal2)