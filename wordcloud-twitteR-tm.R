# Description : Using Twitter Data with R #rstats
# Website : http://decisionstats.com/2013/09/11/using-twitter-data-with-r/

doInstall <- TRUE
toInstall <- c("twitteR","tm","wordcloud")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

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

a=searchTwitter("#rstats", n=2000) #Get the Tweets
tweets_df = twListToDF(a) #Convert to Data Frame

b=Corpus(VectorSource(tweets_df$text), readerControl = list(language = "eng"))
b<- tm_map(b, tolower) #Changes case to lower case 
b<- tm_map(b, stripWhitespace) #Strips White Space 
b <- tm_map(b, removePunctuation) #Removes Punctuation
inspect(b) 
tdm <- TermDocumentMatrix(b) 
m1 <- as.matrix(tdm) 
v1<- sort(rowSums(m1),decreasing=TRUE) 
d1<- data.frame(word = names(v1),freq=v1) 
wordcloud(d1$word,d1$freq)