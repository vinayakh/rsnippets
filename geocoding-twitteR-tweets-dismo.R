# Description : Geocoding ocation data with dismo
# Website : http://is-r.tumblr.com/post/38377477634/geocoding-location-data-with-dismo

doInstall <- TRUE
toInstall <- c("twitteR", "dismo", "maps", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# See http://decisionstats.com/2013/09/11/using-twitter-data-with-r/
# Twitter Auth setups on Linux & Windows are slightly different
# The Steps for Linux are shown below

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

# Search and plot portion start from here
searchTerm <- "#rstats"
searchResults <- searchTwitter(searchTerm, n = 1000) # Gather Tweets
tweetFrame <- twListToDF(searchResults) # Convert to a nice dF
userInfo <- lookupUsers(tweetFrame$screenName) # Batch lookup of user info
userFrame <- twListToDF(userInfo) # Convert to a nice dF

locatedUsers <- !is.na(userFrame$location) # Keep only users with location info
locations <- geocode(userFrame$location[locatedUsers]) # Use amazing API to guess

# approximate lat/lon from textual location data.
with(locations, plot(longitude, latitude))

worldMap <- map_data("world") # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap)
zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group), # Draw map
                       colour = gray(2/3), lwd = 1/3)
zp1 <- zp1 + geom_point(data = locations, # Add points indicating users
                        aes(x = longitude, y = latitude),
                        colour = "RED", alpha = 1/2, size = 1)
zp1 <- zp1 + coord_equal() # Better projections are left for a future post
zp1 <- zp1 + theme_minimal() # Drop background annotations
print(zp1)