# Description : Using XML to grab tables from the web
# Website : http://is-r.tumblr.com/post/36945206190/using-xml-to-grab-tables-from-the-web

doInstall <- TRUE
toInstall <- c("XML")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

myURL <- "http://en.wikipedia.org/wiki/United_States_presidential_election,_2012"

allTables <- readHTMLTable(myURL)
str(allTables) # Look at the allTables object to find the specific table we want
stateTable <- allTables[[14]] # We want the 14th table in the list (maybe 13th?)
head(stateTable)

# Clean up:
stateTable <- stateTable[1:(nrow(stateTable)-2), ] # Drop summary lines
stateTable$State <- do.call(rbind, strsplit(as.character(stateTable$State), "\\["))[, 1]
stateTable$State[stateTable$State == "District of ColumbiaDistrict of Columbia"] <- "District of Columbia"
whichAreNumeric <- colMeans(apply(stateTable, 2, function(cc){
  regexpr(",", cc) != -1})) > 0
stateTable[, whichAreNumeric] <- apply(stateTable[, whichAreNumeric], 2, function(cc){
  as.numeric(gsub(",", "", cc))})

# Display in order of Obama's proportion of the vote:
stateTable[, c("State", "Obama", "Romney")][with(stateTable, order(Obama/Total)), ]