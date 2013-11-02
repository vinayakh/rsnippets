# Description : Using the Google Scholar Package
# Website : http://www.jameskeirstead.ca/blog/new-r-package-scholar/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("scholar","ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

id <- 'B7vSqZsAAAAJ'
feynman <- get_profile(id)
feynman$name # Prints out his name

# Compare Richard Feynman and Stephen Hawking
ids <- c('B7vSqZsAAAAJ', 'qj74uXkAAAAJ')

# Compare their career trajectories, based on year of first citation
df <- compare_scholar_careers(ids)
ggplot(df, aes(x=career_year, y=cites)) + geom_line(aes(linetype=name)) + theme_bw()

## Predict Daniel Acuna's h-index
id <- 'GAi23ssAAAAJ'
predict_h_index(id)