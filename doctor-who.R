# Description : Changes in Doctor Who Characters.
# Website : http://4dpiecharts.com/2013/08/03/the-tenure-of-doctor-who-incarnations/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE

library(ggplot2)

whos <- data.frame(
  doctor = c("William Hartnell", "Patrick Troughton", "Jon Pertwee", "Tom Baker", "Peter Davison", "Colin Baker", "Sylvester McCoy", "Paul McGann", "Christopher Ecclestone", "David Tennant", "Matt Smith"),
  n_episodes = c(136, 127, 129, 173, 70, 35, 42, 1, 13, 49, 44),
  stringsAsFactors = FALSE
)
whos$doctor <- factor(whos$doctor, levels = whos$doctor)
ggplot(whos, aes(doctor, n_episodes)) +   geom_bar(stat = "identity") + coord_flip()