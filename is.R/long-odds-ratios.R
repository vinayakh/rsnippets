# Description : Log odds ratios and an indicator matrix from categorical data
# Website : http://is-r.tumblr.com/post/32865036594/log-odds-ratios-and-an-indicator-matrix-from

# Starting with categorical data, ending with a table of log odds ratios

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("plyr", "reshape2")
if(doInstall){install.packages(toInstall,
                               repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Canonical example of categorical data
HEC <- melt(HairEyeColor)
HEC <- HEC[rep(1:nrow(HEC), HEC[, 4]), -4]
colnames(HEC) <- c("Hair", "Eye", "Gender")
head(HEC) # This df has a row for each observation

# Convert matrix of factors to matrix of indicator variables
indicatorMatrix <- model.matrix(~ ., data = HEC,
                                contrasts.arg = sapply(HEC, contrasts, contrasts = FALSE))[, -1]
# (from http://stackoverflow.com/a/4569239/479554)
head(indicatorMatrix)

# Make a table of log odd ratios between categories
TT <- t(indicatorMatrix) %*% indicatorMatrix # Has both (two true)
TF <- t(indicatorMatrix) %*% !indicatorMatrix # Has one, but not other
FT <- t(!indicatorMatrix) %*% indicatorMatrix # etc.
FF <- t(!indicatorMatrix) %*% !indicatorMatrix # etc.

oddsRatios <- (TT / TF) / (FT / FF)
logOddsRatios <- log(oddsRatios)

arrange(melt(logOddsRatios)[melt(upper.tri(logOddsRatios))[, 3], ], value)
# upper.tri indicates which items in a matrix are in the upper triangle.