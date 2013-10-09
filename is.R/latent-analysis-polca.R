# Description : Latent Class analysis with Polca
# Website : http://is-r.tumblr.com/post/38707429332/latent-class-analysis-with-polca

doInstall <- TRUE
toInstall <- c("ggplot2", "poLCA", "reshape2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

ANES <- read.csv("data/ANES.csv")
ANES <- ANES[ANES$year == 2008, -c(1, 11, 17)] # Limit to just 2008 respondents,
head(ANES) # remove some non-helpful variables
# Adjust so that 1 is the minimum value for each variable:
ANES <- data.frame(apply(ANES, 2, function(cc){ cc - min(cc, na.rm = T) + 1 }))

# Estimate latent class model
lcFormula <- cbind(cohort, female, race6, religion, pid7, trust, ideo7, inerrant, south) ~ 1
lcModel <- poLCA(lcFormula, ANES, nclass = 4,
                 maxiter = 10000) # Make sure MAX LIKE is found.
plot(lcModel) # poLCA-style 3-D plot.

# Make a cleaner plot, first easily converting a list to a DF with melt():
lcModelProbs <- melt(lcModel$probs)

# Replicating the poLCA 3-D plot, without the 3-D:
zp1 <- ggplot(lcModelProbs,
              aes(x = L1, y = value, fill = X2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_wrap(~ X1)
print(zp1)

# Suggested alternative, as a possible improvement:
zp2 <- ggplot(lcModelProbs,
              aes(x = X1, y = value, fill = X2))
zp2 <- zp2 + geom_bar(stat = "identity", position = "stack")
zp2 <- zp2 + facet_wrap(~ L1)
zp2 <- zp2 + scale_x_discrete("Class", expand = c(0, 0))
zp2 <- zp2 + scale_y_continuous("Proportion", expand = c(0, 0))
zp2 <- zp2 + scale_fill_discrete("Factor Level")
zp2 <- zp2 + theme_bw()
print(zp2)