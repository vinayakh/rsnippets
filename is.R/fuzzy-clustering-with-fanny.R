# Description : Fuzzy Clustering with fanny
# Website : http://is-r.tumblr.com/post/37826141731/fuzzy-clustering-with-fanny

doInstall <- TRUE
toInstall <- c("ggplot2", "cluster", "MASS", "smacof")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate a matrix of dissimilarities from pairwise correlations
data(breakfast) # Preference orderings of breakfast items, from smacof
corrMat <- cor(breakfast, use = "pair")
distMat <- dist(corrMat)

# Fuzzy analysis clustering of breakfast items
FANNY <- fanny(distMat, k = 3, maxit = 2000)
FANNY$membership

MDS <- smacofSym(distMat)$conf

plot(MDS, type = "n")
text(MDS, label = rownames(MDS), col = rgb((FANNY$membership)^(1/1)))

# From http://stackoverflow.com/a/7661309/479554
labelFrame <- data.frame(X = MDS[, 1], Y = MDS[, 2], Label = rownames(MDS))
labelFrame <- transform(labelFrame,
                        w = strwidth(rownames(MDS))*1,
                        h = strheight(rownames(MDS))*1.5)
labelFrame$Color <- rgb((FANNY$membership)^(1/1))

zp1 <- ggplot(data = labelFrame,
              aes(x = X, y = Y))
zp1 <- zp1 + geom_rect(aes(xmin = X - w/2, xmax = X + w/2,
                           ymin = Y - h/2, ymax = Y + h/2,
                           fill = Color), alpha = 2/3)
zp1 <- zp1 + geom_text(aes(x = X, y = Y, label = Label),
                       size = 3, colour = "WHITE")
zp1 <- zp1 + scale_fill_identity()
zp1 <- zp1 + theme_classic()
print(zp1)