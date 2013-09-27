# Description : Calculating distances (across matrices)
# Website : http://is-r.tumblr.com/post/32930447064/calculating-distances-across-matrices

# Cross-matrix distances and different measurement options, with "proxy"
doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("proxy", "MASS", "Zelig")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Invent two data frames
voterIdealPoints <- data.frame(matrix(rnorm(26*2), ncol = 2))
rownames(voterIdealPoints) <- letters

candidateIdealPoints <- data.frame(matrix(rnorm(3*2), ncol = 2))
rownames(candidateIdealPoints) <- LETTERS[1:3]

plot(rbind(voterIdealPoints, candidateIdealPoints), type = "n")
text(voterIdealPoints, rownames(voterIdealPoints), cex = 2/3)
text(candidateIdealPoints, rownames(candidateIdealPoints), col = "RED")

# Distance from each of the "many" observations to each of the "few" observations:
distanceMatrix <- proxy::dist(x = voterIdealPoints, y = candidateIdealPoints)
# I use the :: operator to distinguish from stats::dist
heatmap(distanceMatrix)

### A use for distance matrices ###
# Using the Mahalanobis distance metric
pr_DB$get_entry("Phi") # Read about it
# See: http://cran.r-project.org/web/packages/proxy/vignettes/overview.pdf
# for an extensive list of other distance metric options.

# Using a matrix of the votes of Supreme Court Justices
# see: ?SupremeCourt
data(SupremeCourt)
head(SupremeCourt)
distanceMatrix <- proxy::dist(t(SupremeCourt), method = "Phi")
heatmap(as.matrix(distanceMatrix))

# Run a metric multidimensional scaling on these distances
# Essentially reduces the n * n distance matrix to k dimensions.
MDS <- cmdscale(distanceMatrix, k = 2)

plot(MDS, type = "n", asp = 1)
text(MDS, rownames(MDS), cex = 2/3)