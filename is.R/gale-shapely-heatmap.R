# Description : Gale-Shapley Deferred Acceptance in R
# Website : http://is-r.tumblr.com/post/33639308083/gale-shapley-deferred-acceptance-in-r

# Gale-Shapley matching
# From http://plausibel.blogspot.com/2012/01/illustrating-deferred-acceptance.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("source.gist", "animation")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Source the daa() function:
source.gist("1628636")

nOptions <- 15 # Although these matrices don't have to have the same dims
mPrefs <- t(replicate(nOptions, sample(1:nOptions, nOptions)))
wPrefs <- replicate(nOptions, sample(1:nOptions, nOptions))

heatmap(mPrefs, Rowv = NA, Colv = NA)
heatmap(wPrefs, Rowv = NA, Colv = NA)

galeShapleyResult <- daa(nMen = nOptions,
                         nWomen = nOptions,
                         m.prefs = mPrefs,
                         w.prefs = wPrefs)
print(galeShapleyResult)

matchMatrix <- 1*(matrix(rep(galeShapleyResult$matches, each=nOptions),
                         nrow=nOptions,
                         ncol=nOptions)==matrix(data=1:nOptions,
                                                nrow=nOptions,ncol=nOptions,byrow=F))

heatmap(1-matchMatrix, Rowv = NA, Colv = NA)