# Description : Scaling Legislative roll call votes with wnominate
# Website : http://is-r.tumblr.com/post/37110530260/scaling-legislative-roll-call-votes-with-wnominate

doInstall <- TRUE
toInstall <- c("wnominate", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Load most recent senate roll call data:
rollCall <- readKH("http://amypond.sscnet.ucla.edu/rollcall/static/S112.ord")

# Run wnominate on the roll call object
nDims <- 3
NOM <- wnominate(rcObject = rollCall, # The roll call object
                 dims = nDims, # Number of reduced dimensions
                 polarity = rep("PAUL (R KY)", nDims)) # A conservative

plot(NOM) # wnominate's own diagnostic plots

zp1 <- ggplot(NOM$legislators)
zp1 <- zp1 + geom_text(aes(x = coord1D, y = coord2D,
                           label = state, col = party))
zp1 <- zp1 + coord_equal()
zp1 <- zp1 + scale_colour_manual(values = c("BLUE", "GREEN", "RED"))
print(zp1)