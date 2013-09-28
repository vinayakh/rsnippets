# Description : Using cairographics with ggsave()
# Website : http://is-r.tumblr.com/post/33421588764/using-cairographics-with-ggsave

# .png with Windows GDI versus .png with cairographics

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "RColorBrewer", "Cairo")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# Generate some data
nn <- 100
myData <- data.frame(X = rnorm(nn),
                     Y = rnorm(nn))
myData$Z <- with(myData, X * Y)
myData$Y <- myData$Y > 0

# This plot serves only to make a "hard" test case
# comparing Windows GDI to cairographics.
zp1 <- ggplot(myData, # \/ Here's a handy little function
              aes(x = X, fill = cut_number(Z, n = 10)))
zp1 <- zp1 + geom_abline(aes(intercept = X, slope = X), lwd = 1/5)
zp1 <- zp1 + geom_density(alpha = 2/3)
zp1 <- zp1 + theme_bw()
zp1 <- zp1 + facet_grid(~ Y)
zp1 <- zp1 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(10),
                               guide = "none")
print(zp1)

ggsave(plot = zp1, "Standard ggsave.png", h = 9/3, w = 16/3)
ggsave(plot = zp1, "Cairo ggsave.png", h = 9/3, w = 16/3, type = "cairo-png")
# This is the entire trick, type
# gets passed through ggsave...