# Description : Custom Axis breaks
# Website : http://is-r.tumblr.com/post/35630915298/cant-a-plot-catch-a-break-s

toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
library(ggplot2)

text.plots <- data.frame(
  SUPP = c (rep(c( "Control", "Vitamin C", "Orange Juice" ), each = 1500)) ,
  DOSE = rep(rep(c( "I", "II", "III" ), each=500), 3),
  LENGTH = c(rnorm(500, 4, 2),
             rnorm(500, 7, 1.2),
             rnorm(500, 8, 1.4),
             rnorm(500, 7, 1),
             rnorm(500, 8, 4),
             rnorm(500, 10, 2),
             rnorm(500, 8, 2.7),
             rnorm(500, 7, 1.8),
             rnorm(500, 6, 1.9)), stringsAsFactors = FALSE )

Plot.0 <- ggplot( text.plots, aes( x = SUPP, y = LENGTH , fill = SUPP ) ) +
  geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5
  ) + scale_fill_manual(values=c("cadetblue", "orange", "orangered3")) +
  facet_wrap(~DOSE) + theme_bw() +labs(title="Tooth Growth in Guinea Pigs \n",
                                       x="\n Treatment", y="Change in Length (mm) \n") + guides(fill = guide_legend("\n Supplement")) +geom_hline(y=0, lty=2)
Plot.0
#Things we may
#want to do:
#1. Change Y-Axis Breaks:
Plot.0 + scale_y_continuous(breaks=seq(-4, 22, by=2))
Plot.0 + scale_y_continuous(breaks=c(-4, -2, 0, 5:22))
Plot.0 + scale_y_continuous(breaks=c(-4, -2, 0, 5, 8, 14:22))
Plot.1 <- Plot.0 + scale_y_continuous(breaks=seq(-4, 24, by=4))
Plot.1