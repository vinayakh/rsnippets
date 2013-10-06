# Description : Adjusting Plots part 2
# Website : http://is-r.tumblr.com/post/35769073499/textual-healing-part-ii

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

Plot.1 <- Plot.0 + scale_y_continuous(breaks=seq(-4, 24, by=4))
Plot.1


Plot.1 + theme(strip.text.x = element_text(size=16, face="bold") ,
               strip.background = element_rect(fill="yellow")
) + theme(axis.text.x = element_text(colour="black",
                                     size = 11,
                                     face = "bold.italic",
                                     angle=45,
                                     vjust=1,
                                     hjust=1))


#Now, what if we wanted I, II and III to be meaningful; perhaps I corresponds to
#One half dose, II to Five CCs and III to 'A lot'... I know this isn't a great choice of specific labels and
#we could just have gone back and recreated the data from scratch. This is intentionally done to show how to overcome
#another problem. Below we create a factor variable that does this and then plot the result.
text.plots$DOSE2 <- c()
text.plots$DOSE2[text.plots$DOSE=="I"] <- "One Half Dose"
text.plots$DOSE2[text.plots$DOSE=="II"] <- "Five CCs"
text.plots$DOSE2[text.plots$DOSE=="III"] <- "A Lot"


Plot.2 <- ggplot( text.plots, aes( x = SUPP, y = LENGTH , fill = SUPP ) ) +
  geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5
  ) + scale_fill_manual(values=c("cadetblue", "orange", "orangered3")) +
  facet_wrap(~DOSE2) + theme_bw() +labs(title="Tooth Growth in Guinea Pigs \n",
                                        x="\n Treatment", y="Change in Length (mm) \n") + guides(fill = guide_legend("\n Supplement")) +geom_hline(y=0, lty=2)

Plot.3 <- Plot.2 + scale_y_continuous(breaks=seq(-4, 24, by=4))
Plot.3

#Reorder Factor:
text.plots$DOSE2 <- factor(text.plots$DOSE2)
text.plots$DOSE2 <- factor(text.plots$DOSE2, levels= levels(text.plots$DOSE2)[c(3,2,1)])

Plot.2 <- ggplot( text.plots, aes( x = SUPP, y = LENGTH , fill = SUPP ) ) +
  geom_boxplot( alpha = 0.6, outlier.colour = c("grey40") , outlier.size=3.5
  ) + scale_fill_manual(values=c("cadetblue", "orange", "orangered3")) +
  facet_wrap(~DOSE2) + theme_bw() +labs(title="Tooth Growth in Guinea Pigs \n",
                                        x="\n Treatment", y="Change in Length (mm) \n") + guides(fill = guide_legend("\n Supplement")) +geom_hline(y=0, lty=2)

Plot.3 <- Plot.2 + scale_y_continuous(breaks=seq(-4, 24, by=4))

Plot.3 + theme(strip.text.x = element_text(size=16, face="bold") ,
               strip.background = element_rect(fill="yellow")
) + theme(axis.text.x = element_text(colour="black",
                                     size = 11,
                                     face = "bold.italic",
                                     angle=45,
                                     vjust=1,
                                     hjust=1))