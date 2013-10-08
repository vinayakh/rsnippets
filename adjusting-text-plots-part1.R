# Description : Adjusting Plots part 1
# Website : http://is-r.tumblr.com/post/35699866752/textual-healing

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

#Plot.1 Now bcomes our basis plot to adjust:

# Adjusting Labels/Title Text:
# http://docs.ggplot2.org/current/element_text.html
#Adjust X Axis Label Size/Face/Color:
Plot.1 + theme(axis.title.x = element_text(face="bold",
                                           colour="dodgerblue",
                                           size=14) )

#Adjust Y Axis Label Size/Face/Color:
Plot.1 + theme(axis.title.y = element_text(face="italic",
                                           colour="darkred",
                                           size=24) )



#When adjusting multiple parameters, you can use specify both within the "theme" command:
Plot.1 + theme(axis.title.x = element_text(face="bold",
                                           colour="dodgerblue",
                                           size=14),
               axis.title.y = element_text(face="italic",
                                           colour="darkred",
                                           size=24) )


#Changing Main Title Text/Face/Color
Plot.1 + theme(plot.title = element_text(family="sans",
                                         face="bold",
                                         colour="darkblue",
                                         size=44) )

#Adjust just the text in the legend:
#You can adjust either the legend title text
Plot.1 + theme(legend.title = element_text(colour="dodgerblue",
                                           size=26,
                                           face="bold"))

# Or the text for the legend labels.
Plot.1 + theme(legend.text = element_text(colour="goldenrod4",
                                          size = 18,
                                          face = "italic"))

#Doing both simultaneously:
Plot.1 + theme(legend.title = element_text(colour="dodgerblue",
                                           size=26,
                                           face="bold"),
               legend.text = element_text(colour="goldenrod4",
                                          size = 18,
                                          face = "italic"))

#Another option in the element_text() is angle, which is useful in certain ocassions
#Especially when you have tight axis tick labels


Plot.1 + theme(axis.text.x = element_text(colour="black",
                                          size = 12,
                                          face = "plain"))


Plot.1 + theme(axis.text.x = element_text(colour="black",
                                          size = 11,
                                          face = "bold.italic",
                                          angle=45,
                                          vjust=1,
                                          hjust=1))