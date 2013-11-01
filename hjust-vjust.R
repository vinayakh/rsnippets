# Description : Horizontal and Vertical justification on plots
# Website : http://is-r.tumblr.com/post/35835735829/hjust-and-vjust

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

hjustvjust <- expand.grid(
  hjust=c(0, 0.5, 1),
  vjust=c(0, 0.5, 1),
  angle=c(0, 45, 90),
  text="is-R"
)

hv <- ggplot(hjustvjust, aes(x=hjust, y=vjust)) +
  geom_point() +
  geom_text(aes(label=text, angle=angle, hjust=hjust, vjust=vjust)) +
  facet_grid(~angle) +
  scale_x_continuous(breaks=c(0, 0.5, 1), expand=c(0, 0.2)) +
  scale_y_continuous(breaks=c(0, 0.5, 1), expand=c(0, 0.2)) + theme_bw()

hv + labs(title="Various values of angle, hjust and vjust in ggplot \n", y ="vjust \n", x =" \n hjust") +
  theme(axis.text.x = element_text(colour="black",
                                   size = 12,
                                   face = "bold") ,
        axis.text.y = element_text(colour="black",
                                   size = 12,
                                   face = "bold"),
        axis.title.x = element_text(face="plain",
                                    colour="black",
                                    size=24),
        axis.title.y = element_text(face="plain",
                                    colour="black",
                                    size=24),
        plot.title = element_text(face="bold",
                                  colour="black",
                                  size=18))