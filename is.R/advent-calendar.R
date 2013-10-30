# Description : Draw a Calendar of dates
# Website : http://is-r.tumblr.com/post/37687431723/can-you-please-post-the-r-code-for-making-that

toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
#new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")

for(dd in 1:24){
  myDF <- data.frame(advDay = 1:24)
  myDF$x = rep(1:6, 4)
  myDF$y = rep(4:1, each = 6)
  myDF$col = sample(1:24)
  zp1 <- ggplot(myDF)
  zp1 <- zp1 + geom_point(aes(x = x, y = y,
                              colour = factor(col)),
                          fill = "WHITE",
                          size = 24)
  zp1 <- zp1 + new_theme_empty + coord_equal()
  zp1 <- zp1 + scale_colour_discrete(guide = "none")
  zp1 <- zp1 + scale_x_continuous(expand = c(1/5, 1/5))
  zp1 <- zp1 + scale_y_continuous(expand = c(1/5, 1/5))
  zp1 <- zp1 + ggtitle("Advent CalendaR")
  zp1 <- zp1 + geom_text(aes(x=x, y=y, label = advDay),
                         size = 11,
                         colour = "WHITE")
  if(dd < 24){
    zp1 <- zp1 + geom_point(data = myDF[-c(1:dd), ],
                            aes(x = x, y = y,
                                colour = factor(col)),
                            fill = "WHITE",
                            size = 21,
                            shape = 21)
    zp1 <- zp1 + geom_text(data = myDF[-c(1:dd), ],
                           aes(x = x, y = y, label = advDay,
                               colour = factor(col)),
                           size = 11)
  }
  print(zp1)
  
  #setwd()
  #ggsave(paste("Day ", formatC(dd, width = 2, format = "d", flag = "0") , ".png", sep = ""), zp1, h = 9/2, w = 16/2, type = "cairo-png")
} 
