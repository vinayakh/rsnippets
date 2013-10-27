# Description : Spiral Plot of discrete time plots
# Website : http://www.exegetic.biz/blog/2013/10/plotting-times-of-discrete-events/

toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
library(ggplot2)

spiral.length <- function(phi) {
  phi * sqrt(1 + phi**2) + log(phi + sqrt(1 + phi**2))
}

spiral.plot <- function(t, nturn = 5, colour = "black") {
  npoint = nturn * 720
  #
  curve = data.frame(phi = (0:npoint) / npoint * 2 * pi * nturn, r = (0:npoint) / npoint)
  curve = transform(curve,
                    arclen = spiral.length(phi),
                    x = r* cos(phi),
                    y = r * sin(phi))
  #
  points = data.frame(arclen = t * max(curve$arclen) / max(t))
  points = within(points, {
    phi = approx(curve$arclen, curve$phi, arclen, rule = 2)$y
    r = approx(curve$arclen, curve$r, arclen, rule = 2)$y
    x = r* cos(phi)
    y = r * sin(phi)
  })
  #
  ggplot(curve, aes(x = x, y = y)) +
    geom_path(colour = "grey") +
    geom_point(data = points, aes(x = x, y = y), size = 3, colour = colour) +
    coord_fixed(ratio = 1) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

Bq = 5
delay = rexp(2000, Bq)
decay = data.frame(delay, time = cumsum(delay))
spiral.plot(decay$time, 20)