# Description : Percolation Threshold in a square lattice
# Website : httphttp://www.exegetic.biz/blog/2013/10/percolation-threshold-on-a-square-lattice/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("reshape","ggplot2","plyr","gridExtra","foreach","doMC")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

EMPTY    = 0
OCCUPIED = 1
FLOW     = 2

create.grid <- function(N, p) {
  grid = matrix(rbinom(N**2, 1, p), nrow = N)
  attributes(grid)$p = p
  return(grid)
}

set.seed(1)
g1 = create.grid(12, 0.6)
g2 = create.grid(12, 0.4)

visualise.grid <- function(g) {
  N = nrow(g)
  limits = c(0, N) + 0.5
  ggplot(melt(g[nrow(g):1,], 1, varnames = c("row", "col"), value.name = "occupied")) +
  geom_tile(aes(x = col, y = row, fill = factor(occupied))) +
  geom_hline(yintercept = limits, size = 2) +
  geom_vline(xintercept = limits, size = 2) +
  xlim(limits) + ylim(limits) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("0" = "white", "1" = "grey", "2" = "lightblue")) +
  theme(panel.background = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none")
}

grid.arrange(visualise.grid(g1), visualise.grid(g2), ncol = 2)

flow <- function(g, i = NA, j = NA) {
# -> Cycle through cells in top row
  if (is.na(i) || is.na(j)) {
    for (j in 1:ncol(g)) {
      g = flow(g, 1, j)
    }
    return(g)
  }

  # -> Check specific cell
  if (i < 1 || i > nrow(g) || j < 1 || j > ncol(g)) return(g)
  if (g[i,j] == OCCUPIED || g[i,j] == FLOW) return(g)
  g[i,j] = FLOW
  g = flow(g, i+1, j)        # down
  g = flow(g, i-1, j)        # up
  g = flow(g, i, j+1)        # right
  g = flow(g, i, j-1)        # left
  g
}

grid.arrange(visualise.grid(flow(g1)), visualise.grid(flow(g2)), ncol = 2)

# Check whether flow reaches to bottom of the lattice.

percolates <- function(g) {
  g <- flow(g)
    for (j in 1:ncol(g)) {
      if (g[nrow(g), j] == FLOW) return(TRUE)
    }
    return(FALSE)
}

percolates(g1)
percolates(g2)

registerDoMC(cores=4)

N = 25
REPLICATES = 1000

pseq = unique(c(seq(0, 0.2, 0.1), seq(0.2, 0.6, 0.0125), seq(0.6, 1, 0.1)))
perc.occp = foreach(p = pseq, .combine=rbind) %dopar% {
  data.frame(p, percolates = replicate(REPLICATES, percolates(create.grid(N, p))))
}

perc.summary = ddply(perc.occp, .(p), summarize,
  mean = mean(percolates),
  sd = sd(percolates) / sqrt(length(percolates))
)

logistic.fit = glm(mean ~ p, family=binomial(logit), data = perc.summary)

perc.summary$fit = logistic.fit$fitted

(pcrit = - logistic.fit$coefficients[1] / logistic.fit$coefficients[2])
1-pcrit
ggplot(perc.summary, aes(x = p, y = mean)) +
  geom_point(size = 3, shape = 21) +
  geom_line(aes(x = p, y = fit), linetype = 2) +
  geom_vline(xintercept = pcrit, color = "blue") +
  annotate("text", label = sprintf("p1 == %.5f", pcrit), x = 0.9, y = 1.0, parse = TRUE) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  ylab("Percolation Probability") +
  theme_classic()