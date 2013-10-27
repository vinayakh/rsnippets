# Description : Genetic Drift Simulation
# Website : http://rsnippets.blogspot.in/2013/08/genetic-drift-simulation.html

doInstall <- TRUE
toInstall <- c("lattice", "animation")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

# initialize simulation
# side - square grid size
# n    - number of agent types (from 2 to 6)
init <- function(side, n) {
  matrix(sample.int(n, side * side, TRUE), side)
}

# plot simulation state
# s - simulation state
# n - initial number of agent types
do.plot <- function(s, n) {
  print(levelplot(s, at = (0:n) + 0.5,
                  col.regions = 1 + (1:n), 
                  scales = list(draw = F), xlab = "", ylab = "",
                  colorkey = list(tick.number = n)))
}# perform one step of simulation
# state  - simulation state
sim.step <- function(state) {
  updated <- FALSE
  side <- dim(state)[1]
  moves_x <- rep(-1:1, 3)[-5]
  moves_y <- rep(-1:1, each = 3)[-5]
  sequence <- sample.int(length(state))
  for (i in sequence) {
    x <- 1 + (i - 1)%%side
    y <- 1 + (i - 1)%/%side
    ref <- sample.int(8, 1)
    ref.x <- 1 + (x + moves_x[ref] - 1)%%side
    ref.y <- 1 + (y + moves_y[ref] - 1)%%side
    if (state[x, y] != state[ref.x, ref.y]) {
      state[x, y] <- state[ref.x, ref.y]
      updated <- TRUE
    }
  }
  list(state, updated)
}

go <- function(size, n) {
  s <- init(size, n)
  updated <- TRUE
  for (i in 1:4) {
    do.plot(s, n)
  }
  i <- 0
  while (updated) {
    i <- i + 1
    do.plot(s, n)
    out <- sim.step(s)
    s <- out[[1]]
    updated <- out[[2]]
    do.plot(s, n)
  }
  print(i)
  for (i in 1:4) {
    do.plot(s, n)
  }
}

set.seed(1)
animate <- FALSE  # do we want to save animation?
side <- 15        # simulation grid will be side x side
n <- 5            # initial number of agent types

if (animate) {
  ani.options(interval = 0.2)
  saveGIF(go(side, n))
} else {
  go(side, n)
}
