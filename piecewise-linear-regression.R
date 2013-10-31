# Description : Estimating continuous piecewise linear regression
# Website : http://rsnippets.blogspot.in/2013/04/estimating-continuous-piecewise-linear.html

N <- 40 # number of sampled points
K <- 5  # number of knots

piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

f <- function(x) {
  2 * sin(6 * x)
}

set.seed(1)
x <- seq(-1, 1, len = N)
y <- f(x) + rnorm(length(x))

knots <- seq(min(x), max(x), len = K + 2)[-c(1, K + 2)]
model <- lm(formula(paste("y ~", piece.formula("x", knots))))

par(mar = c(4, 4, 1, 1))
plot(x, y)
lines(x, f(x))
new.x <- seq(min(x), max(x) ,len = 10000)
points(new.x, predict(model, newdata = data.frame(x = new.x)),
       col = "red", pch = ".")
points(knots, predict(model, newdata = data.frame(x = knots)),
       col = "red", pch = 18)