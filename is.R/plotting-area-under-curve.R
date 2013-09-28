# Description : Functions for plotting and getting Greek in labels
# Website : http://is-r.tumblr.com/post/33151549508/functions-for-plotting-and-getting-greek-in-labels

shade.norm <- function(MU, SD, Lower, Upper, fill.color) {
  cord.x <- c(Lower,seq(Lower,Upper,0.01),Upper)
  cord.y <- c(0,dnorm(seq(Lower,Upper,0.01),mean = MU, sd = SD),0)
  y.axis.text <-bquote("Density of N " * " " * "(" *
                         mu * " " * "=" * " " * .(MU) * ","* " " * sigma *
                         " " * "=" * " " * .(SD) * ")")
  
  curve(dnorm(x, MU, SD), xlim=c(MU - 4*SD, MU + 4*SD),ylab=y.axis.text)
  polygon(cord.x, cord.y, col=fill.color)
}
#The function is then called using shade.norm()
#So, which proportion of the population has IQs between 80 and 90?
shade.norm(100, 15, 80, 90, 'red')

#And, if we don't like red,
shade.norm(100, 15, 120, 130, 'blue')

#Or green?
shade.norm(5, 1.3, 2,4, 'green')

#Now, if we want to change how filled in the plot is, we can
#add an additional argument to our function. To keep things
#straight, I'm going to make a new function called
# shade.norm.density, and add a sixth argument, fill.density.
#This then appears in the plotting command, as the argument
# that corresponds with, you guessed it, the density of the
#color on the polygon function.
shade.norm.density <- function(MU, SD, Lower, Upper,
                               fill.color, fill.density){
  cord.x <- c(Lower,seq(Lower,Upper,0.01),Upper)
  cord.y <- c(0,dnorm(seq(Lower,Upper,0.01), mean = MU, sd = SD),0)
  y.axis.text <-bquote("Density of N " * " " * "(" *
                         mu * " " * "=" * " " * .(MU) * ","* " " * sigma *
                         " " * "=" * " " * .(SD) * ")")
  curve(dnorm(x, MU, SD), xlim=c(MU - 4*SD, MU + 4*SD), ylab=y.axis.text)
  polygon(cord.x, cord.y, col=fill.color, density = fill.density)
}
shade.norm.density(0, 1, 1, 2, 'darkblue', 45)