# Description : Conditional Colors and Shapes in plot() with ifelse()
# Website : http://is-r.tumblr.com/post/33223979190/conditional-colors-and-shapes-in-plot-with-ifelse

#Plotting Random Variables
#rnorm(N, mean, sd): generates a
#random normal variable of length N
#with specified mean and std. dev. (sd)
# x/ylim = range of X/Y axis.
#col: colors, specified with an ifelse()
#pch, plot symbol to use
#pch list: http://voteview.com/symbols_pch.htm

X <- rnorm(500) #draw var 1
Y <- rnorm(500) #draw var 2
plot(X, Y, xlim=c(-3,3),ylim=c(-3,3),
     col=ifelse(((abs(X)>1.65 & abs(Y)>1.65)),"red", "black"),
     main="Two Dimensional Outliers (in red)" ,
     pch=ifelse(((abs(X)>1.65 & abs(Y)>1.65)), 17, 1)
)