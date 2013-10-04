# Description : Creating a matrix from a long dataframe
# Website : http://lamages.blogspot.in/2013/10/creating-matrix-from-long-dataframe.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("reshape2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

n <- 7
claims.df <- data.frame(
  originf = factor(rep(2007:2013, n:1)),
  dev=sequence(n:1),
  paid=
    c(3511, 3215, 2266, 1712, 1059, 587,
      340, 4001, 3702, 2278, 1180, 956,
      629, 4355, 3932, 1946, 1522, 1238,
      4295, 3455, 2023, 1320, 4150, 3747,
      2320, 5102, 4548, 6283)
)

(claims.triangle <- with(claims.df, {
  M <- matrix(nrow=n, ncol=n,
              dimnames=list(origin=levels(originf), dev=1:n))
  M[cbind(originf, dev)] <- paid
  M
}))

acast(claims.df, originf ~ dev , value.var='paid',
      fun.aggregate=sum, margins=TRUE)
