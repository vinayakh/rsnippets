# Description : Apsrtable Getting from R to Latex
# Website : http://is-r.tumblr.com/post/37255330693/apsrtable-getting-tables-from-r-to-latex

doInstall <- TRUE
toInstall <- c("apsrtable")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#http://cran.r-project.org/web/packages/apsrtable/apsrtable.pdf

X1 <- rnorm(1000)
X2 <- rnorm(1000)
X3 <- rnorm(1000)
X4 <- rnorm(1000)
#Create a relationship to Model:
Y <- X1 + 1.8*X2 -.12*X3 + 1.8*X4 + rnorm(1000)

Model.1 <- lm(Y ~ X1 + X2)
Model.2 <- lm(Y ~ X1 + X3)
Model.3 <- lm(Y ~ X1 + X2 + X3)
Model.4 <- lm(Y ~ X1 + X2 + X3 + X4)

apsrtable(Model.1, Model.2, Model.3, Model.4, model.names=c("Model 1" , "Second Try", "Model 3", "My Fourth Model"))

apsrtable(Model.1, Model.2, Model.3, Model.4, lev=.30,
          model.names=c("Model 1" , "Second Try", "Model 3", "My Fourth Model"),
          coef.names=c("Intercept", "Some Variables", "That Might", "Be Of", "Interest to You"))