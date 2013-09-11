# Description : Generate and Aggregate many object with sequential names
# Website http://statcompute.wordpress.com/2013/09/08/generate-and-retrieve-many-objects-with-sequential-names/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("MASS")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

data(Boston, package = "MASS")

for (i in 1:10) {
  set.seed(i)
  smp <- Boston[sample(1:nrow(Boston), nrow(Boston), replace = TRUE), ]
  glm <- glm(medv ~ ., data = smp)
  prd <- predict(glm, Boston)
  ### ASSIGN A LIST OF SEQUENTIAL NAMES TO PREDICTIONS ###
  assign(paste("p", i, sep = ""), prd)
}

### RETURN NAMED OBJECTS TO A LIST ###
plist <- mget(paste('p', 1:i, sep = ''),as.environment(-1))
### AGGREGATE ALL PREDICTIONS ###
pcols <- do.call('cbind', plist)
pred_medv <- rowSums(pcols) / i

### A SIMPLE FUNCTION CALCULATION R-SQUARE ###
r2 <- function(y, yhat) {
  ybar <- mean(y)
  r2 <- sum((yhat - ybar) ^ 2) / sum((y - ybar) ^ 2)
  return(r2)
}
print(r2(Boston$medv, pred_medv))