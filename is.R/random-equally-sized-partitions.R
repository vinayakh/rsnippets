# Description : Randomly allocating observations into groups, for, e.g. cross-validation
# Website : http://is-r.tumblr.com/post/32661033774/making-random-equally-sized-partitions

kk <- 10 # Number of partitions, as in "kk-fold cross-validation."

# Here is a data.frame full of good data:
nn <- 1003
myData <- data.frame(matrix(rnorm(nn * 3), ncol = 3))
colnames(myData) <- LETTERS[1:3]

# This does not work:
whichK <- sample(LETTERS[1:kk], nrow(myData), replace = T)
table(whichK) # Because the partitions are not equally sized

# This does work:
randomDraw <- rnorm(nrow(myData))
kQuantiles <- quantile(randomDraw, 0:kk/kk)
whichK <- cut(randomDraw, kQuantiles, include.lowest = TRUE) # Divide randomDraw into kk equally-sized groups
levels(whichK) <- LETTERS[1:kk] # (Optionally) Give the levels handier names

# Check partition counts:
table(whichK) # As equal as possible.

# Illustrating a lapply() over the training sets:
plot.new()
plot.window(xlim = c(-4, 4), ylim = c(0, 1/2))
lapply(levels(whichK), function(k){
  lines(density(myData$A[whichK != k]))})