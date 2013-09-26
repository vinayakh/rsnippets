# Description : Compactly display the structure of an arbitrary R object 
# Website : http://is-r.tumblr.com/post/31980859159/compactly-display-the-structure-of-an-arbitrary-r

# Let's say you want to display a table of model coefficients
# in order of their significance,
# and you want to plot the distribution of model residuals,
# but you don't know how to access these values.
# Use str().

# Generate some random data
NN <- 1000
theData <- data.frame(Alpha = rnorm(NN),
                      Beta = rnorm(NN))
theData$Gamma <- theData$Alpha * 2 + theData$Beta / 2 + rnorm(NN)

# Model the random data
simpleModel <- lm(Gamma ~ Alpha + Beta, data = theData)
# Save the model summary
modelSummary <- summary(simpleModel)

is.list(modelSummary) # The model summary is a list
# which includes, among other things, a table of coefficients
# standard errors, etc.
# str() lets you investigate the structure of any R object:
str(modelSummary)
# Here we see an item called "coefficients," which we can access with the "$."
modelSummary$coefficients

# And here is our ordered table
modelSummary$coefficients[order(modelSummary$coefficients[, 3]), ]

# Likewise for the residuals. str() reveals an item called "residuals,"
# and we can easily plot their distribution.
plot(density(modelSummary$residuals))