# Description : Example of QQ plot
# Website : http://chemicalstatistician.wordpress.com/2013/09/22/exploratory-data-analysis-quantile-quantile-plots-for-new-yorks-ozone-pollution-data/

##### Quantile-Quantile Plots of Ozone Pollution Data
##### By Eric Cai - The Chemical Statistician
# clear all variables
rm(list = ls(all.names = TRUE))

# view first 6 entries of the "Ozone" data frame 
head(airquality)

# extract "Ozone" data vector
ozone = airquality$Ozone

# sample size of "ozone"
length(ozone)

# summary of "ozone"
summary(ozone)

# remove missing values from "ozone"
ozone = ozone[!is.na(ozone)]

# having removed missing values, find the number of non-missing values in "ozone"
n = length(ozone)

# calculate mean, variance and standard deviation of "ozone"
mean.ozone = mean(ozone)
var.ozone = var(ozone)
sd.ozone = sd(ozone)
# set n points in the interval (0,1)
# use the formula k/(n+1), for k = 1,..,n
# this is a vector of the n probabilities
probabilities = (1:n)/(n+1)

# calculate normal quantiles using mean and standard deviation from "ozone"
normal.quantiles = qnorm(probabilities, mean(ozone, na.rm = T), sd(ozone, na.rm = T))

# normal quantile-quantile plot for "ozone"
png('qq1.png')
plot(sort(normal.quantiles), sort(ozone) , xlab = 'Theoretical Quantiles from Normal Distribution', ylab = 'Sample Quqnatiles of Ozone', main = 'Normal Quantile-Quantile Plot of Ozone')
abline(0,1)
dev.off()

# calculate gamma quantiles using mean and standard deviation from "ozone" to calculate shape and scale parameters
gamma.quantiles = qgamma(probabilities, shape = mean.ozone^2/var.ozone, scale = var.ozone/mean.ozone)

# gamma quantile-quantile plot for "ozone"
png('qq2.png')
plot(sort(gamma.quantiles), sort(ozone), xlab = 'Theoretical Quantiles from Gamma Distribution', ylab = 'Sample Quantiles of Ozone', main = 'Gamma Quantile-Quantile Plot of Ozone')
abline(0,1)
dev.off()