# Description : # A method for modifying only select off-diagonal items in a matrix
# Website :http://is-r.tumblr.com/post/32315446620/modifying-select-off-diagonal-items-in-a-matrix

# From "Thierry" and "Ben Bolker"
# At http://stackoverflow.com/a/11759744/479554

# A sample matrix
size <- 6
mat <- matrix(seq_len(size ^ 2), ncol = size)
print(mat)

# A companion matrix that indicates how "off" a diagonal is:
delta <- row(mat) - col(mat)
print(delta)

# Set these to select on the "delta" matrix
low <- 0
high <- 3

# Operate on the "mat" matrix
mat[delta < low | delta > high] <- NA
print(mat)

# Another example:
mat <- matrix(seq_len(size ^ 2), ncol = size)
mat <- mat * 0
print(mat) # All zeros

mat[abs(delta) == 1] <- 1
print(mat) # Ones on the "just off-diagonal."

###############
# Application #
###############

# To see year-to-year correlation of a set of observations
countryByYear <- matrix(rnorm(500), ncol = 20)
# Replace with numbers that actually have a relationship:
for(ii in 2:ncol(countryByYear)){
  countryByYear[, ii] <- countryByYear[, ii-1] + rnorm(nrow(countryByYear))
}

colnames(countryByYear) <- paste("year", (2012-ncol(countryByYear)+1):2012, sep = "") # Years
rownames(countryByYear) <- LETTERS[1:nrow(countryByYear)]

# Correlations across all years
yearToYearCorrelation <- cor(countryByYear, use = "pair")
print(yearToYearCorrelation)
plot(density(yearToYearCorrelation, na.rm = T))
text(yearToYearCorrelation, 0, "|", col = "GRAY", cex = 1/2)

# The delta matrix, which does all of the work for us
delta <- row(yearToYearCorrelation) - col(yearToYearCorrelation)
print(delta)

# Correlations between consecutive years only:
yearToYearCorrelation[abs(delta) != 1] <- NA
print(yearToYearCorrelation)
text(yearToYearCorrelation, 0, "|", col = "RED")
# These are generally much higher correlations, naturally