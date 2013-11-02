# Description : Precalculated Lookup speedup
# Website : http://rmflight.github.io/posts/2013/10/precalcLookup.html

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

pMatch <- 1 - pchisq(distVal, 1)

# Random Number Set
nPoint <- 1e+06
randomData <- abs(rnorm(nPoint, mean = 5, sd = 5))  # take absolute so we have only positive values
randomData[randomData > 25] <- 25
hist(randomData, 100)

# The R way
bTime <- Sys.time()
actPVals <- 1 - pchisq(randomData, 1)
eTime <- Sys.time()
rwayDiff <- difftime(eTime, bTime)
rwayDiff

# Naive Way
naiveRes <- numeric(nPoint)
bTime <- Sys.time()
for (iP in 1:nPoint) {
  naiveRes[iP] <- 1 - pchisq(randomData[iP], 1)
}
eTime <- Sys.time()
naiveDiff <- difftime(eTime, bTime)
naiveDiff

# Lookup Table
nDivision <- 10000
dof <- 1
nSD <- 25

nElements <- nSD * nDivision
chiVals <- seq(0, nElements, 1)/nDivision  # the chi-squared values (or distances), also used as indices when multiplied by 10000
pTable <- 1 - pchisq(chiVals, 1)  # the actual chi-square p-values for those distances

testVal <- sample(chiVals, 1)  # grab a value from the chiVals previously generated
pTable[(testVal * nDivision) + 1]

1 - pchisq(testVal, 1)

tableRes <- numeric(nPoint)
bTime <- Sys.time()
for (iP in 1:nPoint) {
  tableRes[iP] <- pTable[(randomData[iP] * nDivision) + 1]
}
eTime <- Sys.time()
tableDiff <- difftime(eTime, bTime)
tableDiff

tableRawPrecision <- abs(tableRes - actPVals)/actPVals * 100

precTable <- data.frame(org = actPVals, table = tableRes, percError = tableRawPrecision)
ggplot(precTable, aes(x = org, y = table)) + geom_point()
ggplot(precTable, aes(x = org, y = percError)) + geom_point()

# Raw Calculations
cppRaw <- c("#include <iostream>", "#include <boost/math/distributions/chi_squared.hpp>", 
            "int nVal = 1000000;", "double dof = 1.0;", "int i;", 
            paste("float randVals[1000000] = {",
            paste(as.character(randomData), sep = "", collapse = ", "), "};", 
                                                                        collapse = ""), paste("float pTable[250001] = {", paste(as.character(pTable), 
                                                                                                                                sep = "", collapse = ", "), "};", sep = "", collapse = ""), "int main() {", 
            "using boost::math::chi_squared_distribution;", "chi_squared_distribution<> myChi(dof);", 
            "for (i = 0; i < nVal; i++){", "1 - cdf(myChi, randVals[i]);", "};", "return(0);", 
            "};")
cat(cppRaw, sep = "\n", file = "cppRaw.cpp")

system("g++ cppRaw.cpp -o cppRaw.out")
system("time ./cppRaw.out")

cppLookup <- c("#include <iostream>", "#include <boost/math/distributions/chi_squared.hpp>", 
               "int nVal = 1000000;", "double dof = 1.0;", "int i;", paste("float randVals[1000000] = {", 
                                                                           paste(as.character(randomData), sep = "", collapse = ", "), "};", sep = "", 
                                                                           collapse = ""), paste("float pTable[250001] = {", paste(as.character(pTable), 
                                                                                                                                   sep = "", collapse = ", "), "};", sep = "", collapse = ""), "int main() {", 
               "using boost::math::chi_squared_distribution;", "chi_squared_distribution<> myChi(dof);", 
               "for (i = 0; i < nVal; i++){", "pTable[(int(randVals[i] * nVal))];", "};", 
               "return(0);", "};")
cat(cppLookup, sep = "\n", file = "cppLookup.cpp")
system("g++ cppLookup.cpp -o cppLookup.out")
system("time ./cppLookup.out")