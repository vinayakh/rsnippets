# Description : Converting an R object to text, with dput()
# Website : http://is-r.tumblr.com/post/31918894953/converting-an-r-object-to-text-with-dput

# Let's say you have an R object that you'd like to share with someone else
# but, for whatever reason, it is necessary to share it in text form.

# Generate a random data.frame
set.seed(1337)
NN <- 10
theData <- data.frame(Alpha = rnorm(NN),
                      Beta = rnorm(NN))
theData$Gamma <- theData$Alpha * 2 + theData$Beta / 2 + rnorm(NN)
print(theData)

# Generate output that will allow another user to re-constitute that data.frame
dput(theData) # Note that for large objects, this will produce a lot of text.

# Test it out, by copy-and-pasting the dput() output
theDataReconstituted <- structure(list(Alpha = c(0.192491906485068, -1.44670180633351,
                                                 -0.323180534047634, 1.62229611652493, -0.689024123596357, 2.04212222261495,
                                                 0.94377911190294, 2.0819268787991, 1.91711727878331, -0.414812239592928
), Beta = c(1.03285349943413, -1.67856959219527, 0.157549690345431,
            1.48913611644558, -0.0757895625491196, 1.27178094415894, 0.641673407672177,
            0.800761254937157, 1.86265922566283, -0.545356026768875), Gamma = c(1.52068837343838,
                                                                                -3.61004798325456, -1.35857038834863, 3.48938862108709, -3.05109504225968,
                                                                                6.5047022366346, 2.50727319977214, 5.31673927920108, 3.69096202696173,
                                                                                -1.03802874828505)), .Names = c("Alpha", "Beta", "Gamma"), row.names = c(NA,
                                                                                                                                                         -10L), class = "data.frame")
print(theDataReconstituted)

# See that they are (approximately) equal.
# We lose some precision, as the dput() output has a limited number of decimals.
round(theDataReconstituted, 10) == round(theData, 10)

# Another approach, saving to and loading from a file:
dput(theData, "temporary_file")
theDataReconstitutedAgain <- dget("temporary_file")
print(theDataReconstitutedAgain)

# Check it again:
round(theDataReconstitutedAgain, 10) == round(theData, 10)