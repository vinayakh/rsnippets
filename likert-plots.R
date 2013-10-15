# Description : Likert Plots
# Website : http://jason.bryer.org/likert/

require(devtools)
install_github('likert', 'jbryer')
require(likert)
ls("package:likert")

data(pisaitems)
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"]
head(items28)
ncol(items28)

# Plots
l28 <- likert(items28)
summary(l28)
plot(l28)
plot(l28, centered = FALSE, wrap = 30)

# Some more plots 
plot(l28, type = "density")
plot(l28, type = "heat")

# Group by Country
l28g <- likert(items28, grouping = pisaitems$CNT)
print(l28g)
plot(l28g)
plot(l28g, include.histogram = TRUE)
plot(l28g, centered = FALSE)
plot(l28g, type = "density")

## Item 29
title <- "How often do you read these materials because you want to?"
items29 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST25Q"]
names(items29) <- c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
l29 <- likert(items29)
summary(l29)

# Plots
plot(l29, centered = FALSE) + ggtitle(title)
plot(l29, center = 2.5) + ggtitle(title)

l29g <- likert(items29, grouping = pisaitems$CNT)
summary(l29g)
plot(l29g) + ggtitle(title)
plot(l29g, centered = FALSE, center = 2.5) + ggtitle(title)
plot(l29g, type = "density", legend = "Country") + ggtitle(title)