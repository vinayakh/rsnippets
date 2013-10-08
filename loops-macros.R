# Description : Use of looping over R data structures
# Website : http://rforpublichealth.blogspot.in/2013/10/loops-revisited-how-to-rethink-macros.html

set.seed(10)
x <- rnorm(100, 5, 2)
z <- rnorm(100, 6, 5)
w <- rnorm(100, 3, 2)
y <- x * 2 + w * 0.5 + rnorm(100, 0, 1)
ybin <- as.numeric(y < 10)

mydata <- as.data.frame(cbind(x, z, w, y, ybin))
xvars <- cbind(x, z, w)
summary(lm(ybin ~ xvars, data = mydata))

summary(glm(ybin ~ xvars, family = binomial(logit), data = mydata))
data.sub <- as.data.frame(mydata[x > 2 & z < 3, c("x", "z", "ybin")])
xvars.sub <- as.matrix(data.sub[, c("x", "z")])
summary(lm(ybin ~ xvars.sub, data = data.sub))
apply(mydata[,c("y","ybin")], 2, function(outcome){summary(lm(outcome~x+z))})

# Looping over variable names
jan <- rnorm(100, 3, 5)
feb <- rnorm(100, 4, 8)
march <- rnorm(100, 2, 5)

months <- as.data.frame(cbind(jan, feb, march))
names(months)
for (n in names(months)) {
  months[[paste0("HadInc_", n)]] <- as.numeric(months[[n]] > 0)
}

# Looping over Varlists
names(months) <- toupper(names(months))

# Looping over numbers
Inc1990 <- rnorm(100, 5, 6)
Inc1991 <- rnorm(100, 3, 8)
Inc1992 <- rnorm(100, 4, 4)

Income <- as.data.frame(cbind(Inc1990, Inc1991, Inc1992))
years <- c(1990:1992)
for (i in seq(along = years)) {
  Income[[paste0("hadInc_", years[i])]] <- as.numeric(Income[[i]] > 0)
}

# Looping over Values and LevelsOf
race <- c(rep(1, 30), rep(2, 30), rep(3, 40))
age <- rnorm(100, 25, 3)
y <- age * 10 + ifelse(race == 1, 100, ifelse(race == 2, 2000, 0)) + rnorm(100, 0, 1)
racedata <- as.data.frame(cbind(race, age, y))
racedata$race <- as.factor(racedata$race)

lapply(1:3, function(index) summary(lm(y~age, data=racedata[racedata$race==index,])))

lapply(as.numeric(levels(race)), function(index) summary(lm(y~age, data=racedata[racedata$race==index,])))