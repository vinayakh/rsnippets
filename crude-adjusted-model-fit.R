# Description : Crude and Adjusted Model fit
# Website : http://gforge.se/2013/10/printcrudeandadjusted/

toInstall <- c("survival","Hmisc","rms")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
install_github('Gmisc',username='gforge')

data(colon)
# Choose th ones wih deah as outcome
colon <- subset(colon, etype = 2)
# Change into years
colon$time <- colon$time/365.25
# Factor these binary variables or the software will interpret them as
# numeric even if the regression is correct
colon$sex <- factor(colon$sex, labels = c("Female", "Male"))
colon$perfor <- factor(colon$perfor, labels = c("No", "Yes"))
colon$obstruct <- factor(colon$obstruct, labels = c("No", "Yes"))

label(colon$rx) <- "Treatment"
label(colon$perfor) <- "Colon perforation"
label(colon$age) <- "Age (years)"
label(colon$sex) <- "Sex"

dd <- datadist(colon[, c("time", "status", "rx", "sex", "obstruct", "perfor", 
                         "age")])
options(datadist = "dd")
sn <- Surv(colon$time, colon$status)
fit <- cph(sn ~ rx + sex + perfor + age, data = colon)

library(Gmisc)
printCrudeAndAdjustedModel(fit, add_references = TRUE, ctable = TRUE)

# Update
update(fit, . ~ . + obstruct)
update(fit, . ~ . - age)
update(fit, . ~ rx)