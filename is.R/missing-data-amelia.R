# Description : Handling Missing Data with Amelia
# Website : http://is-r.tumblr.com/post/37547995110/handling-missing-data-with-amelia

doInstall <- TRUE
toInstall <- c("Amelia", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

ANES <- read.csv("data/ANES.csv")
ANES <- ANES[ANES$year == 2008, -c(1, 11, 17)] # Limit to just 2008 respondents,
head(ANES) # remove some non-helpful variables

with(ANES, plot(jitter(pid7), jitter(ideo7)))
myModel <- lm(pid7 ~ ideo7 + female + age + south, data = ANES)
summary(myModel) # Note 715 observations deleted due to missingness
missmap(ANES) # Show where observations are missing

### Imput missing data ###
imputedANES <- amelia(x = ANES, m = 10, # number of imputed data sets
                      noms = c("female", "race6", "religion", "dems", "south"),
                      ords = c("cohort", "pid7", "trust", "ideo7", "inerrant"))
# These last two options allow you to list nominal and ordinal variables.

plot(imputedANES)

with(imputedANES, lm(pid7 ~ ideo7 + female + age + south))

b.out<-NULL
se.out<-NULL
for(i in 1:imputedANES$m) { # Replicate the model on each imputed dataset
  ols.out <- lm(pid7 ~ ideo7 + female + age + south,
                data = imputedANES$imputations[[i]])
  b.out <- rbind(b.out, ols.out$coef)
  se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}

# Use Rubin's rules for combining results.
combined.results <- mi.meld(q = b.out, se = se.out)

lwdFrame <- data.frame(Variable = rownames(summary(myModel)$coef),
                       Coef = summary(myModel)$coef[, 1],
                       SE = summary(myModel)$coef[, 2],
                       Method = "listwiseDelete")
midFrame <- data.frame(Variable = colnames(combined.results$q),
                       Coef = c(combined.results$q),
                       SE = c(combined.results$se),
                       Method = "imputation")
resultFrame <- data.frame(rbind(lwdFrame, midFrame))

zp1 <- ggplot(resultFrame, aes(colour = Method))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coef - SE*1,
                                ymax = Coef + SE*1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coef,
                                 ymin = Coef - SE*2, ymax = Coef + SE*2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Comparing coefficient estimates by missing data methods")
print(zp1) # The trick to these cool coefficient plots is position_dodge().