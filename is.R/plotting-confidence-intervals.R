# Description : The Definitive guide to plotting confidence intervals in R
# Website : http://is-r.tumblr.com/post/38538981197/the-definitive-guide-to-plotting-confidence-intervals

doInstall <- TRUE
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

ANES <- read.csv("data/ANES.csv")
ANES <- ANES[ANES$year == 2008, -c(1, 11, 17)] # Limit to just 2008 respondents,
head(ANES) # remove some non-helpful variables

# Fit several models with the same DV:
model1 <- lm(pid7 ~ ideo7 + female + age + south, data = ANES)
model2 <- lm(pid7 ~ ideo7 + female + age + female:age, data = ANES)
model3 <- lm(pid7 ~ ideo7, data = ANES) # These are just arbitrary examples

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "South Indicator")
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Age Interaction")
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Univariate")
# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame)) # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2) # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Comparing several models")
print(zp1) # The trick to these is position_dodge().