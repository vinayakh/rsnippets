# Description : GGtutorial: Day 3 - Introduction to Colors
# Website : http://is-r.tumblr.com/post/34693674524/ggtutorial-day-3-introduction-to-colors

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "gridExtra")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

X <- c(1)
Y <- c(1)

DF <- as.data.frame(cbind(X,Y))
c.1 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="One Colour") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

X <- c(1:2)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.2 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="Two Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

X <- c(1:3)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.3 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="Three Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

X <- c(1:4)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.4 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none")+ labs(x="", y="", title="Four Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

X <- c(1:5)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.5 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="Five Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

X <- c(1:6)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.6 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="Six Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

X <- c(1:7)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.7 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="Seven Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))


X <- c(1:8)
Y <- rep(1, length(X))
DF <- as.data.frame(cbind(X,Y))
c.8 <- ggplot(data = DF) + geom_bar(aes(x = X, y = Y, fill = factor(X))) + coord_flip() + theme(legend.position="none") + labs(x="", y="", title="Eight Colours") + scale_y_continuous(breaks=NULL, expand=c(0,0)) + scale_x_continuous(breaks=NULL, expand=c(0,0))

MfrowGG <- grid.arrange(c.1, c.2, c.3, c.4, c.5,
                        c.6, c.7, c.8, ncol=2)