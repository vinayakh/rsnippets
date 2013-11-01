# Description : Using Changepoint to find sudden drops in trends
# Website : http://rforwork.info/2013/10/21/when-did-how-i-met-your-mother-become-less-legen-wait-for-it/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("changepoint","ggplot2","zoo")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

himym = read.csv("data/himym.csv")
mean2.pelt = cpt.mean(himym$Rating, method="PELT")
plot(mean2.pelt, type='l', cpt.col='red',xlab="Episode",
     ylab='IMDB Avg Rating', cpt.width=2, main="Changepoints in IMDB Ratings of 'How I Met Your Mother'",ylim=c(0,10), xlim=c(0,200))
eps.under.8 = rollapply(himym$Rating, width=10, function (x) mean(x < 8), by=10, partial=TRUE)
ep.nums = paste(((seq(1,20)-1)*10)+1, ((seq(1,20)-1)*10)+10, sep="-")
ep.nums[19] = "181-189"
ep.grouped.ratings = data.frame(ep.nums, eps.under.8)

p1 <- ggplot(ep.grouped.ratings, aes(x=reorder(ep.nums, seq(1,20)), 
      y=eps.under.8, group=1)) 
p1 <- p1  + geom_line(size=3, colour="dark red") 
p1 <- p1  + scale_y_continuous(name="Proportion of Epsidoes/10 With a Below 8.0 Avg Rating") 
p1 <- p1  + scale_x_discrete(name="Episode Number Grouping") 
p1 <- p1  + theme(axis.text.x=element_text(angle=-90, vjust=.5, colour="black", size=14), axis.text.y=element_text(colour="black", size=14), axis.title.x=element_text(size=16, face='bold'), axis.title.y=element_text(size=16, face="bold"), plot.title=element_text(size=20,face="bold")) 
p1 <- p1  + geom_vline(xintercept = 10,linetype='longdash') 
p1 <- p1  + ggtitle("How I Met Your Mother Ratings, Every 10 Episodes")
print(p1)