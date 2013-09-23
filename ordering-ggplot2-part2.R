# Description : Order by Demo part2
# Website : http://trinkerrstuff.wordpress.com/2013/08/14/how-do-i-re-arrange-ordering-a-plot-revisited/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2","gridExtra","RCurl","XML","rjson","qdap","reshape2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
               lapply(toInstall, library, character.only = TRUE)


library(RCurl)
library(XML)
library(rjson)
library(ggplot2)
library(qdap)
library(reshape2)
library(gridExtra)

URL <-"http://www.payscale.com/top-tech-employers-compared-2012/job-satisfaction-survey-data"
doc   <-htmlTreeParse(URL, useInternalNodes=TRUE)
nodes <-getNodeSet(doc, "//script[@type='text/javascript']")[[19]][[1]]
dat <-gsub("];", "]", capture.output(nodes)[5:27])
ndat <-data.frame(do.call(rbind, fromJSON(paste(dat, collapse = ""))))[, -2]
ndat[, 1:5] <-lapply(ndat, unlist)
IBM <-grepl("International Business Machines", ndat[, 1])
ndat[IBM, 1] <-bracketXtract(ndat[IBM, 1])
ndat[, 1] <-sapply(strsplit(ndat[, 1], "\\s|,"), "[", 1)

## Re-level with order_by
ndat[, "Employer.Name"] <-order_by(Employer.Name, ~Job.Satisfaction, ndat, df=FALSE)
colnames(ndat)[1] <-"Employer"
ndat

## Melt the data to long format
mdat <-melt(ndat)
mdat[, 2] <-factor(gsub("\\.", " ", mdat[, 2]),
                   levels = gsub("\\.", " ", colnames(ndat)[-1]))
head(mdat)

ggplot(data=mdat, aes(x=Employer, y=value, fill=factor(Employer))) +
  geom_bar(stat="identity") + coord_flip() + ylim(c(0, 1)) +
  facet_wrap( ~ variable, ncol=2) + theme(legend.position="none") +
  ggtitle("Plot 3: Employee Job Satisfaction at Top Tech Companies") +
  ylab(c("Job Satisfaction"))

mod <-lm(Job.Satisfaction ~ Work.Stress + Job.Meaning + Job.Flexibility, data=ndat)
mod

anova(mod)
theplot <-ggplot(data=ndat, aes(x = Job.Meaning, y = Job.Satisfaction)) +
  geom_smooth(method="lm", fill = "blue", alpha = .1, size=1) + 
  geom_smooth(color="red", fill = "pink", alpha = .3, size=1) +
  xlim(c(.4, .9)) +
  geom_point(aes(size = Job.Flexibility, colour = Work.Stress)) +
  geom_text(aes(label=Employer), size = 3, hjust=-.1, vjust=-.1) +
  scale_colour_gradient(low="gold", high="red")

theplot
theplot + annotation_custom(grob=circleGrob(r = unit(.4,"npc")), xmin=.47, xmax=.57, ymin=.72, ymax=.82)

ndat$outs <-1
ndat$outs[ndat$Employer %in% qcv(AOL, Amazon.com, Nvidia, Sony)] <-0

ggplot(data=ndat, aes(x = Job.Meaning, y = Job.Satisfaction)) +
  geom_smooth(method="lm", fill = "blue", alpha = .1, size=1, aes(group=outs)) + 
  geom_smooth(color="red", fill = "pink", alpha = .3, size=1) +
  xlim(c(.4, .9)) +
  geom_point(aes(size = Job.Flexibility, colour = Work.Stress)) +
  geom_text(aes(label=Employer), size = 3, hjust=-.1, vjust=-.1) +
  scale_colour_gradient(low="gold", high="red") 

