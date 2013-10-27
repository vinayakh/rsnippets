# Description : Beautiful Donut Charts
# Website : http://r-nold.blogspot.in/2012/10/nscb-sexy-stats-version-2.html

doInstall <- TRUE
toInstall <- c("XML","RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

url<-"http://www.nscb.gov.ph/sexystats/2012/Filipinoversion/SS20121022_joblessness_filver.asp"
unemployment<-readHTMLTable(url, header=T, which=2,stringsAsFactors=F)
agegroup<-unemployment[3:8,c(1,3,5)]
gender<-unemployment[12:13,c(1,3,5)]
civil<-unemployment[17:20,c(1,3,5)]
education<-unemployment[25:31,c(1,3,5)]

#Copy to clipboard
a <- c("Education","Y2006", "Y2009")
b <- c("Elementary","42.5", "37.9")
c <- c("High School","47.7","52.2")
d <- c("College","9.7","10")
educ <- rbind(b,c,d)
colnames(educ) <- a

#educ<-read.table("clipboard", header=T, sep="\t")

colnames(agegroup)<-c("Age.Group","Y2006","Y2009")
colnames(gender)<-c("Gender","Y2006","Y2009")
colnames(civil)<-c("Civil.Status","Y2006","Y2009")
colnames(education)<-c("Education","Y2006","Y2009")

agegroup$Age.Group[6]<-"65 & Up"
agegroup$Y2006<-as.numeric(agegroup$Y2006)
agegroup$Y2009<-as.numeric(agegroup$Y2009)

gender$Gender[2]<-"Female"
gender$Gender[1]<-"Male"
gender$Y2006<-as.numeric(gender$Y2006)
gender$Y2009<-as.numeric(gender$Y2009)

civil$Y2006<-as.numeric(civil$Y2006)
civil$Y2009<-as.numeric(civil$Y2009)
cs<-c("Single","Married","Widowed", "Divorced")

par(mfrow=c(1,2), oma=c(1,0,1,1) , mar=c(1,1,0,1))

#Chart 1
pie(agegroup$Y2006,label=agegroup$Age.Group, col=brewer.pal(6,"Set1"), border="white")
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment\nby Age Group\nYear 2006", cex=1.5, font=2)

pie(agegroup$Y2009,label=agegroup$Age.Group, col=brewer.pal(6,"Set1"), border="white")
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment\nby Age Group\nYear 2009", cex=1.5, font=2)
text(0.5,-1, "Data Source: NSCB\nCreated by: ARSalvacion", adj=c(0,0), cex=0.7)

#Chart 2
pie(gender$Y2006,label=gender$Gender, col=brewer.pal(2,"Set1"), border="white", cex=1.5)
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment\nby Gender\nYear 2006", cex=1.5, font=2)

pie(gender$Y2009,label=gender$Gender, col=brewer.pal(2,"Set1"), border="white", cex=1.5)
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment\nby Gender\nYear 2009", cex=1.5, font=2)
text(0.5,-1, "Data Source: NSCB\nCreated by: ARSalvacion", adj=c(0,0), cex=0.7)

#Chart 3
pie(civil$Y2006,label=cs, col=brewer.pal(4,"Dark2"), border="white")
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment\nby Civil  Status\nYear 2006", cex=1.5, font=2)

pie(civil$Y2009,label=cs, col=brewer.pal(4,"Dark2"), border="white")
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment\nby Civil  Status\nYear 2009", cex=1.5, font=2)
text(0.5,-1, "Data Source: NSCB\nCreated by: ARSalvacion", adj=c(0,0), cex=0.7)

#Chart 4
pie(educ$Y2006,label=educ$Education, col=brewer.pal(3,"Dark2"), border="white", cex=1.5)
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment by\nEducational Level\nYear 2006", cex=1.5, font=2)

pie(educ$Y2009,label=educ$Education, col=brewer.pal(3,"Dark2"), border="white", cex=1.5)
par(new=TRUE) 
pie(c(1), labels=NA, border='white', radius=0.4)
text(0,0,labels="Percent\nUnemployment by\nEducational Level\nYear 2009", cex=1.5, font=2)
text(0.5,-1, "Data Source: NSCB\nCreated by: ARSalvacion", adj=c(0,0), cex=0.7)