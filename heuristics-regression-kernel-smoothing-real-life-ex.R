# Description : Real life example of heuristics about local regression and kernel smoothing
# Website : http://freakonometrics.hypotheses.org/9182

toInstall <- c("xml","downloader")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

file = "geos-tww.csv"
html = htmlParse("http://www.geos.tv/index.php/list?sid=189&collection=all")
html = xpathApply(html, "//table[@id='collectionTable']")[[1]]
data = readHTMLTable(html)
data = data[,-3]
names(data)=c("no",names(data)[-1])
data=data[-(61:64),]

data$no = 1:96
data$mu = as.numeric(substr(as.character(data$Mean), 0, 4))
data$se =  sd(data$mu,na.rm=TRUE)/sqrt(as.numeric(as.character(data$Count)))
data$season = 1 + (data$no - 1)%/%12
data$season = factor(data$season)
plot(data$no,data$mu,ylim=c(6,10))
segments(data$no,data$mu-1.96*data$se,
         data$no,data$mu+1.96*data$se,col="light blue")

plot(data$no,data$mu,ylim=c(6,10))
abline(v=12*(0:8)+.5,lty=2)
for(s in 1:8){
  reg=lm(mu~no,data=subset(data,season==s))
  lines((s-1)*12+1:12,predict(reg)[1:12],col="red") 
}

db = data
NW = ksmooth(db$no,db$mu,kernel = "normal",bandwidth=5)
plot(data$no,data$mu)
lines(NW,col="red")

# Fix missing value
db$mu[95]=7
NW = ksmooth(db$no,db$mu,kernel = "normal",bandwidth=12) 
plot(data$no,data$mu,ylim=c(6,10)) 
lines(NW,col="red")
