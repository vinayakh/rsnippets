# Description : Mapping daycare centres using RgoogleMaps
# Website : http://rforwork.info/2013/10/17/big-and-small-daycares-in-toronto-by-building-type-mapped-using-rgooglemaps-and-toronto-open-data/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ff","ffbase","RgoogleMaps","plyr")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

childcare = read.csv.ffdf(file="data/child-care.csv", first.rows=500,next.rows=500,colClasses=NA,header=TRUE)
pcodes = read.csv.ffdf(file="data/Canada.csv", first.rows=50000, next.rows=50000, colClasses=NA, header=FALSE)

childcare$PCODE_R = as.ff(as.factor(sub(" ","", childcare[,"PCODE"])))
names(pcodes) = c("PCODE","Lat","Long","City","Prov")

childcare = merge(childcare, as.ffdf(pcodes[,1:3]), by.x="PCODE_R", by.y="PCODE", all.x=TRUE)

childcare.gc = subset(childcare, !is.na(Lat))
childcare.worship = subset(childcare.gc, bldg_type == "Place of Worship")
childcare.house = subset(childcare.gc, bldg_type == "House")
childcare.community = subset(childcare.gc, bldg_type == "Community/Recreation Centre")
childcare.pschool = subset(childcare.gc, bldg_type == "Public Elementary School")
childcare.highrise = subset(childcare.gc, bldg_type == "High Rise Apartment")
childcare.purpose = subset(childcare.gc, bldg_type == "Purpose Built")

Fn = ecdf(childcare.worship[,"TOTSPACE"])
childcare.worship$TOTSPACE.pct = as.ff(Fn(childcare.worship[,"TOTSPACE"]))
mymap = MapBackground(lat=childcare.worship[,"Lat"], lon=childcare.worship[,"Long"])
PlotOnStaticMap(mymap, childcare.worship[,"Lat"], childcare.worship[,"Long"], cex=childcare.worship[,"TOTSPACE.pct"]*4, pch=21, bg=addTrans("purple",100))

Fn = ecdf(childcare.house[,"TOTSPACE"])
childcare.house$TOTSPACE.pct = as.ff(Fn(childcare.house[,"TOTSPACE"]))
mymap = MapBackground(lat=childcare.house[,"Lat"], lon=childcare.house[,"Long"])
PlotOnStaticMap(mymap, childcare.house[,"Lat"], childcare.house[,"Long"], cex=childcare.house[,"TOTSPACE.pct"]*4, pch=21, bg=addTrans("purple",100))

Fn = ecdf(childcare.community[,"TOTSPACE"])
childcare.community$TOTSPACE.pct = as.ff(Fn(childcare.community[,"TOTSPACE"]))
mymap = MapBackground(lat=childcare.community[,"Lat"], lon=childcare.community[,"Long"])
PlotOnStaticMap(mymap, childcare.community[,"Lat"], childcare.community[,"Long"], cex=childcare.community[,"TOTSPACE.pct"]*4, pch=21, bg=addTrans("purple",100))

Fn = ecdf(childcare.pschool[,"TOTSPACE"])
childcare.pschool$TOTSPACE.pct = as.ff(Fn(childcare.pschool[,"TOTSPACE"]))
mymap = MapBackground(lat=childcare.pschool[,"Lat"], lon=childcare.pschool[,"Long"])
PlotOnStaticMap(mymap, childcare.pschool[,"Lat"], childcare.pschool[,"Long"], cex=childcare.pschool[,"TOTSPACE.pct"]*4, pch=21, bg=addTrans("purple",100))

Fn = ecdf(childcare.highrise[,"TOTSPACE"])
childcare.highrise$TOTSPACE.pct = as.ff(Fn(childcare.highrise[,"TOTSPACE"]))
mymap = MapBackground(lat=childcare.highrise[,"Lat"], lon=childcare.highrise[,"Long"])
PlotOnStaticMap(mymap, childcare.highrise[,"Lat"], childcare.highrise[,"Long"], cex=childcare.highrise[,"TOTSPACE.pct"]*4, pch=21, bg=addTrans("purple",100))

Fn = ecdf(childcare.purpose[,"TOTSPACE"])
childcare.purpose$TOTSPACE.pct = as.ff(Fn(childcare.purpose[,"TOTSPACE"]))
mymap = MapBackground(lat=childcare.purpose[,"Lat"], lon=childcare.purpose[,"Long"])
PlotOnStaticMap(mymap, childcare.purpose[,"Lat"], childcare.purpose[,"Long"], cex=childcare.purpose[,"TOTSPACE.pct"]*4, pch=21, bg=addTrans("purple",100))

space.by.bldg_type = ddply(as.data.frame(childcare.gc), .(bldg_type), function (x) c(min.space = min(x[,"TOTSPACE"], na.rm=TRUE), average.space = mean(x[,"TOTSPACE"], na.rm=TRUE), median.space = median(x[,"TOTSPACE"], na.rm=TRUE), max.space = max(x[,"TOTSPACE"], na.rm=TRUE), tot_daycares = sum(!is.na(x[,"TOTSPACE"]))))
space.by.bldg_type = space.by.bldg_type[order(-space.by.bldg_type$tot_daycares),]
