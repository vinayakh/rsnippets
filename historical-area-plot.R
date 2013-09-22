# Description : Emulation of Historical Area plot in R
# Website : http://beckmw.wordpress.com/2013/09/17/a-nifty-area-plot-or-a-bootleg-of-a-ggplot-geom/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2","reshape","gridExtra")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

#create data
set.seed(3)

#time steps
t.step<-seq(0,20)

#group names
grps<-letters[1:10]

#random data for group values across time
grp.dat<-runif(length(t.step)*length(grps),5,15)

#create data frame for use with plot
grp.dat<-matrix(grp.dat,nrow=length(t.step),ncol=length(grps))
grp.dat<-data.frame(grp.dat,row.names=t.step)
names(grp.dat)<-grps

# Plot area function definition
plot.area<-function(x,col=NULL,horiz=F,prop=T,stp.ln=T,grp.ln=T,axs.cex=1,axs.lab=T,lab.cex=1,
                    names=c('Group','Step','Value'),...){
  
  #sort out color fector  
  if(!is.null(col)){
    if(sum(col %in% colors()) != length(col)) stop('col vector must be in "colors()"')
    col<-colorRampPalette(col)(ncol(x))
  }
  else col<-colorRampPalette(c('lightblue','green'))(ncol(x))
  
  #convert data 
  if(prop) plt.dat<-x/rowSums(x)
  else plt.dat<-x
  plt.dat<-t(apply(plt.dat,1,cumsum))
  plt.dat<-data.frame(strt=rep(0,nrow(plt.dat)),plt.dat)
  
  #create plot
  y.range<-c(0,max(plt.dat))
  x.range<-c(0,max(plt.dat))
  y.locs<-seq(y.range[2],y.range[1],length=nrow(plt.dat))
  if(horiz) y.locs<-rev(y.locs)
  
  plot.new()
  
  par(...)
  plot(x.range,y.range,type='n',axes=F,xlab='',ylab='')
  
  #polygons
  sapply(
    1:(nrow(plt.dat)),
    function(y){
      sapply(
        1:(ncol(plt.dat)-1),
        function(x){ 
          x.loc.poly<-c(plt.dat[y,x],plt.dat[y,x+1],plt.dat[y+1,x+1],plt.dat[y+1,x])
          y.loc.poly<-rep(c(y.locs[y],y.locs[y+1]),each=2)
          if(horiz) polygon(y.loc.poly,x.loc.poly,col=col[x],border=NA)
          else polygon(x.loc.poly,y.loc.poly,col=col[x],border=NA)
        }
      )     
    }
  )
  
  #group labels
  x.labs<-sapply(1:(ncol(plt.dat)-1),function(x) mean(c(plt.dat[1,x],plt.dat[1,x+1])))
  
  parms<-list(
    1:ncol(plt.dat),
    function(col) lines(plt.dat[,col],y.locs,lwd=1), 
    function(y) segments(0,y.locs[y],plt.dat[y,ncol(plt.dat)],y.locs[y]),
    3,1,2,
    x.labs,
    y.locs,
    3,2,1
  )
  if(horiz) 
    parms<-list(
      ncol(plt.dat):1,
      function(col) lines(y.locs,plt.dat[,col],lwd=1),
      function(y) segments(y.locs[y],0,y.locs[y],plt.dat[y,ncol(plt.dat)]),
      2,4,1,
      x.labs,
      y.locs,
      2,1,4
    )
  
  #grp lines
  if(grp.ln) sapply(parms[[1]],parms[[2]])
  
  #step lines
  if(stp.ln) sapply(nrow(plt.dat):1,parms[[3]])
  
  #group axis labels
  axis(side=parms[[4]],at=parms[[7]],labels=names(plt.dat)[-1],tick=F,line=-1,las=1,cex.axis=axs.cex)
  
  #value axis
  axis(side=parms[[5]],las=1,cex.axis=axs.cex)
  
  #time step axis labels
  axis(side=parms[[6]],at=parms[[8]],labels=row.names(plt.dat),tick=F,line=-1,las=1,cex.axis=axs.cex)
  
  #axis labels
  if(axs.lab){
    mtext(side=parms[[9]],names[1],line=1,cex=lab.cex)
    mtext(side=parms[[10]],names[2],line=1,cex=lab.cex)
    mtext(side=parms[[11]],names[3],line=2,cex=lab.cex)
  }
  
}

# Plot an area graph
plot.area(grp.dat)

# Plot with more color variance
plot.area(grp.dat,col=c('red','lightgreen','purple'))

# More Customisation of lines
plot.area(grp.dat,col=c('red','lightgreen','purple'),grp.ln=F)
plot.area(grp.dat,col=c('red','lightgreen','purple'),stp.ln=F)

# Some custmisation and orientation change
plot.area(grp.dat,prop=F)
plot.area(grp.dat,prop=F,horiz=T)

# Area plots using ggplot2
require(ggplot2)
require(reshape)
require(gridExtra)

p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F)
p.dat<-melt(p.dat,id='step')
p.dat$step<-as.numeric(p.dat$step)

p<-ggplot(p.dat,aes(x=step,y=value))

p1<-p + geom_area(aes(fill=variable)) + theme(legend.position="bottom")

p2<-p + geom_area(aes(fill=variable),position='fill')

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))