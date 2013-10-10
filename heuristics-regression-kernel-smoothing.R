# Description : heuristics about local regression and kernel smoothing
# Website : http://freakonometrics.hypotheses.org/9182

# Generate Random data
set.seed(1)
n=10
xr = seq(0,n,by=.1)
yr = sin(xr/2)+rnorm(length(xr))/2
db = data.frame(x=xr,y=yr)
plot(db)

# Run regression
reg = lm(y ~ x,data=db)
abline(reg,col="red")

# Regression with x^5 polynomial
reg=lm(y~poly(x,5),data=db)
lines(xr,predict(reg),col="blue")

# Regression with x^25 polynomial
reg=lm(y~poly(x,25),data=db)
lines(xr,predict(reg),col="green")

# Local Regression with x^25 polynomial
plot(db)
attach(db)
lines(xr,predict(reg),col="red",lty=2)
yrm=yr;yrm[31]=yr[31]-2 
regm=lm(yrm~poly(xr,25)) 
lines(xr,predict(regm),col="red")

fitloc0 = function(x0){
  w=dnorm((xr-x0))
  reg=lm(y~1,data=db,weights=w)
  return(predict(reg,newdata=data.frame(x=x0)))
}

ul=seq(0,10,by=.01)
vl0=Vectorize(fitloc0)(ul)
u0=seq(-2,7,by=.01)
linearlocalconst=function(x0){
  w=dnorm((xr-x0))
  plot(db,cex=abs(w)*4)
  lines(ul,vl0,col="red")
  axis(3)
  axis(2)
  reg=lm(y~1,data=db,weights=w)
  u=seq(0,10,by=.02)
  v=predict(reg,newdata=data.frame(x=u))
  lines(u,v,col="red",lwd=2)
  abline(v=c(0,x0,10),lty=2)
}
linearlocalconst(2)

vx0=seq(1,9,by=.1)
vx0=c(vx0,rev(vx0))
graphloc=function(i){
  name=paste("local-reg-",100+i,".png",sep="")
  png(name,600,400)
  linearlocalconst(vx0[i])
}

for(i in 1:length(vx0)) graphloc(i)
# Run the following on the command line to get a single image
# convert -delay 25 local-reg-1*.png local-reg.gif

# First degree local rgeression function
fitloc1 = function(x0){
  w=dnorm((xr-x0))
  reg=lm(y~poly(x,degree=1),data=db,weights=w)
  return(predict(reg,newdata=data.frame(x=x0)))}

# 2nd degree local rgeression function
fitloc2 = function(x0){
  w=dnorm((xr-x0))
  reg=lm(y~poly(x,degree=2),data=db,weights=w)
  return(predict(reg,newdata=data.frame(x=x0)))}