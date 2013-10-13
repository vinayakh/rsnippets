# Description : Kernel Smoothing part 2
# Website : http://freakonometrics.hypotheses.org/9184

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("splines")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE

# Generate Random data
set.seed(1)
n=10
xr = seq(0,n,by=.1)
yr = sin(xr/2)+rnorm(length(xr))/2
db = data.frame(x=xr,y=yr)
plot(db)


attach(db)
B=bs(xr,knots=c(3),Boundary.knots=c(0,10),degre=1)
reg=lm(yr~B)
lines(xr[xr<=3],predict(reg)[xr<=3],col="red")
lines(xr[xr>=3],predict(reg)[xr>=3],col="blue")

reg=lm(yr~xr,subset=xr<=3)
lines(xr[xr<=3],predict(reg)[xr<=3],col="red",lty=2)
reg=lm(yr~xr,subset=xr>=3)
lines(xr[xr>=3],predict(reg),col="blue",lty=2)

reg=lm(yr~bs(xr,knots=c(3),Boundary.knots=c(0,10),degre=1),data=db)

B=bs(xr,knots=c(2,5),Boundary.knots=c(0,10),degre=1)
matplot(xr,B,type="l")
abline(v=c(0,2,5,10),lty=2)

B=bs(xr,knots=c(2,5),Boundary.knots=c(0,10),degre=1)
matplot(xr,B,type="l")
abline(v=c(0,2,5,10),lty=2)

reg=lm(yr~B)
lines(xr,predict(reg),col="red")

B=bs(xr,knots=1:9,Boundary.knots=c(0,10),degre=1)
reg=lm(yr~B)
lines(xr,predict(reg),col="red")

reg=lm(yr~B)
P=predict(reg,interval="confidence")
plot(db,col="white")
polygon(c(xr,rev(xr)),c(P[,2],rev(P[,3])),col="light blue",border=NA)
points(db)
reg=lm(yr~B)
lines(xr,P[,1],col="red")
abline(v=c(0,2,5,10),lty=2)

B=bs(xr,knots=c(2,5),Boundary.knots=c(0,10),degre=2)
matplot(xr,B,type="l")
abline(v=c(0,2,5,10),lty=2)

k=2
plot(db)
B=cbind(1,B)
lines(xr,B[,1:k]%*%coefficients(reg)[1:k],col=k-1,lty=k-1)

k=3
plot(db)
B=cbind(1,B)
lines(xr,B[,1:k]%*%coefficients(reg)[1:k],col=k-1,lty=k-1)

k=4
plot(db)
B=cbind(1,B)
lines(xr,B[,1:k]%*%coefficients(reg)[1:k],col=k-1,lty=k-1)

k=5
plot(db)
B=cbind(1,B)
lines(xr,B[,1:k]%*%coefficients(reg)[1:k],col=k-1,lty=k-1)

reg=lm(yr~B)
P=predict(reg,interval="confidence")
plot(db,col="white")
polygon(c(xr,rev(xr)),c(P[,2],rev(P[,3])),col="light blue",border=NA)
points(db)
reg=lm(yr~B)
lines(xr,P[,1],col="red")
abline(v=c(0,2,5,10),lty=2)
