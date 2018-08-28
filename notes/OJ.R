
############################################
### Log-Log Model example: imports vs GDP
############################################

par(mfrow=c(1,2))
trade <- read.csv("imports.csv")
attach(trade)
plot(IMPORTS, GDP, col=0, xlim=c(0,1300))
text(IMPORTS, GDP, labels=Country)
plot(log(IMPORTS), log(GDP), col=0, xlim=c(-2.5, 9))
text(log(IMPORTS), log(GDP), labels=Country)

lm( log(GDP) ~ log(IMPORTS) )


############################################
# OJ Example
############################################
OJ = read.table("OJ.txt",header=T)
attach(OJ)

par(mfrow=c(1,2))

model = lm(Sales~Price)
plot(Price,Sales,pch=19,col=4,main="Fitted Model")
abline(lsfit(Price,Sales),lwd=2,col=2)

plot(Price,model$res,pch=19,col=2,ylab="residuals",main="Residual Plot")

Logmodel = lm(log(Sales)~log(Price))



par(mfrow=c(1,2))
plot(Price,Sales,pch=19,col=4,main="Plug-in Prediction")
lines(sort(Price),exp(Logmodel$fitted[order(Price)]),pch=19,col=2,lwd=2)
lines(sort(Price),exp(Logmodel$fitted[order(Price)] - 2*0.38),pch=19,col=2,lwd=2,lty=2)
lines(sort(Price),exp(Logmodel$fitted[order(Price)] + 2*0.38),pch=19,col=2,lwd=2,lty=2)

plot(log(Price),log(Sales),pch=19,col=4,main="Plug-in Prediction")
abline(lsfit(log(Price),log(Sales)),lwd=2,col=2)
abline(c(Logmodel$coef[1]-2*0.38,Logmodel$coef[2]),lwd=2,col=2,lty=2)
abline(c(Logmodel$coef[1]+2*0.38,Logmodel$coef[2]),lwd=2,col=2,lty=2)

lines(sort(Price),exp(Logmodel$fitted[order(Price)] - 2*0.38),pch=19,col=2,lwd=2,lty=2)
lines(sort(Price),exp(Logmodel$fitted[order(Price)] + 2*0.38),pch=19,col=2,lwd=2,lty=2)





plot(Price,Logmodel$res,pch=19,col=2,ylab="residuals",main="Residual Plot")




###
## Telemarkting Example...
###
Tele = read.table("Tele.txt",header=T)
attach(Tele)



plot(months,calls,pch=19,col=4,cex.lab=1.4,cex=1.5,xlim=c(9,40),ylim=c(15,40))

polyfit = lm(calls~poly(months,8))

Xf = data.frame(months=seq(10,40,by=0.5))
Yf = predict(polyfit,Xf)


lines(sort(Xf$months),Yf[order(Xf)],col=6,lwd=3)
legend(10,38,c("2","3","8"),lty=c(1,1,1),col=c(2,3,6),lwd=c(2,2,2))

############################################
# More Non-Linear Models
############################################

attach(Boston)

plot(lstat,medv)
abline(lsfit(lstat,medv),lwd=3)


poly2fit = lm(medv~poly(lstat,2))
poly3fit = lm(medv~poly(lstat,3))
poly4fit = lm(medv~poly(lstat,4))
poly15fit = lm(medv~poly(lstat,15))

lines(sort(lstat),poly2fit$fit[order(lstat)],col=2,lwd=3)
lines(sort(lstat),poly3fit$fit[order(lstat)],col=3,lwd=3)
lines(sort(lstat),poly4fit$fit[order(lstat)],col=4,lwd=3)
lines(sort(lstat),poly15fit$fit[order(lstat)],col=5,lwd=3)
legend(30,40,c(2,3,4,15),lwd=c(3,3,3,3),col=c(2,3,4,5))


############################################
# Regression Splines
############################################


plot(lstat,medv,pch=20)
abline(lsfit(lstat,medv),lwd=3)

splinefit1 = lm(medv~bs(lstat,knots=1))
splinefit3 = lm(medv~bs(lstat,knots=3))
splinefit10 = lm(medv~bs(lstat,knots=10))

lines(sort(lstat),splinefit1$fit[order(lstat)],col=2,lwd=3)

lines(sort(lstat),splinefit3$fit[order(lstat)],col=4,lwd=3)

lines(sort(lstat),splinefit10$fit[order(lstat)],col=3,lwd=3)

legend(30,40,c("1 knot","3 knots","10 knots"),lwd=c(3,3,3),col=c(2,4,3))


############################################
# Smoothing Slines
############################################

plot(lstat,medv,pch=20)
abline(lsfit(lstat,medv),lwd=3)

smoothfit1 = smooth.spline(lstat,medv)
lines(smoothfit1$x,smoothfit1$y,col=4,lwd=2)

smoothfitL0 = smooth.spline(lstat,medv,spar=0)
lines(smoothfitL0$x,smoothfitL0$y,col=3,lwd=2)

smoothfitL07 = smooth.spline(lstat,medv,spar=0.7)
lines(smoothfitL07$x,smoothfitL07$y,col=2,lwd=2)

legend(30,40,c("Automatic","spar=0","spar=0.7"),lwd=c(3,3,3),col=c(4,3,2))


############################################
# GAMs
############################################


pairs(Boston[c(14,1,6,8,13)])

gamfit=gam(medv~s(crim)+s(rm)+s(dis)+s(lstat),data=Boston)

par(mfrow=c(2,2))
plot.gam(gamfit,se=T)


par(mfrow=c(1,1))
plot(medv,gamfit$fit,ylab="Fitted Values",pch=19)
abline(0,1,lwd=2,col=2)

n = dim(Boston)[1]

mse = matrix(0,1000,3)

for(i in 1:1000){
tr = sample(1:n,400,replace=F)

gamfit=gam(medv~s(crim)+s(rm)+s(dis)+s(lstat),data=Boston,subset=tr)
lmfit = lm(medv~crim+rm+dis+lstat,subset=tr)


mse[i,1] = mean((predict(gamfit,Boston)[-tr]-medv[-tr])^2)
mse[i,2] = mean((predict(lmfit,Boston)[-tr]-medv[-tr])^2)
mse[i,3] = mean((predict(lm(medv~1),Boston)[-tr]-medv[-tr])^2)
print(i)
}


############################################
# Variable Selection
############################################

library(lars)
data(diabetes)
attach(diabetes)

lassofit = lars(x,y,type="lasso")
names(lassofit)

par(mfrow=c(2,2))
plot(lassofit)
cv.lars(x,y)

coef.lars(lassofit,s=0.4,mode="fraction")


newX = data.frame(age=0,sex=0,bmi=0,map=0,tc=0,ldl=0,hdl=0,tch=0,ltg=0,glu=0)

predict(lassofit,newX,s=0.4,mode="fraction")


############################################
# BIC, AIC...
############################################

data(diabetes)
attach(diabetes)
lmfit = lm(y~x)

n=dim(x)[1]
extractAIC(lmfit,k=log(n))













