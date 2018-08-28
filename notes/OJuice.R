
############################################
# OrangeJuice Example
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


