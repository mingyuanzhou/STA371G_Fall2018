###
## Telemarkting Example...
###
Tele = read.table("Tele.txt",header=T)
attach(Tele)

plot(months,calls,pch=19,col=4,cex.lab=1.4,cex=1.5,xlim=c(9,40),ylim=c(15,40))
fit1 = lm(calls~months)
Xf = data.frame(months=seq(2,40,by=0.5))
Yf1 = predict(fit1,Xf)
lines(sort(Xf$months),Yf1[order(Xf)],col=1,lwd=3)

polyfit2 = lm(calls~poly(months,2))
Yf2 = predict(polyfit2,Xf)
lines(sort(Xf$months),Yf2[order(Xf)],col=2,lwd=3)

polyfit3 = lm(calls~poly(months,3))
Yf3 = predict(polyfit3,Xf)
lines(sort(Xf$months),Yf3[order(Xf)],col=3,lwd=3)

polyfit8 = lm(calls~poly(months,8))
Yf8 = predict(polyfit8,Xf)
lines(sort(Xf$months),Yf8[order(Xf)],col=6,lwd=3)

legend(10,38,c("1","2","3","8"),lty=c(1,1,1,1),col=c(1,2,3,6),lwd=c(2,2,2,2))

