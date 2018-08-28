
HouseData = read.table("MidCity.txt",header=T)
attach(HouseData)
dn1 = (Nbhd==1)
dn2 = (Nbhd==2)
dn3 = (Nbhd==3)
#Price = Price/1000
#SqFt = SqFt/1000

MidCity = lm(Price~SqFt)
plot(SqFt,Price,xlab="Size")
abline(lsfit(SqFt,Price),col=1,lwd=2,lty=2)
plot(SqFt,MidCity$residuals,xlab="Size")

points(SqFt[dn1],MidCity$residuals[dn1],col=2,pch=19)
points(SqFt[dn2],MidCity$residuals[dn2],col=3,pch=19)
points(SqFt[dn3],MidCity$residuals[dn3],col=4,pch=19)



MidCity = lm(Price~dn1+dn2+SqFt)
plot(SqFt,Price,xlab="Size")
abline(coef=MidCity$coef[c(1,4)],col=2,lwd=2)
points(SqFt[dn3==1],Price[dn3==1],col=2,pch=19)
abline(coef=c(MidCity$coef[1]+MidCity$coef[2],MidCity$coef[4]),col=4,lwd=2)
points(SqFt[dn1==1],Price[dn1==1],col=4,pch=19)
abline(coef=c(MidCity$coef[1]+MidCity$coef[3],MidCity$coef[4]),col=3,lwd=2)
points(SqFt[dn2==1],Price[dn2==1],col=3,pch=19)
legend(1450,210000,c("Nbhd = 1","Nbhd = 2","Nbhd = 3","Just Size"),lty=c(1,1,1,2),lwd=c(2,2,2,2),col=c(4,3,2,1))
abline(lsfit(SqFt,Price),col=1,lwd=2,lty=2)

plot(SqFt,MidCity$residuals,xlab="Size")

points(SqFt[dn1],MidCity$residuals[dn1],col=2,pch=19)
points(SqFt[dn2],MidCity$residuals[dn2],col=3,pch=19)
points(SqFt[dn3],MidCity$residuals[dn3],col=4,pch=19)

MidCity = lm(Price~dn1+dn2+SqFt+Offers+Brick+Bedrooms+Bathrooms)

MidCity = lm(Price~dn1+dn2+SqFt+Brick+Bedrooms+Bathrooms)

MidCity = lm(Price~dn2+dn3+SqFt+Brick+Bedrooms+Bathrooms)

MidCity = lm(Offers~dn1+dn2+SqFt+Price+Brick+Bedrooms+Bathrooms)

summary(MidCity)
