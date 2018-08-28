install.packages("ISLR")
library(ISLR)
summary(Auto)
attach(Auto)
pairs(Auto)

par(mfrow=c(1,2))
plot(horsepower,mpg,pch=19)
#Simple linear regerssion
MPGfit = lm(mpg~horsepower)
abline(MPGfit$coefficients,col='red',lwd=3)
summary(MPGfit)
plot(horsepower,MPGfit$residuals,pch=19)

par(mfrow=c(2,2))
plot(horsepower,mpg)
plot(horsepower,log(mpg))
plot(log(horsepower),mpg)
plot(log(horsepower),log(mpg))

par(mfrow=c(1,2))
#Simple linear regerssion
MPGfit = lm(mpg~horsepower)
summary(MPGfit)
plot(horsepower,MPGfit$residuals)

MPGfit1 = lm(log(mpg)~log(horsepower))
summary(MPGfit1)
plot(log(horsepower),MPGfit1$residuals)


#Multiple linear regression
MPGfit2 = lm(log(mpg)~log(horsepower)+log(weight))
summary(MPGfit2)
plot(log(horsepower),MPGfit2$residuals)
plot(log(weight),MPGfit2$residuals)

par(mfrow=c(1,2))
plot(horsepower,mpg,pch=19)
lines(sort(horsepower),
      exp(MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3)
plot(horsepower,MPGfit1$residuals,pch=19)


par(mfrow=c(1,2))
plot(horsepower,mpg,pch=19)
s = sqrt(sum(MPGfit1$residuals^2)/MPGfit1$df.residual)
lines(sort(horsepower),
      exp(-2*s+MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3,lty=2)
lines(sort(horsepower),
      exp(MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3)
lines(sort(horsepower),
      exp(2*s+MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3,lty=2)

plot(log(horsepower),log(mpg),pch=19)
s = sqrt(sum(MPGfit1$residuals^2)/MPGfit1$df.residual)
lines(sort(log(horsepower)),
      (-2*s+MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3,lty=2)
lines(sort(log(horsepower)),
      (MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3)
lines(sort(log(horsepower)),
      (2*s+MPGfit1$fitted.values[order(horsepower)]),col='red',pch=19,lwd=3,lty=2)

