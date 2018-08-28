attach(anscombe)
par(mfrow=c(2,2))
plot(x1,y1,xlim=c(3,20),ylim=c(3,14))
Yfit1 = lm(y1~x1)
abline(Yfit1$coefficients,col='red')

plot(x2,y2,xlim=c(3,20),ylim=c(3,14))
Yfit2 = lm(y2~x2)
abline(Yfit2$coefficients,col='red')

plot(x3,y3,xlim=c(3,20),ylim=c(3,14))
Yfit3 = lm(y3~x3)
abline(Yfit3$coefficients,col='red')

plot(x4,y4,xlim=c(3,20),ylim=c(3,14))
Yfit4 = lm(y4~x4)
abline(Yfit4$coefficients,col='red')

summary(Yfit1)
summary(Yfit2)

