##Calculation of covariance and correlation
Size=c(0.8,0.9,1.0,1.1,1.4,1.4,1.5,1.6,1.8,2.0,2.4,2.5,2.7,3.2,3.5)
Price=c(70,83,74,93,89,58, 85,114, 95,100,138,111,124,161,172)

plot(Size,Price)
Sx = sqrt(var(Size))
sqrt(sum((Size-mean(Size))^2)/(length(Size)-1))

Sy = sqrt(var(Price))
sqrt(sum((Price-mean(Price))^2)/(length(Price)-1))

cov(Size,Price)
sum((Size-mean(Size))*(Price-mean(Price)))/(length(Price)-1)

cor(Size,Price)
cov(Size,Price)/(Sx*Sy)

##Correlation is scale invariant
cov(Size*10,Price)/cov(Size,Price)
a=rnorm(1)*1000;
b=rnorm(1)*2;
#sign(a*b)
cor(Size*a,Price*b)/sign(a*b)/cor(Size,Price)


##Uncorrelated != independent

x=rnorm(100)
x=c(x,-x)
y=x^2
plot(x,y,main=cor(x,y))



##Linear regression, correlation between residuales and X
par(mfrow=c(1,1))
HouseFit=lm(formula=Price~Size)
summary(HouseFit)
res = summary(HouseFit)$residuals
plot(Size,res)
lines(Size,matrix(1,length(Size))*0)
title(cor(Size,res))
mean(res)



##Arbitrary values
alpha = summary(HouseFit)$coefficients[1,1]
beta = summary(HouseFit)$coefficients[2,1]

#beta = cor(Size,Price)*Sy/Sx
#alpha = mean(Price)-beta*mean(Size)

plot(Size,alpha+beta*Size)
lines(Size,alpha+beta*Size)

alpha1=10
beta1 = 50
lines(Size,alpha1+beta1*Size,col='red')

res1 = Price - (alpha1+beta1*Size)
plot(Size,res1)
lines(Size, -(alpha1+beta1*Size) + (alpha+beta*Size),col='Red')
lines(Size,matrix(1,length(Size))*0)
title(cor(Size,res1))
mean(res1)
mean(res1^2)

