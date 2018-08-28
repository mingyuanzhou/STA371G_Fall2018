install.packages("ISLR")
library(ISLR)
summary(Auto)
attach(Auto)
pairs(Auto)


#Simple linear regerssion
MPGfit = lm(mpg~weight)
summary(MPGfit)

#Multiple linear regression
MPGfit1 = lm(mpg~weight+horsepower)
summary(MPGfit1)

#Multiple linear regression
MPGfit2 = lm(mpg~weight+horsepower+displacement)
summary(MPGfit2)

#Multiple linear regression
MPGfit3 = lm(mpg~weight+horsepower+displacement+acceleration)
summary(MPGfit3)

#Multiple linear regression
MPGfit4 = lm(mpg~weight+horsepower+displacement+acceleration+cylinders)
summary(MPGfit4)
