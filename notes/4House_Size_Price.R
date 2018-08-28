#install.packages("gdata")
#library(gdata) 
#setwd("~/Box Documents/Teaching/R_teaching")
#data =read.xls("housedata.xls")
#data
#Size = data[,1]
#Price=data[,2]

Size=c(0.8,0.9,1.0,1.1,1.4,1.4,1.5,1.6,1.8,2.0,2.4,2.5,2.7,3.2,3.5)
Price=c(70,83,74,93,89,58, 85,114, 95,100,138,111,124,161,172)
plot(Size,Price)

HouseFit=lm(formula=Price~Size)
summary(HouseFit)


alpha = summary(HouseFit)$coefficients[1,1]
beta = summary(HouseFit)$coefficients[2,1]

lines(Size,alpha+beta*Size)


sum(Price-(alpha+beta*Size))
sum((Price-(alpha+beta*Size))^2)

plot(Size,Price-(alpha+beta*Size))


HouseFit=lm(formula=Price~Size)
summary(HouseFit)
anova(HouseFit)