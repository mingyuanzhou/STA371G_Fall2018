#install.packages("gdata")
#library(gdata) 
#setwd("~/Box Documents/Teaching/R_teaching")
data=read.table("RunsPerGame.txt",header=T,sep="\t")
data
#AVG (batting average)
AVG=data[,3]
#OBP (on base percentage)
OBP=data[,4]
#SLG (slugging percentage)
SLG=data[,5]
#RG (runs per game)
RG=data[,16]


par(mfrow=c(1,3))

AVGfit=lm(formula=RG~AVG)
summary(AVGfit)
plot(AVG,RG)
title(round(sum((summary(AVGfit)$residuals)^2)/30*1000)/1000)
alpha = summary(AVGfit)$coefficients[1,1]
beta = summary(AVGfit)$coefficients[2,1]
lines(AVG,alpha+beta*AVG)

SLGfit=lm(formula=RG~SLG)
summary(SLGfit)
plot(SLG,RG)
title(round(sum((summary(SLGfit)$residuals)^2)/30*1000)/1000)
alpha = summary(SLGfit)$coefficients[1,1]
beta = summary(SLGfit)$coefficients[2,1]
lines(SLG,alpha+beta*SLG)


OBPfit=lm(formula=RG~OBP)
summary(OBPfit)
plot(OBP,RG)
title(round(sum((summary(OBPfit)$residuals)^2)/30*1000)/1000)
alpha = summary(OBPfit)$coefficients[1,1]
beta = summary(OBPfit)$coefficients[2,1]
lines(OBP,alpha+beta*OBP)


