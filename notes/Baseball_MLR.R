#install.packages("gdata")
#library(gdata) 
#setwd("~/Box Documents/Teaching/R_teaching")
data=read.table("RunsPerGame.txt",header=T,sep="\t")
attach(data)

pairs(data[,c(16,3:5)])
RGfit = lm(formula=R.G~AVG+OBP+SLG)
summary(RGfit)


RGfit1 = lm(formula=R.G~OBP+SLG)
summary(RGfit1)