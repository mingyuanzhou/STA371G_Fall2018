Data = read.table("SalaryData.txt",header=T)
attach(Data)

plot(Gender,Salary,col=4,lwd=1.5,main="Salary Data")
NumYears = 95-YrHired

plot(NumYears,Salary,xlab="Year Hired")
points(NumYears[Gender=="Male"],Salary[Gender=="Male"],col=4,pch=19)
points(NumYears[Gender=="Female"],Salary[Gender=="Female"],col=2,pch=24)

model1 = lm(Salary~Gender,data=Data)

model2 = lm(Salary~Gender+NumYears,data=Data)
plot(NumYears,Salary,xlab="Number of Years")
abline(coef=model2$coef[c(1,3)],col=2,lwd=2)
points(NumYears[Gender=="Female"],Salary[Gender=="Female"],col=2,pch=19)
abline(coef=c(model2$coef[1]+model2$coef[2],model2$coef[3]),col=4,lwd=2)
points(NumYears[Gender=="Male"],Salary[Gender=="Male"],col=4,pch=19)


plot(NumYears,model2$residuals,xlab="Number of Years")
points(NumYears[Gender=="Female"],model2$residuals[Gender=="Female"],col=2,pch=19)
points(NumYears[Gender=="Male"],model2$residuals[Gender=="Male"],col=4,pch=19)


Ed1=(EducLev==1 | EducLev==2);
Ed2=(EducLev==3 | EducLev==4);
Ed3=(EducLev==5);

model4 = lm(Salary~Gender+NumYears+Ed2+Ed3)
plot(NumYears,Salary,xlab="Number of Years")


#Female, Ed=1
abline(coef=model4$coef[c(1,3)],col=2,lwd=2)
points(NumYears[Gender=="Female"&Ed1],Salary[Gender=="Female"&Ed1],col=2,pch=19)

#Female, Ed=2
abline(coef=c(model4$coef[1]+model4$coef[4],model4$coef[3]) ,col=3,lwd=2)
points(NumYears[Gender=="Female"&Ed2],Salary[Gender=="Female"&Ed2],col=3,pch=19)

#Female, Ed=3
abline(coef=c(model4$coef[1]+model4$coef[5],model4$coef[3]) ,col=4,lwd=2)
points(NumYears[Gender=="Female"&Ed3],Salary[Gender=="Female"&Ed3],col=4,pch=19)

#Male, Ed=1
abline(coef=c(model4$coef[1]+model4$coef[2],model4$coef[3]),col=5,lwd=2)
points(NumYears[Gender=="Male"&Ed1],Salary[Gender=="Male"&Ed1],col=5,pch=24)

#Male, Ed=2
abline(coef=c(model4$coef[1]+model4$coef[2]+model4$coef[4],model4$coef[3]) ,col=6,lwd=2)
points(NumYears[Gender=="Male"&Ed2],Salary[Gender=="Male"&Ed2],col=6,pch=24)

#Male, Ed=3
abline(coef=c(model4$coef[1]+model4$coef[2]+model4$coef[5],model4$coef[3]) ,col=7,lwd=2)
points(NumYears[Gender=="Male"&Ed3],Salary[Gender=="Male"&Ed3],col=7,pch=24)


plot(NumYears,model4$residuals,xlab="Number of Years")
points(NumYears[Gender=="Female"],model4$residuals[Gender=="Female"],col=2,pch=19)
points(NumYears[Gender=="Male"],model4$residuals[Gender=="Male"],col=4,pch=19)



#model3 = lm(Salary~Gender*NumYears,data=Data)
model3 = lm(Salary~Gender+NumYears+Gender*NumYears,data=Data)
plot(NumYears,Salary,xlab="Number of Years")
abline(coef=model3$coef[c(1,3)],col=2,lwd=2)
points(NumYears[Gender=="Female"],Salary[Gender=="Female"],col=2,pch=19)
abline(coef=c(model3$coef[1]+model3$coef[2],model3$coef[3]+model3$coef[4]),col=4,lwd=2)
points(NumYears[Gender=="Male"],Salary[Gender=="Male"],col=4,pch=19)



plot(NumYears,model3$residuals,xlab="Number of Years")
points(NumYears[Gender=="Female"],model3$residuals[Gender=="Female"],col=2,pch=19)
points(NumYears[Gender=="Male"],model3$residuals[Gender=="Male"],col=4,pch=24)



model5 = lm(Salary~Gender*NumYears+Ed2+Ed3)
plot(NumYears,Salary,xlab="Number of Years")





#Female, Ed=1
abline(coef=model5$coef[c(1,3)],col=2,lwd=2)
points(NumYears[Gender=="Female"&Ed1],Salary[Gender=="Female"&Ed1],col=2,pch=19)

#Female, Ed=2
abline(coef=c(model5$coef[1]+model5$coef[4],model5$coef[3]) ,col=3,lwd=2)
points(NumYears[Gender=="Female"&Ed2],Salary[Gender=="Female"&Ed2],col=3,pch=19)

#Female, Ed=3
abline(coef=c(model5$coef[1]+model5$coef[5],model5$coef[3]) ,col=4,lwd=2)
points(NumYears[Gender=="Female"&Ed3],Salary[Gender=="Female"&Ed3],col=4,pch=19)

#Male, Ed=1
abline(coef=c(model5$coef[1]+model5$coef[2],model5$coef[3]+model5$coef[6]),col=5,lwd=2)
points(NumYears[Gender=="Male"&Ed1],Salary[Gender=="Male"&Ed1],col=5,pch=24)

#Male, Ed=2
abline(coef=c(model5$coef[1]+model5$coef[2]+model5$coef[4],model5$coef[3]+model5$coef[6]) ,col=6,lwd=2)
points(NumYears[Gender=="Male"&Ed2],Salary[Gender=="Male"&Ed2],col=6,pch=24)

#Male, Ed=3
abline(coef=c(model5$coef[1]+model5$coef[2]+model5$coef[5],model5$coef[3]+model5$coef[6]) ,col=7,lwd=2)
points(NumYears[Gender=="Male"&Ed3],Salary[Gender=="Male"&Ed3],col=7,pch=24)



plot(NumYears,model5$residuals,xlab="Number of Years")
points(NumYears[Gender=="Female"],model5$residuals[Gender=="Female"],col=2,pch=19)
points(NumYears[Gender=="Male"],model5$residuals[Gender=="Male"],col=4,pch=19)


Job1=JobGrade==1
Job2=JobGrade==2
Job3=JobGrade==3
Job4=JobGrade==4
Job5=JobGrade==5
Job6=JobGrade==6
model6 = lm(Salary~Gender*NumYears+Ed2+Ed3+Job2+Job3+Job4+Job5+Job6)

plot(NumYears,Salary,xlab="Number of Years")
points(NumYears[Job1],Salary[Job1],col=2,pch=24)
points(NumYears[Job2],Salary[Job2],col=3,pch=24)
points(NumYears[Job3],Salary[Job3],col=4,pch=24)
points(NumYears[Job4],Salary[Job4],col=5,pch=24)
points(NumYears[Job5],Salary[Job5],col=6,pch=24)
points(NumYears[Job6],Salary[Job6],col=7,pch=24)


plot(NumYears,model6$residuals,xlab="Number of Years")
points(NumYears[Gender=="Female"],model6$residuals[Gender=="Female"],col=2,pch=19)
points(NumYears[Gender=="Male"],model6$residuals[Gender=="Male"],col=4,pch=19)


