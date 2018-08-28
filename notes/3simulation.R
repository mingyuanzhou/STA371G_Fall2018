#R demo code for STA371G
#April 2014

#Simulate Market Return
Prob = c(0.12,0.40,0.25,0.15,0.08)
Return = c(0.23,0.18,0.15,0.09,0.03)
Mu = sum(Prob*Return)
Sigma = sqrt(sum(Prob*(Return-Mu)^2))
SimulatedReturn = sample(Return,size=1000,replace=TRUE,prob=Prob)
mean(SimulatedReturn)
sd(SimulatedReturn)
Mu
Sigma
par(mfrow=c(3,1))
plot(SimulatedReturn)
plot(Return,Prob,type="h",lwd=10)
hist(SimulatedReturn,breaks=seq(0.01,0.25,0.002))

#Sampling distribution of the sample mean
SampleMu = matrix(0,10000,1)
for (i in 1:10000){
  SimulatedReturn = sample(Return,size=1000,replace=TRUE,prob=Prob)
  SampleMu[i]=mean(SimulatedReturn)
}
par(mfrow=c(1,2)) 
plot(SampleMu)
abline(Mu,0,col='Red',lwd=3)
hist(SampleMu)
lines(c(Mu,Mu),c(0,4000),col='Red',lwd=3)
Sigma/sqrt(1000)/sd(SampleMu)

#######
(Sigma/sd(SampleMu))^2
#Sampling distribution of the sampling mean, how many samples???
########


#Uniform random number
plot(runif(1000))
hist(runif(100000))

#Uniform random number to discrete random number
#Flip a coin
x = runif(1)>0.5
#Flip a die 100 times
x= runif(100)>0.5
plot(x)
title(c("number of heads = ",sum(x)))
hist(x)

#Flip two coins
x1 = runif(1000)>0.5
x2 = runif(1000)>0.5
x= x1 + x2
plot(x)
title(c("both are tails = ",sum(x==0)))
hist(x)


#Toss a die
ceiling(runif(1)*6)
x = ceiling(runif(100)*6)
#x = sample(1:6,100,replace=TRUE)
plot(x)
hist(x,breaks=seq(0.5,6.5,1))

x = ceiling(runif(100000)*6)
hist(x,breaks=seq(0.5,6.5,1))

#Toss two dice
x1 = ceiling(runif(100)*6)
x2 = ceiling(runif(100)*6)
x=x1+x2;
hist(x,breaks=seq(1.5,12.5,1))


x1 = ceiling(runif(1000000)*6)
x2 = ceiling(runif(1000000)*6)
x=x1+x2;
hist(x,breaks=seq(1.5,12.5,1))

#Simulate Pi with 10, 100, 1000, or 10000 uniform random numbers
attach(mtcars)
par(mfrow=c(2,2))

for (n in c(10,100,1000,10000))
{
  x=runif(n)
  y=runif(n)
  Pi = sum(sqrt((x-0.5)^2+(y-0.5)^2)<0.5)/n*4
  plot(x,y,xlim=c(0,1),ylim=c(0,1),pch=19)
  lines(c(0,0),c(0,1))
  lines(c(1,0),c(0,0))
  lines(c(1,0),c(1,1))
  lines(c(1,1),c(1,0))
  
  r = 1/2
  theta = seq(-pi,pi,0.001)
  lines(r*cos(theta)+0.5,r*sin(theta)+0.5,type="s",lwd=2,col='red')
  title(c('Estimated pi=',Pi))
}



##Simulate Pi 100 times, each time using 1000 uniform random numbers
count=matrix(0,100,1)
for (iter in (1:100))
{
  x = runif(1000)
  y = runif(1000)
  for (i in (1:length(x)))
  {
    if (sqrt((x[i]-0.5)^2+(y[i]-0.5)^2)<=0.5)
    {
      count[iter]=count[iter]+1
    }
  }
}
count/length(x)*4
pi_estimate = count/length(x)*4
par(mfrow=c(1,2))
plot(pi_estimate)
abline(pi,0,col='Red',lwd=3)
hist(pi_estimate)
lines(c(pi,pi),c(0,800),col='Red',lwd=3)
pi_estimate_mu = mean(pi_estimate)
pi_estimate_mu-2*sd(pi_estimate)
pi_estimate_mu+2*sd(pi_estimate)
#mean(count/length(x)*4) - 2*sd(count/length(x)*4)/10
#mean(count/length(x)*4) + 2*sd(count/length(x)*4)/10




############
#Standard normal distribution
attach(mtcars)
par(mfrow=c(2,2))
?dnorm
x= seq(-5,5,0.01)
##probability density
plot(x,dnorm(x),type='s')

##cumulative density function (CDF)
plot(x,pnorm(x),type='s')

prob=seq(0.005,0.995,0.005)

##Inverse CDF
plot(prob,qnorm(prob))
plot(qnorm(prob),prob)

##normal random variables
plot(rnorm(1000))
hist(rnorm(10000))
x= rnorm(10000,0,1)
sum(x< 0)/10000
sum(x> -1 & x< 1)/10000
sum(x> -2 & x< 2)/10000
sum(x< -2)/10000
sum(x> -3 & x< 3)/10000
y=sort(x)
y[0.02*10000]

###Normal probability calculation
#P(X<0)=?
n=10000
x= rnorm(n,0.06,0.15)
sum(x< 0)/n
#P(X<?)=0.02
n=10000
x= rnorm(n,0.06,0.15)
y=sort(x)
y[round(0.02*n)]


#####
#Sampling distribution of the sample mean
par(mfrow=c(2,2))
n=10000
x= rnorm(n,33,10)
plot(x)
hist(x)
Xbar=matrix(0,1000,1)
for (i in 1:1000){
  Xbar[i]=mean(sample(x,size=16,replace=FALSE))
}
plot(Xbar)
hist(Xbar)  

###
#Central Limit Theorem
#Uniform number
par(mfrow=c(3,2))
n=10000
x= runif(n,0,100)
plot(x)
hist(x)
Xbar2=matrix(0,1000,1)
for (i in 1:1000){
  Xbar2[i]=mean(sample(x,size=2,replace=FALSE))
}
plot(Xbar2)
hist(Xbar2)  

Xbar10=matrix(0,1000,1)
for (i in 1:1000){
  Xbar10[i]=mean(sample(x,size=10,replace=FALSE))
}
plot(Xbar10)
hist(Xbar10) 

##An asymetric distribution, demonstrate central limit Theorem
par(mfrow=c(3,2))
Values=c(-5,-2,10)
Prob=c(0.8,0.1,0.1)
x=sample(Values,size=10000,replace=TRUE,prob=Prob)
plot(x)
hist(x)

Xbar10=matrix(0,800,1)
for (i in 1:800){
  Xbar10[i]=mean(sample(x,10,FALSE))
}
plot(Xbar10)
hist(Xbar10) 

Xbar100=matrix(0,800,1)
for (i in 1:800){
  Xbar100[i]=mean(sample(x,100,FALSE))
}
plot(Xbar100)
hist(Xbar100) 

mean(x)
sd(x)

mean(Xbar100)
sd(Xbar100)

sd(x)/sqrt(100)/sd(Xbar100)


plot(rnorm(2000,100,10))
attach(mtcars)
par(mfrow=c(2,2))
hist(rnorm(100,0,1),breaks=seq(-10,10,0.01))
hist(rnorm(1000,0,1),breaks=seq(-10,10,0.01))
hist(rnorm(10000,0,1),breaks=seq(-10,10,0.01))
hist(rnorm(1000000,0,1),breaks=seq(-10,10,0.01))

attach(mtcars)
par(mfrow=c(2,2))
hist(rnorm(10000,0,0.5),breaks=seq(-10,10,0.01))
hist(rnorm(10000,0,1),breaks=seq(-10,10,0.01))
hist(rnorm(10000,0,2),breaks=seq(-10,10,0.01))
hist(rnorm(10000,2,1),breaks=seq(-10,10,0.01))


par(mfrow=c(1,1))
x1=rnorm(10000)
x2=rnorm(10000)
x3=rnorm(10000)
x4=rnorm(10000)
hist((x1+x2+x3+x4)/2)



####Simulate Weekly Demand
Demand = 1:4
Prob = c(0.2,0.4,0.3,0.1)

SimulatedDemand = sample(x=Demand,size=100,replace=TRUE,prob=Prob)
plot(SimulatedDemand)
SimulatedDemand = sample(x=Demand,size=5000,replace=TRUE,prob=Prob)
plot(SimulatedDemand)
count = c(sum(SimulatedDemand==1),sum(SimulatedDemand==2),sum(SimulatedDemand==3),sum(SimulatedDemand==4))
print(count)
count/5000
sum(SimulatedDemand>2)/5000
mean(SimulatedDemand)
sum(Prob*Demand)

WeekDaysDemand=matrix(0,1000,5)
for (i in 1:1000){
  WeekDaysDemand[i,]=sample(x=Demand,size=5,replace=TRUE,prob=Prob)
}

WeekDemand = rowSums(WeekDaysDemand)
plot(WeekDemand)
hist(WeekDemand)
mean(WeekDemand)
sum(WeekDemand>12)/1000

#Disease example
#P(D=1|T=1) = 0.019/(0.019+0.0098)
#P(D=1|T=0) = 0.001/(0.001+0.9702)
Disease = runif(10^4)<=0.02
Test=matrix(0,1,length(Disease))
Test[Disease==TRUE] = runif(sum(Disease==TRUE))<=0.95
Test[Disease==FALSE] = runif(sum(Disease==FALSE))<=0.01

sum(Test==TRUE & Disease==TRUE)/sum(Test==TRUE)
sum(Test==FALSE & Disease==TRUE)/sum(Test==FALSE)

plot(Disease[1:1000])
plot(Test[1:1000]&Disease[1:1000])
plot(!Test[1:1000]&Disease[1:1000])

plot(Test[1:1000])
plot(Test[1:1000]&Disease[1:1000])
plot(Test[1:1000]&!Disease[1:1000])




