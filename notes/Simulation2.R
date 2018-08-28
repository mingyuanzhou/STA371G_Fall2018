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

WeekDaysDemand=matrix(0,10000,5)
for (i in 1:10000){
  WeekDaysDemand[i,]=sample(x=Demand,size=5,replace=TRUE,prob=Prob)
}

WeekDemand = rowSums(WeekDaysDemand)
plot(WeekDemand)
hist(WeekDemand)
mean(WeekDemand)
sum(WeekDemand>12)/10000

par(mfrow=c(1,2))
##Find the losses for ordering 12 and 16
Loss12=matrix(0,10000,1)
for (i in 1:10000){
  Loss12[i]=lossf(WeekDemand[i],12)
}
hist(Loss12,breaks=0:80,freq=FALSE,ylim=c(0,0.4),col='blue')  
mean(Loss12)

Loss16=matrix(0,10000,1)
for (i in 1:10000){
  Loss16[i]=lossf(WeekDemand[i],16)
}
hist(Loss16,breaks=0:80,freq=FALSE,ylim=c(0,0.4),col='red')  
mean(Loss16)

lossf = function(weekdemand,inventory) {
  if(weekdemand>inventory) { # understocked
    return(10*(weekdemand-inventory))
  } else { #overstocked
    if((inventory-weekdemand)==1) return(0)
    if((inventory-weekdemand)==0) return(0)
    return(2*(inventory-weekdemand))
  }
  return(0)
}


########Freemark Abbey##########

HarvestNow = FALSE
NumBottle = 12000


par(mfrow=c(1,2))
ProbStormHit = 0.5
ProbFormMold = 0.4
ProbGrapeGrade = c(0.4,0.4,0.2)
GrapeGrade = c(1,2,3)
PriceGrapeGrade = c(3.5,3.0,2.5)
count=0;
prob = matrix(0,1,2*2*3)
profit = matrix(0,1,2*2*3)
for (StormHit in c(FALSE,TRUE))
{
  if (StormHit==TRUE)
  {
    for (FormMold in c(TRUE, FALSE))
    {
      if (FormMold==TRUE)
      {
        count=count+1
        prob[count]=ProbStormHit*ProbFormMold
        profit[count]=8*12000*0.7
      }
      else
      {
        count=count+1
        prob[count]=ProbStormHit*(1-ProbFormMold)
        profit[count]=1*12000
      }  
    }
  }
  else
    for (GrapeGrade in c(1,2,3))
    {
      count=count+1
      prob[count]=(1-ProbStormHit)*ProbGrapeGrade[GrapeGrade]
      profit[count]=12000*PriceGrapeGrade[GrapeGrade]
    }
}
prob = prob[c(1:count)]
profit=profit[c(1:count)]

sum(prob*profit)
plot(profit,prob,type='h',lwd=10,ylim=c(0, 0.5))



ProbStormHit = 0.5
ProbFormMold = 0.4
par(mfrow=c(3,3))
for (ProbStormHit  in c(0.1,0.5,0.9)){
  for (ProbFormMold  in c(0.1,0.4,0.7)){
    Profit = matrix(0,10000,1)
    for (iter in (1:length(Profit)))
    {
      
      NumBottleDownPercent = 0
      StormHit = runif(1)<=ProbStormHit
      if (HarvestNow==TRUE)
      {
        PircePerBottle = 2.85
      }
      else
      {
        if (StormHit==TRUE)
        {
          FormMold = runif(1)<=ProbFormMold
          if (FormMold==TRUE)
          {
            PricePerBottle = 8
            NumBottleDownPercent = 0.3
          }
          else
          {
            PricePerBottle = 1
          }
        }
        else
        {
          GrapeGrade = sample(3,1,prob=c(0.4,0.4,0.2))
          if (GrapeGrade==1)
          {
            PricePerBottle = 3.5
          }
          else if (GrapeGrade==2)
          {
            PricePerBottle = 3.0
          }
          else
          {
            PricePerBottle = 2.5
          }
        }
      }
      Profit[iter] = PricePerBottle*(1-NumBottleDownPercent)*NumBottle
      
    }
    hist(Profit,breaks=20,main=c(ProbStormHit,ProbFormMold))
    mean(Profit)
  }
}


######Portfolio analysis
Weights =  c(10500, 16300,	9600,	9300,	9500,	15400,	14300,	15100)
Mu =  c(10.1,7.3,	11.8,	9.9,	11.8,	9.1,	9.6,	12.3)/100
Sigma=matrix(
  +c(0.0154,  0.0047,  0.0061,	0.0107,	0.0157,	0.0120,	0.0077,	0.0121,
     +0.0047,	0.0142,	0.0065,	0.0131,	0.0081,	0.0151,	0.0128,	0.0099,
     +0.0061,	0.0065,	0.0180,	0.0062,	0.0182,	0.0081,	0.0051,	0.0163,
     +0.0107,	0.0131,	0.0062,	0.0199,	0.0151,	0.0112,	0.0080,	0.0164,
     +0.0157,	0.0081,	0.0182,	0.0151,	0.0250,	0.0146,	0.0075,	0.0148,
     +0.0120,	0.0151,	0.0081,	0.0112,	0.0146,	0.0253,	0.0165,	0.0094,
     +0.0077,	0.0128,	0.0051,	0.0080,	0.0075,	0.0165,	0.0128,	0.0128,
     +0.0121,	0.0099,	0.0163,	0.0164,	0.0148,	0.0094,	0.0128,	0.0303),
  +8,8)+diag(0.005,8,8)
library(MASS)
Return=matrix(0,1,10000)
for (iter in (1:10000)){
  Return[iter] = sum(Weights*mvrnorm(n=1,Mu,Sigma))
}
hist(Return)
#Truth
c(sum(Weights*Mu), sqrt(Weights%*%Sigma%*%Weights))
#Simulated
c(mean(Return), sd(Return))
sum(Return<0)/10000
sum(Return>30000)/10000



###Simulate number of non-TX residents in a sample of 60 from a population of 4000
par(mfrow=c(1,4))
plot(dhyper(0:20,520,3480,60),type='s')
sum(dhyper(11:60,520,3480,60))

McCombs=c(matrix(1,1,870*3),matrix(0,1,130*3)) #4000 students, 87% of which are from Texas
#Find the distribution of the number of non-Texas residents
NotTX = matrix(0,10000,1)
for (iter in 1:10000){
  NotTX[iter]=60-sum(sample(x=McCombs,size=60,replace=FALSE))
}
hist(NotTX,freq=FALSE,breaks=0:20)
sum(NotTX>10)/10000

NotTX_bino = rbinom(n=10000,size=60,prob=1-0.87)
hist(NotTX_bino,freq=FALSE,breaks=0:20)
sum(NotTX_bino>10)/10000

NotTX_normal = rnorm(10000,60*(1-0.87),sqrt(60*0.87*(1-0.87)))
NotTX_normal[NotTX_normal<0]=0
hist(NotTX_normal,freq=FALSE,breaks=0:20)
sum(NotTX_normal>10.5)/10000

  

###Free throws###
#p is the probability to sucessfully make a free throw
#Probability to sucessfully make 10 consecutive free throws
par(mfrow=c(2,2))
for (p in c(0.2,0.4,0.6,0.8))
{
  x = matrix(0,10000,1)
  for (iter in 1:10000){
    count=0
    while (runif(1)<p){
      count = count+1
    }
    x[iter]=count
  }
  hist(x,freq=FALSE,breaks=seq(-0.5,max(x)+0.5,1),main=c(p,sum(x >= 10)/10000))
}


###Simulate t distribution####
par(mfrow=c(2,2))
for (n in c(4,7,10,13)){
  t=matrix(0,10000,1)
  for (iter in 1:10000){
    mu=runif(1)*100
    sigma=runif(1)*100
    x=rnorm(n,mu,sigma)
    t[iter]=(mean(x)-mu)/(sd(x)/sqrt(n))
  }
  hist(t,freq=FALSE,breaks=seq(-0.5+min(t),max(t)+0.5,0.25),main=n-1)
}


###Simulate Random Walk
par(mfrow=c(1,2))
mu=0
sigma=0.01
Y=matrix(0,365,1)
for (i in 1:364){ 
  Y[i+1]=Y[i]+rnorm(1,mu,sigma)
}
plot(Y)
plot(Y[2:length(Y)]-Y[1:length(Y)-1])




#Simulate BostonRobbery
library(zoo)
library(rdatamarket)
BostonRobbery= coredata(dmseries("http://data.is/1nUVO39"))

par(mfrow=c(1,1))
Y=matrix(0,118,1)
Y[1]=41
b0=-4.0675770
b1=0.5639723
b2=1.5612420
s=36.7
for (i in 1:117){ 
  Y[i+1]=b0+b1*Y[i]+b2*i+rnorm(1,0,s)
  if (Y[i+1]<0){
    Y[i+1]=0
  }
}

#Simulate BostonRobbery Log Transformation
plot(BostonRobbery,type='b',col='Blue',pch=19)
lines(Y,col='red',lwd=3)



LogY=matrix(0,118,1)
LogY[1]=log(41)
b0=1.329217172 
b1=0.648914825 
b2=0.007597908 
s=0.1898
for (i in 1:117){ 
  LogY[i+1]=b0+b1*LogY[i]+b2*i+rnorm(1,0,s)
}
Y=exp(LogY)

plot(BostonRobbery,type='b',col='Blue',pch=19)
lines(Y,col='red',lwd=3)


#Fitting and Prediction BostonRobbery Log Transformation
plot(BostonRobbery,type='b',col='Blue',pch=19)
lines(Y,col='red',lwd=3)

LogY=matrix(0,118+12,1)
LogY[1]=log(41)
b0=1.329217172 
b1=0.648914825 
b2=0.007597908 
s=0.1898
for (i in 1:117){ 
  LogY[i+1]=b0+b1*log(BostonRobbery[i])+b2*i
  #+rnorm(1,0,s)
}
for (i in 118:(118+11)){ 
  LogY[i+1]=b0+b1*LogY[i]+b2*i
  #+rnorm(1,0,s)
}
Y=exp(LogY)

plot(Y,col='red',lwd=3,type='l')
lines(BostonRobbery,type='b',col='Blue',pch=19)

LogX=matrix(0,10000,12)
for (iter in 1:10000){
  LogX[iter,1]=b0+b1*LogY[118]+b2*i+rnorm(1,0,s)
  for (i in 119:(118+11)){
    LogX[iter,i-118+1]=b0+b1*LogX[iter,i-118]+b2*i+rnorm(1,0,s)
  }
}
par(mfrow=c(2,4))
plot(exp(LogX[,1]))
plot(exp(LogX[,4]))
plot(exp(LogX[,8]))
plot(exp(LogX[,12]))
     
hist(exp(LogX[,1]))
hist(exp(LogX[,4]))
hist(exp(LogX[,8]))
hist(exp(LogX[,12]))


par(mfrow=c(1,2))
sigma_Log = matrix(0,12,1)
sigma = matrix(0,12,1)
for (i in 1:12){
  sigma_Log[i]=sd((LogX[,i]))
  sigma[i]=sd(exp(LogX[,i]))
}
plot(sigma_Log)
plot(sigma)

par(mfrow=c(1,1))
Y_low = Y
Y_high = Y
for (i in 1:117){ 
  Y_low[i+1]=exp(LogY[i+1]-2*s)
  Y_high[i+1]=exp(LogY[i+1]+2*s)
  #+rnorm(1,0,s)
}
for (i in 118:(118+11)){ 
  Y_low[i+1]=Y[i+1]-2*sigma[i-117]
  Y_high[i+1]=Y[i+1]+2*sigma[i-117]
  #+rnorm(1,0,s)
}

plot(Y_high,col='orange',lwd=3,type='l')
lines(Y,col='red',lwd=3,type='l')
lines(Y_low,col='orange',lwd=3,type='l')
lines(BostonRobbery,type='b',col='Blue',pch=19)
