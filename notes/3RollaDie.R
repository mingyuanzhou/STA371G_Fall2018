#Flipping a coin
prob=matrix(0,100000,1);
count=0;
for (i in (1:100000))
{
  Head=sample(0:1,1);
  if (Head==1)
  {
    count=count+1;
  }
  prob[i]=count/i;
}
plot(prob,type="s",log="x")
#plot(prob,type="s")
lines(c(1,i),c(1/2,1/2))


#Rolling a die
prob=matrix(0,10^6,1);
count=0;
for (i in (1:10^6))
{
  Die=sample(1:6,1);
  if (Die==6)
  {
    count=count+1;
  }
  prob[i]=count/i;
}
plot(prob,type="s",log="x",main="Toss a die 1 million times",
     xlab="Number of tosses",ylab="Proportion of six")
#plot(prob,type="s")
lines(c(1,i),c(1/6,1/6))


