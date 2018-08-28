#Rolling two dies
x=matrix(0,10000,1);
count=0;
for (i in (1:10000))
{
  Die1=sample(1:6,1);
  Die2=sample(1:6,1);
  x[i]=Die1+Die2;
}
hist(x,breaks=seq(1.5,12.5,1))