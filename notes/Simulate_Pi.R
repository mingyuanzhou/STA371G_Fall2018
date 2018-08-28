#R Code for STA371G
#Mingyuan Zhou
#January 23, 2014
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
