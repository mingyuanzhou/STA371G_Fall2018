x = seq(-4,4,0.01)
plot(x,pnorm(x),type="s")
lines(x,dnorm(x))
lines(x,pnorm(x,1,1/2),col="red")
lines(x,dnorm(x,1,1/2),col="red")


plot(x,punif(x),type="s")
lines(x,dunif(x),col="red")
