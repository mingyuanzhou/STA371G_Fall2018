par(mfrow=c(2,2))
x = rnorm(10000,200,100)
plot(x)
hist(x)
mean_x = mean(x)
var_x = sum((x-mean_x)^2)/(length(x)-1)
sd_x = sqrt(var_x)

y=matrix(0,1000,1)
for (i in (1:1000)){
  y[i]=mean(sample(x,100,replace=TRUE))
}
plot(y)
hist(y)
sample_mean_y = mean(y)
sample_var_y = sum((y-sample_mean_y)^2)/(length(y)-1)
sample_sd_y=sqrt(sample_var_y)

mean_x 
sample_mean_y
var_x
sample_var_y