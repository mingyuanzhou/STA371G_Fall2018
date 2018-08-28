Session = 04635
# Session = 04640
if (Session == 04635){
  #04635 12:30-2:00 PM, Enrollment = 71,  as of 01/13/2014
  #Student Last Name Length
  NameLen = c(4,8,4,3,8,5,7,4,7,5,4,5,6,5,6,9,6,8,6,7,8,7,6,8,
              6,5,7,6,5,6,5,5,8,7,3,6,3,4,3,8,4,6,6,5,5,5,9,7,
              2,3,5,5,4,4,7,3,7,6,5,7,6,6,6,9,7,8,7,6,3,15,6)
} else{
  # 04640 12:30-2:00 PM, Enrollment = 61, as of 01/13/2014
  # Student Last Name Length
  NameLen = c(7,8,3,6,9,6,7,7,4,6,7,7,7,6,4,7,6,7,8,6,6,2,2,6,
              8,8,6,4,6,6,7,9,3,3,5,3,5,2,8,8,7,7,7,4,10,6,12,
              5,8,10,7,6,4,6,7,7,4,3,5,8,5)
}

##Create 4 subfgures
par(mfrow=c(2,2))

hist(NameLen,breaks=seq(0.5,20.5,0.2))

c(mean(NameLen)-2*sd(NameLen)/sqrt(4),mean(NameLen)+2*sd(NameLen)/sqrt(4))

Ave2=matrix(0,1,1000)
for (i in (1:1000))
{
  dex= sample(70,2,replace=FALSE)
  Ave2[i]=sum(NameLen[dex])/2;
}
hist(Ave2,breaks=seq(0,20.5,0.20))
CI = c(mean(NameLen)-2*sd(NameLen)/sqrt(2),mean(NameLen)+2*sd(NameLen)/sqrt(2));
lines(c(CI[1],CI[1]),c(0,100) ,col="Red" )
lines(c(CI[2],CI[2]),c(0,100) ,col="Red" )

Ave5=matrix(0,1,1000)
for (i in (1:1000))
{
  dex= sample(70,5,replace=FALSE)
  Ave5[i]=sum(NameLen[dex])/5;
}
hist(Ave5,breaks=seq(0,20.5,0.20))
CI = c(mean(NameLen)-2*sd(NameLen)/sqrt(5),mean(NameLen)+2*sd(NameLen)/sqrt(5));
lines(c(CI[1],CI[1]),c(0,100) ,col="Red" )
lines(c(CI[2],CI[2]),c(0,100) ,col="Red" )


Ave10=matrix(0,1,1000)
for (i in (1:1000))
{
  dex= sample(70,10,replace=FALSE)
  Ave10[i]=sum(NameLen[dex])/10;
}
hist(Ave10,breaks=seq(0,20.5,0.20))
CI = c(mean(NameLen)-2*sd(NameLen)/sqrt(10),mean(NameLen)+2*sd(NameLen)/sqrt(10));
lines(c(CI[1],CI[1]),c(0,100) ,col="Red" )
lines(c(CI[2],CI[2]),c(0,100),col="Red" )