# Spring 2014, 04635 Enrollment = 71, 12:30-2:00 PM, as of 01/13/2014
# Spring 2014, 04640 Enrollment = 61,  2:00-3:30 PM, as of 01/13/2014
# Assuming it is true that the students of STA371G (04635 and 04640) 
# do not prefer the early afternoon session to the late afternoon one, 
# would it be suprising that the early afternoon session has 71 students 
# whereas the later one has only 61 students?

attach(mtcars)
par(mfrow=c(1,2))

Total = 71+61
Session1 = matrix(0,1,10000)
Session2 = matrix(0,1,10000)
SessionDifference = matrix(0,1,10000)
for (i in (1:10000))
{
  Session1[i] = sum(runif(Total)>0.5)
  Session2[i] = Total-Session1[i]
  SessionDifference[i] = (Session1[i]-Session2[i])
}
sum(SessionDifference>=10)/10000
hist(SessionDifference)

## If I find the difference of 80 for this year and the next 7 years combined, shall I accept the Null Hypothesis?
Total = (71+61)*8
Session1 = matrix(0,1,10000)
Session2 = matrix(0,1,10000)
SessionDifference = matrix(0,1,10000)
for (i in (1:10000))
{
  Session1[i] = sum(runif(Total)>0.5)
  Session2[i] = Total-Session1[i]
  SessionDifference[i] = (Session1[i]-Session2[i])
}
sum(SessionDifference>=10*8)/10000
hist(SessionDifference)
