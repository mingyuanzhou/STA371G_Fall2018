#Case Study, Amore Forzen Food
#STA 371G, Statistics and Modeling, Spring 2014
#McCombs School of Business, The University of Texas at Austin
#Mingyuan Zhou

#Standard deviation is 0.22 ounce
sigma = 0.22

#Number of dozen pies per 20-minute batch 
ProductionRate = 1000

#Number of Batches per month
BatchPerMonth = 60000/ProductionRate

#price per dozen pies
WhoseSalePrice = 4.50
ThriftStorePrice = 3.60

#Cost per dozen pies
Cost = 3.00

#Sample size
n=5


FillingTargets=seq(7.7,9,0.01)
Profit = matrix(0,1,length(FillingTargets))

for (i in (1:length(FillingTargets)))
{
  x=FillingTargets[i]
  
  #The probability for a batch to be sent to the thrift store
  p = pnorm(8.00,x,sigma/sqrt(n))
  
  #Expected number of accepted batches
  AcceptedBatch = (1-p)*BatchPerMonth*12
  
  #Expected number of rejected batches 
  RejectedBatch = p*BatchPerMonth*12
  
  #Expected revenue for selling accepted batches
  WholeSaleRevenue = AcceptedBatch * 1000 * WhoseSalePrice 
  
  #Annual demand at the thrift store, 60 per week * 52 weeks per year
  AnnualDemandThrift = 60*52
  
  #Expected revenue for selling rejected batches
  if(RejectedBatch*1000 > AnnualDemandThrift){
    ThriftStoreRevenue = AnnualDemandThrift * ThriftStorePrice 
    DonatedDozenpies = RejectedBatch*1000-AnnualDemandThrift
  } else{
    ThriftStoreRevenue = RejectedBatch * 1000 * ThriftStorePrice 
    DonatedDozenpies = 0
  }
  
  #annual ingredient costs
  IngredientCost = BatchPerMonth*12*1000*(x/8.44)*1.82
  #annual package costs
  PackageCost = BatchPerMonth*12*1000*0.62
  #annual labor and overhead costs
  OtherCost = BatchPerMonth*12*1000*(0.07+0.13+0.36)
  
  #Expected profit
  ExpectedProfit = WholeSaleRevenue + ThriftStoreRevenue - IngredientCost-PackageCost-OtherCost
  
  x
  p
  AcceptedBatch
  RejectedBatch
  WholeSaleRevenue
  ThriftStoreRevenue 
  DonatedDozenpies
  IngredientCost
  PackageCost
  OtherCost
  ExpectedProfit
  
  Profit[i]=ExpectedProfit
}

plot(FillingTargets,Profit)
#Best Filling Target
BestTarget = FillingTargets[which.max(Profit)]
#Profit at the best filling target
MaxProfit = max(Profit)



