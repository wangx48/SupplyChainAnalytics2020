#Xiuqi Wang
#HW2

#2
rm(list=ls())
#3
setwd('C:\\Users\\wxq\\Desktop\\supply chain\\HW2')
#4
purchaseorders=read.table('Lokad_PurchaseOrders.tsv', sep='\t', header = T)
#5
attach(purchaseorders)
partial=ifelse(Received<Quantity*0.1,'Partial Delivery','Not Partial')
purchaseorders = cbind(purchaseorders,partial)
#6a
PODate = as.Date(Date, "%d-%m-%y")
Netdollar=ifelse(Currency == "GBP", NetAmount * 1.30, 
                ifelse(Currency == "EUR", NetAmount * 1.11,
                       NetAmount))
plot(Netdollar~PODate,col = ifelse(partial == "Partial Delivery", "red", "green"))

#6b
par(mfrow=c(3,1))
plot(Netdollar[Loc=='New-York']~PODate[Loc=='New-York'],col = ifelse(partial == "Partial Delivery", "red", "green"))
plot(Netdollar[Loc=='Chicago']~PODate[Loc=='Chicago'],col = ifelse(partial == "Partial Delivery", "red", "green"))
plot(Netdollar[Loc=='Los Angeles']~PODate[Loc=='Los Angeles'],col = ifelse(partial == "Partial Delivery", "red", "green"))

#6c
NetAmount_new=ifelse(partial == "Partial Delivery",(Quantity-Received)*Netdollar/Quantity,0)
purchaseorders = cbind(purchaseorders, NetAmount_new)
supplydata = tapply(NetAmount_new,Supplier,FUN=sum)
which.max(supplydata)
print('Drecom has the most partially delivered ')

#6d
Unitcost = 	ifelse(Currency == "GBP", NetAmount/Quantity * 1.30, 
                     ifelse(Currency == "EUR", NetAmount/Quantity * 1.11,
                            NetAmount/Quantity))
hist(Unitcost[Loc=='New-York'])
hist(Unitcost[Loc=='Chicago'])
hist(Unitcost[Loc=='Los Angeles'])
print('There is no significance between their distribution, but the frequency of each city varies.
New York has the largest number of records and Los Angeles is the lowest.')


#7a
count = 0
while (rnorm(1)<=3){
  count=count+1
}
count

#7b
mat <- matrix(nrow=1000,ncol=4)
for (i in 1:1000){
  mat[i,1]=rnorm(1,30,5)
  mat[i,2]=rnorm(1,20,8)
  mat[i,3]=rnorm(1,40,12)
  mat[i,4]=mat[i,1]+mat[i,2]+mat[i,3]
}
mean(mat[,4])
sd(mat[,4])