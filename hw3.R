#Xiuqi Wang
#HW3

#1
rm(list=ls())
setwd('C:\\Users\\wxq\\Desktop\\supply chain\\Class3')
customerorders = read.table("Lokad_Orders.tsv", sep="\t", head=T)
attach(customerorders)
UnitCost = NetAmount/Quantity
hist(UnitCost,breaks=100)

#2
library(dplyr)
ordersubset=filter(customerorders,NetAmount/Quantity<100)

#3
lm_unit_cost = lm(ordersubset$NetAmount ~ ordersubset$Quantity)
summary(lm_unit_cost)
#R-squared is too low, just 0.1095, which means this model doesn't interpret the data well.

#4
par(mfrow = c(2,2))
plot(lm_unit_cost)
#Residuls vs Fitted: The points don't fit the model well and most points are above the line.')
#Normal Q-Q: The residuals are not normal distributed.')
#Scale-Location: The line fits some data but the others are far from the line. That's because orders have different unit costs.')
#Residuals vs Leverage: Cook's distance of most points are below 0.5. But a few points has Cook's points far above 1.')
#Summary: The model is not appropriate. We should use several linear models for orders with different unit costs.')

#5
library(forecast)
PurchaseDate = as.Date(Date, "%d-%m-%y")
mo = strftime(PurchaseDate, "%m")
yr = strftime(PurchaseDate, "%Y")
date = paste(mo, "01", yr, sep="-")
date = as.Date(date, "%m-%d-%Y")
agg_NetAmt = aggregate(NetAmount ~ date, FUN = sum)
ses_model=ses(agg_NetAmt$NetAmount, h=5,level=0.95)
par(mfrow = c(1,1))
plot(ses_model)
summary(ses_model)
#95% prediction interval for last month is between 239523.3 and 396997.4.
