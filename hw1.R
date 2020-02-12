#Xiuqi Wang
#Assignment1

#2
rm(list=ls())
#3
setwd('C:\\Users\\wxq\\Desktop\\supply chain\\HW1')
#4
realestate = read.csv("datagovbldgrexus.csv",header = TRUE)
#5
summary(realestate)
#6
dim(realestate)
#7
attach(realestate)
#8
length(unique(Region.Code))
#9
hist(Total.Parking.Spaces, main = 'Parking Space Histogram',ylab = 'Frequency', xlab = 'Number of Spots')
#10
tapply(Property.Type  == 'BUILDING', Bldg.State , sum)
#11
tapply(Bldg.ANSI.Usable, Bldg.State,mean)
#12
plot(Bldg.ANSI.Usable, Total.Parking.Spaces, col = 'blue', xlab = 'Usable', ylab = 'Total')