setwd("D:/CRSP") ###file directionary
library(readr)
######setup###########################################
closedate="Wednesday" ###choose Wednesday close price as the weekly close price
start=as.Date("1976-01-01")##start date of your dataset. Notice: If you set closedate as Wednesday, please use a Thursday date as the start date.
end=as.Date("2018-06-29")  ##end date of your dataset
######################################################
daily=read_csv("daily.csv") ### dataset from CRSP with date,vwretd,RET,PERMNO
###########calculate weekly value weighted market return
mkt=subset(daily,select=c("date","vwretd"))
mkt=mkt[!duplicated(mkt),]
mkt$date=as.Date(mkt$date,format=c("%d/%m/%Y"))
daily$date=as.Date(daily$date,format=c("%d/%m/%Y"))
daily$RET=as.numeric(as.character(daily$RET))
daily=daily[!is.na(daily$RET),]
x=seq(start,end,by=1)
y=c(1:(as.numeric(end)-as.numeric(start)+1))
z=data.frame(cbind(x,y))
names(z)=c("date","count") ###z stores the calendar dates and their counts
z$date=as.Date(z$date,origin="1970-01-01")
z$weekday=weekdays(z$date)  ### get the weekday of the calendar dates
z$weeks=trunc((z$count-1)/7) ### get the week counts of the calendar dates
use1=subset(z,weekday==closedate)
use1=subset(use1,select=c("date","weeks")) ### get the weekly closedates
use2=subset(z,select=c("date","weeks"))
mkt2=merge(mkt,use2,by="date")
mkt2$vwretd=mkt2$vwretd+1
mkt3=aggregate(vwretd~weeks,prod,data=mkt2)### get the cumulated weekly return
mkt3$vwretd=mkt3$vwretd-1
mkt4=merge(mkt3,use1,by="weeks") ####final data for weekly market return
###calculate weekly stock return
daily$count=as.numeric(daily$date)-as.numeric(use2$date[1])+1
daily=subset(daily,count>0)
daily$weeks=trunc((daily$count-1)/7)
daily$RET=daily$RET+1
weekly=aggregate(RET~PERMNO+weeks,prod,data=daily)### get the cumulated weekly return
weekly$RET=weekly$RET-1
weekly2=merge(weekly,mkt4,by="weeks")
weekly2=weekly2[,-1]
write.csv(weekly2,"weekly.csv",row.names=FALSE)###final dataset
library(foreign)
write.dta(weekly2,"weekly.dta")   ###output for STATA
