#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
library(zoo)
library(data.table)
library(ggplot2)

rm(list=ls())
start_time <- Sys.time()

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# Functions
#---------+---------+---------+---------+---------+---------+
na.replace = function(v,value=0) { v[is.na(v)] = value; v }


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
load(paste0(working_data_dir, "/portland_panel.RData"))


#---------+---------+---------+---------+---------+---------+
# Create Data for Analysis
#---------+---------+---------+---------+---------+---------+
dt = data.table(portland_panel)
setkeyv(dt,c("ACCOUNT_NO","DUE_DT"))

# Date variables
dt[,year:=year(dt$DUE_DT)]
dt[,quarter:=quarter(dt$DUE_DT)]
# fix missing
dt[,discount_assistance:=na.replace(discount_assistance)]
dt[,crisis_voucher_amount:=na.replace(crisis_voucher_amount)]
# payment and revenue variables
dt[,total_payments:=-total_payments]
dt[,crisis_voucher_amount:=-crisis_voucher_amount]
dt[,discount_assistance:=-discount_assistance]
dt[,OOP_payments:=total_payments-crisis_voucher_amount]
dt[,Rev:=total_payments+leftover_debt]
dt[,RevRate:=total_payments/Rev*100]
dt[,discounts_and_vouchers:=discount_assistance+crisis_voucher_amount]
dt[,unpaid_debt:=leftover_debt-discounts_and_vouchers]
dt[,date:=paste(format(DUE_DT, "%Y"),  # Convert dates to quarterly
                sprintf("%02i", (as.POSIXlt(DUE_DT)$mon) %/% 3L + 1L), 
                sep = "/")]

# censor unreliable outliers
dt$water_cons[dt$water_cons>1000] = 1000
dt$RevRate[dt$RevRate<0] = 0
dt$RevRate[dt$RevRate>100] = 100


zz = dt$OOP_payments+dt$crisis_voucher_amount+dt$discount_assistance+dt$bill_leaf+dt$bill_donate+dt$unpaid_debt

#---------+---------+---------+---------+---------+---------+
# Decomposition of total revenue
#---------+---------+---------+---------+---------+---------+
dt1 = dt[,list(Rev=sum(Rev,na.rm=T),
               total_payments=sum(total_payments,na.rm=T),
               OOP_payments=sum(OOP_payments,na.rm=T),
               leftover_debt=sum(leftover_debt,na.rm=T),
               bill_penalty=sum(bill_penalty,na.rm=T),
               bill_donate=sum(bill_donate,na.rm=T),
               bill_bankrupt=sum(bill_bankrupt,na.rm=T),
               unpaid_debt=sum(unpaid_debt,na.rm=T),
               crisis_voucher_amount=sum(crisis_voucher_amount,na.rm=T),
               discount_assistance=sum(discount_assistance,na.rm=T),
               discounts_and_vouchers=sum(discounts_and_vouchers,na.rm=T),
               bill_leaf=sum(bill_leaf,na.rm=T)), by = date]
setkeyv(dt1,c("date"))

dt2 = dt1[1:16,]
scale = 1000000
dt2[,Rev.scaled:=Rev/scale]
dt2[,OOP.scaled:=OOP_payments/scale]
dt2[,discounts.scaled:=discounts_and_vouchers/scale]
dt2[,unpaid.scaled:=unpaid_debt/scale]
dt2[,accum.OOP_payments:=OOP_payments/scale]
dt2[,accum.discounts_and_vouchers:=accum.OOP_payments+discounts_and_vouchers/scale]
dt2[,accum.unpaid_debt:=accum.discounts_and_vouchers+unpaid_debt/scale]

ggplot(data=dt2,aes(x=date,y=Rev.scaled,group=1)) +
  #xlim("2018/10","2022/04") +
  geom_path(color = "black", size = 1) + 
  geom_path(aes(x=date,y=accum.OOP_payments), col="blue",size=1) +
  geom_path(aes(x=date,y=accum.discounts_and_vouchers), col="brown",size=1) +
  geom_path(aes(x=date,y=accum.unpaid_debt), col="green",size=1) +  
  geom_ribbon(aes(ymin=0,ymax=accum.OOP_payments), fill="blue", alpha=0.5) +
  geom_ribbon(aes(ymin=accum.OOP_payments,ymax=accum.discounts_and_vouchers), fill="brown", alpha=0.5) +
  geom_ribbon(aes(ymin=accum.discounts_and_vouchers,ymax=accum.unpaid_debt), fill="green", alpha=0.5) +
  #geom_vline(data=dt2[1,],aes(xintercept = date),color="black") +
  geom_text(data=dt2[1,],aes(x = date, y = OOP.scaled/2,label=paste(round(OOP_payments/Rev*100,2),"%",sep="")),colour="black",size=3) +
  geom_segment(data=dt2[1,],aes(x = date, y = 0, xend = date, yend = OOP.scaled),
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  geom_text(data=dt2[.N,],aes(x = date, y = OOP.scaled/2,label=paste(round(OOP_payments/Rev*100,2),"%",sep="")),colour="black",size=3) +
  geom_text(data=dt2[.N,],aes(x = date, y = OOP.scaled+discounts.scaled/2,label=paste(round(discounts_and_vouchers/Rev*100,2),"%",sep="")),colour="black",size=3) +
  geom_text(data=dt2[.N,],aes(x = date, y = OOP.scaled+discounts.scaled+unpaid.scaled/2,label=paste(round(unpaid_debt/Rev*100,2),"%",sep="")),colour="black",size=3) +
  geom_segment(data=dt2[.N,],aes(x = date, y = 0, xend = date, yend = OOP.scaled),
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  labs(x="date (year/quarter)",y="Revnue ($ millions)",title="Breakdown of Revenue") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),text = element_text(size=12),axis.text = element_text(size=12),axis.text.x = element_text(size=12,angle=-45,hjust=0),axis.text.y = element_text(size=12))



#---------+---------+---------+---------+---------+---------+
# Water Usage vs Revenue Rate in last quarter of 2020
#---------+---------+---------+---------+---------+---------+
# Let's look at last quarter of 2020
dd = data.frame(dt[year(dt$DUE_DT)==2020 & quarter(dt$DUE_DT)==4,])
dd$water_cons[dd$water_cons>1000] = 1000
dd$RevRate[dd$RevRate<0] = 0
dd$RevRate[dd$RevRate>100] = 100

# (1) RevRate vs Water Usage
zz = hist(dd$RevRate,breaks=9)
bins = data.frame(RevRate=zz$mids,count=zz$count,weight=zz$count/sum(zz$count))

coeff = max(dd$water_cons)
ggplot(data=dd,aes(x=RevRate,y=water_cons)) +
  geom_point(color = "#0073D9", size = 1) + 
  geom_bar(data=bins,aes(x=RevRate,y=weight*coeff), width = 10,stat="identity",col="grey",alpha=.3)+
  
  # Y axis:
  scale_y_continuous(
    
    # first axis
    name = "water consumption",
    
    # second axis
    sec.axis = sec_axis( trans=~.*.001, name="fraction of consumers")
  )


# (1) Water Usage vs Rev Rate
zz = hist(dd$water_cons,breaks=c(0,10,20,30,40,50,60,70,80,100,1000))
bins1 = data.frame(water_cons=zz$mids,count=zz$count,weight=zz$count/sum(zz$count))
bins1$RevRate = 0
for(rr in 1:nrow(bins1)){
  bins1$RevRate[rr] = mean(dd$RevRate[dd$water_cons>zz$breaks[rr] & dd$water_cons<=zz$breaks[rr+1]],na.rm=TRUE)
}
ggplot(data=bins1,aes(x=water_cons,y=RevRate)) +
  geom_path(color = "#0073D9", size = 1) + 
  geom_bar(data=bins1,aes(x=water_cons,y=weight*max(dd$RevRate)), width = 10,stat="identity",col="grey",alpha=.3)+
  
  # Y axis:
  scale_y_continuous(
    
    # first axis
    name = "Bill Payment Rate (%)",
    
    # second axis
    sec.axis = sec_axis( trans=~.*.001, name="fraction of consumers")
  )

