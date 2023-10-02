#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
library(zoo)
library(data.table)
library(ggplot2)
#library(gplot)
library(lfe)
library(plyr)
library(dplyr)

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

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # Summary. For each group's data frame, return vector with N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=TRUE),
                     mean = mean   (xx[[col]], na.rm=TRUE),
                     sd   = sd     (xx[[col]], na.rm=TRUE)
                   )
                 },
                 measurevar
  )
  # Rename "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
load(paste0(working_data_dir, "/portland_panel.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))


#---------+---------+---------+---------+---------+---------+
# Create Data for Analysis
#---------+---------+---------+---------+---------+---------+
dt = data.table(portland_panel)
setkeyv(dt,c("ACCOUNT_NO","BILL_RUN_DT"))

# Date variables
dt[,year:=year(dt$BILL_RUN_DT)]
dt[,quarter:=quarter(dt$BILL_RUN_DT)]
dt$time = (dt$year-min(dt$year))*4 + dt$quarter
# fix missing
dt[,discount_assistance:=na.replace(discount_assistance)]
dt[,crisis_voucher_amount:=na.replace(crisis_voucher_amount)]
dt[,writeoff_amount:=na.replace(writeoff_amount)]
dt[,final_writeoff:=na.replace(final_writeoff)]
dt[,collection_sent_amount:=na.replace(collection_sent_amount)]
dt[,collection_collected_amount:=na.replace(collection_collected_amount)]
# payment and revenue variables
dt[,total_payments:=-total_payments]
dt[,crisis_voucher_amount:=-crisis_voucher_amount]
dt[,discount_assistance:=-discount_assistance]
dt[,OOP_payments:=total_payments-crisis_voucher_amount]
dt[,Rev:=total_payments+leftover_debt+final_writeoff+writeoff_amount]
dt[,RevRate:=total_payments/Rev*100]
dt[,discounts_and_vouchers:=discount_assistance+crisis_voucher_amount]
dt[,unpaid_debt:=leftover_debt-discounts_and_vouchers+
     final_writeoff+writeoff_amount-collection_collected_amount]
dt[,date:=paste(format(BILL_RUN_DT, "%Y"),  # Convert dates to quarterly
                sprintf("%02i", (as.POSIXlt(BILL_RUN_DT)$mon) %/% 3L + 1L), 
                sep = "/")]

# censor unreliable outliers
dt$water_cons[dt$water_cons>1000] = 1000
dt$RevRate[dt$RevRate<0] = 0
dt$RevRate[dt$RevRate>100] = 100

# merge in census tract demographics
dt <- merge(x=dt,y=portland_demographics_tract_wide, by="tract",all.x=TRUE)


# Collapse Data to Quarterly Frequency
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

# Decompose components of Quarterly Billed Revenue
dt2 = dt1[1:18,]
dt2[,period:=1:18]
scale = 1000000
dt2[,Rev.scaled:=Rev/scale]
dt2[,OOP.scaled:=OOP_payments/scale]
dt2[,discounts.scaled:=discounts_and_vouchers/scale]
dt2[,unpaid.scaled:=unpaid_debt/scale]
dt2[,accum.OOP_payments:=OOP_payments/scale]
dt2[,accum.discounts_and_vouchers:=accum.OOP_payments+discounts_and_vouchers/scale]
dt2[,accum.unpaid_debt:=accum.discounts_and_vouchers+unpaid_debt/scale]


# Collapse Data to Tract-Quarterly Frequency
dt3 = dt[,list(Rev=sum(Rev,na.rm=T),
               total_payments=sum(total_payments,na.rm=T),
               OOP_payments=sum(OOP_payments,na.rm=T),
               leftover_debt=sum(leftover_debt,na.rm=T),
               bill_penalty=sum(bill_penalty,na.rm=T),
               bill_donate=sum(bill_donate,na.rm=T),
               bill_bankrupt=sum(bill_bankrupt,na.rm=T),
               unpaid_debt=sum(unpaid_debt,na.rm=T),
               unpaid_debt_bar=mean(unpaid_debt,na.rm=T),
               crisis_voucher_amount=sum(crisis_voucher_amount,na.rm=T),
               discount_assistance=sum(discount_assistance,na.rm=T),
               discounts_and_vouchers=sum(discounts_and_vouchers,na.rm=T),
               bill_leaf=sum(bill_leaf,na.rm=T),
               DEM_hh_income = mean(hh_income,na.rm=T),
               DEM_black = mean(black,na.rm=T),
               DEM_hispanic = mean(hispanic,na.rm=T),
               DEM_life_expectancy = mean(life_expectancy,na.rm=T),
               DEM_hh_poverty= mean(hh_poverty,na.rm=T),
               DEM_food_stamp = mean(food_stamp,na.rm=T),
               DEM_unemployment = mean(unemployment,na.rm=T),
               time = mean(time)), by = c("date","tract")]
setkeyv(dt3,c("tract","date"))

# Create Black and Hispanic quantile groups
dt3$BlackQuantileGroup = (dt3 %>% mutate(BlackQuantileGroup = ntile(DEM_black, 4)))$BlackQuantileGroup
dt3$HispanicQuantileGroup = (dt3 %>% mutate(HispanicQuantileGroup = ntile(DEM_hispanic, 4)))$HispanicQuantileGroup
dt3$PovQuantileGroup = (dt3 %>% mutate(PovQuantileGroup = ntile(DEM_hh_poverty, 4)))$PovQuantileGroup
dt3$IncomeQuantileGroup = (dt3 %>% mutate(IncomeQuantileGroup = ntile(DEM_hh_income, 4)))$IncomeQuantileGroup

# merge quantile groups back with raw data
dt = merge(x=dt3[,c("tract","date","BlackQuantileGroup","HispanicQuantileGroup","PovQuantileGroup","IncomeQuantileGroup")],y=dt, by=c("tract","date"))



# Demographic profile of tracts in Q1 2023
Demog = dt[,list(DEM_black = mean(black,na.rm=T),
                 DEM_hh_income = mean(hh_income,na.rm=T),
                 DEM_hispanic = mean(hispanic,na.rm=T),
                 DEM_life_expectancy = mean(life_expectancy,na.rm=T),
                 DEM_hh_poverty= mean(hh_poverty,na.rm=T),
                 DEM_food_stamp = mean(food_stamp,na.rm=T),
                 DEM_unemployment = mean(unemployment,na.rm=T),
                 DEM_time = mean(time)), by = tract]



# Manually Assemble Prices
price = cbind(sort(unique(dt$date)),as.matrix(c(rep(4.89,2),rep(5.251,4),rep(5.593,4),rep(6.029,4),rep(6.493,4),rep(7.006, 4))))


#---------+---------+---------+---------+---------+---------+
# Descriptive Figures
#---------+---------+---------+---------+---------+---------+
#plotmeans(RevRate ~ date, main="Revenue Rate over Time", data=dt,ylab = "Revenue Rate (%)",n.label=F)
#abline(v=5.8,col="red",lty=2)
#text(x=5.8,y=92.5,"COVID",col="red")

#plotmeans((RevRate<100)*100 ~ date, main="Delinquency Rate over Time", data=dt,ylab = "Delinquency Rate (%)",n.label=F)
#abline(v=5.8,col="red",lty=2)
#text(x=5.8,y=92.5,"COVID",col="red")

#par(mfrow=c(2,1))
#plotmeans(RevRate ~ tract, main="Heterogeineity in Revenue Rate across tracts (Q1 2019)", data=dt[dt$date=="2019/01"],ylab = "Revenue Rate (%)",n.label=F)
#plotmeans(RevRate ~ tract, main="Heterogeineity in Revenue Rate across tracts (Q1 2023)", data=dt[dt$date=="2023/01"],ylab = "Revenue Rate (%)",n.label=F)

#plotmeans(water_cons ~ date, main="Water Usage over Time", data=dt,ylab = "Water Use (CCF)",n.label=F)
#abline(v=5.8,col="red",lty=2)
#text(x=5.8,y=92.5,"COVID",col="red")


# for(tt in c(2019,2023)){
#   datetemp = paste(tt,"/01",sep="")
#   dtsum = summarySE(dt[dt$date==datetemp],measurevar="RevRate",group = "tract")
#   ggplot(data=dtsum, aes(x=tract, y=RevRate)) + scale_y_continuous(expand = c(0, 1)) +
#     geom_errorbar(aes(ymin=RevRate-se, ymax=RevRate+se), colour="black", width=.1) +
#     geom_point(size=3, shape=21, fill="white") + 
#     labs(y="Revenue Rate (%)",title="Heterogeineity in Revenue Rate across tracts (,",datetemp,")",sep="") +
#     theme_bw() + 
#     theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
# }

#par(mfrow=c(2,1))
#plotmeans(water_cons ~ tract, main="Heterogeineity in Water Use across tracts", data=dt[dt$date=="2019/01"],ylab = "Water Use (CCF)",n.label=F)
#plotmeans(water_cons ~ tract, main="Heterogeineity in Water Use across tracts", data=dt[dt$date=="2023/01"],ylab = "Water Use (CCF)",n.label=F)



#---------+---------+---------+---------+---------+---------+
# FIGURE: Decomposition of total revenue from Q1 2019 to Q2 2023
#---------+---------+---------+---------+---------+---------+
trendline = lm(dt2$unpaid_debt~as.matrix(1:nrow(dt2)))
growth = round((dt2$unpaid_debt[18]-dt2$unpaid_debt[1])/dt2$unpaid_debt[1]*100)
dt2[,growth:=growth]
dt2[,debthat:=trendline$coefficients[1]+trendline$coefficients[2]*as.matrix(1:nrow(dt2))]
g=ggplot(data=dt2,aes(x=date,y=unpaid_debt,group=1,fill=date)) +
  geom_col(data=dt2,aes(x=date,y=unpaid_debt,group=1),alpha=.3, show.legend = FALSE) +
  geom_text(data=dt2,aes(x = date, y = unpaid_debt,label=paste("$",prettyNum(round(dt2$unpaid_debt,2),big.mark = ","),sep="")),colour="black",size=2) +
  #  geom_abline(slope=trendline$coefficients[2],intercept=trendline$coefficients[1],col="darkred",size=2) +
  geom_line(data=dt2,aes(x = date, y = debthat),arrow=arrow(length=unit(0.60,"cm")),col="darkred",size=2) +
  geom_text(data=dt2[9,],aes(x = date, y = debthat*.85),label=paste(growth,"%\ngrowth",sep=""),col="darkred",size=8,angle=50,lineheight=.65) +
  scale_y_continuous(labels = scales::dollar_format()) +
  # COVID Timing
  geom_vline(xintercept=6,col="darkred",linetype="dashed") +
  geom_text(aes(x = 6,y=-1,label="COVID"),col="darkred",size=5) +
  labs(x="date (year/quarter)",y="Unpaid Debt",title="Debt Growth") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),text = element_text(size=12),axis.text = element_text(size=12),axis.text.x = element_text(size=12,angle=-45,hjust=0),axis.text.y = element_text(size=12))
print(g)
jpeg("output/figures/debtgrowth.jpeg",res=300,width=6,height=6,units="in")
g
dev.off()


#---------+---------+---------+---------+---------+---------+
# FIGURE: Decomposition of total revenue from Q1 2019 to Q2 2023
#---------+---------+---------+---------+---------+---------+

g1=ggplot(data=dt2,aes(x=date,y=Rev.scaled,group=1)) +
  geom_path(color = "black", size = 1) + 
  geom_path(aes(x=date,y=accum.OOP_payments), col="blue",size=1) +
  geom_path(aes(x=date,y=accum.discounts_and_vouchers), col="brown",size=1) +
  geom_path(aes(x=date,y=accum.unpaid_debt), col="green",size=1) +  
  geom_ribbon(aes(ymin=0,ymax=accum.OOP_payments), fill="blue", alpha=0.5) +
  geom_ribbon(aes(ymin=accum.OOP_payments,ymax=accum.discounts_and_vouchers), fill="brown", alpha=0.5) +
  geom_ribbon(aes(ymin=accum.discounts_and_vouchers,ymax=accum.unpaid_debt), fill="green", alpha=0.5) +
  # breakdown in Q1 2019
  geom_text(data=dt2[1,],aes(x = date, y = OOP.scaled/2,label=paste("payments:\n",round(OOP_payments/Rev*100,2),"%",sep="")),colour="black",size=3,lineheight=.7,hjust=0) +
  # breakdown in Q2 2023
  geom_text(data=dt2[.N,],aes(x = date, y = OOP.scaled/2,label=paste("payments:\n",round(OOP_payments/Rev*100,2),"%",sep="")),colour="black",size=3,lineheight=.7,hjust=1) +
  geom_text(data=dt2[.N,],aes(x = date, y = OOP.scaled+discounts.scaled/2,label=paste("discounts:\n",round(discounts_and_vouchers/Rev*100,2),"%",sep="")),colour="black",size=3,lineheight=.7,hjust=1) +
  geom_text(data=dt2[.N,],aes(x = date, y = OOP.scaled+discounts.scaled+unpaid.scaled/2,label=paste("debt:\n",round(unpaid_debt/Rev*100,2),"%",sep="")),colour="black",size=3,lineheight=.7,hjust=1) +
  # COVID Timing
  geom_vline(xintercept=6,col="darkred",linetype="dashed") +
  geom_text(aes(x = 6,y=-1,label="COVID"),col="darkred",size=5) +
  labs(x="date (year/quarter)",y="Revnue ($ millions)",title="Breakdown of Revenue") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),text = element_text(size=12),axis.text = element_text(size=12),axis.text.x = element_text(size=12,angle=-45,hjust=0),axis.text.y = element_text(size=12))
print(g1)
jpeg("output/figures/RevRatedecomp.jpeg",res=300,width=6,height=6,units="in")
g1
dev.off()




#---------+---------+---------+---------+---------+---------+
# FIGURE: Water Usage vs Revenue Rate in first quarter of 2019 and 2022 
#---------+---------+---------+---------+---------+---------+
for(tt in c(2020,2023)){
  dd = data.frame(dt[year(dt$BILL_RUN_DT)==tt & quarter(dt$BILL_RUN_DT)==2 & is.na(dt$water_cons)==0,])
  # Winsorize data above at 99th percentile
  q99 = quantile(dd$water_cons,prob=.99)
  dd$water_cons[dd$water_cons>=q99] = q99
  # drop cases with missing revenue rate (i.e., new accounts)
  dd=dd[-which(is.na(dd$RevRate)),]

  zz = hist(dd$RevRate,breaks=9)
  bins = data.frame(RevRate=zz$mids,count=zz$count,weight=zz$count/sum(zz$count))
  
  coeff = max(dd$water_cons)
  ggplot(data=dd,aes(x=RevRate,y=water_cons)) +
    geom_point(color = "#0073D9", size = 1) + 
    geom_bar(data=bins,aes(x=RevRate,y=weight*coeff), width = 10,stat="identity",col="green",alpha=.3,fill="green")+
    stat_smooth(method = "lm",formula = y ~ x,geom = "smooth",col="black",se=TRUE) +
    labs(x="Revenue Rate (%)",title=paste("Relationship Between Revenue Rate and Water Usage in",tt)) +
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),text = element_text(size=12),axis.text = element_text(size=12),axis.text.x = element_text(size=12,angle=-45,hjust=0),axis.text.y = element_text(size=12)) +
    theme_dark() +
    # Y axis:
    scale_y_continuous(
      # first axis
      name = "water consumption (ccf)",
      # second axis
      sec.axis = sec_axis( trans=~.*100/coeff, name="Share of consumers (%)")
    )
  
}


#---------+---------+---------+---------+---------+---------+
# Predict Payment Propensity and Water Usage with Census
#---------+---------+---------+---------+---------+---------+
Zcols = names(dt)[57:69]
Y = dt$RevRate[dt$date=="2023/01"]
Y1 = dt$RevRate[dt$date=="2023/01"]==100
X = as.matrix(dt[dt$date=="2023/01",Zcols,with=FALSE])
ind =is.na(Y1) | apply(is.na(X),1,sum)
Y = Y[ind==0]
Y1 = Y1[ind==0]
X = X[ind==0,]
X1 = as.numeric(dt$tract)
colnames(X) = Zcols

ind = dt$date=="2019/01"
summary(lm(Y[ind]~X1[ind]))
summary(lm(Y[ind]~X[ind,]))

ind1 = dt$date=="2023/01"
summary(lm(Y[ind1]~X[ind1,]))
summary(lm(Y[ind1]~X1[ind1,]))


out=cv.gamlr(X,Y1,family="binomial",nfold=5)
BIC = (out$gaml$deviance + out$gamlr$df*log(out$gamlr$nobs))[out$seg.min] 
gm.set = which(coef(out)!=0)
rownames(coef(out))[gm.set]
gam.out = glm(Y1~X,gm.set,family='binomial')


out1=cv.gamlr(X,Y,family="gaussian",nfold=5)
BIC1 = (out$gaml$deviance + out$gamlr$df*log(out$gamlr$nobs))[out$seg.min] 
gm.set1 = which(coef(out1)!=0)
rownames(coef(out1))[gm.set1]


