#---------+---------+---------+---------+---------+---------+
# Analyze Racial Profile of Unpaid Debt
#---------+---------+---------+---------+---------+---------+
library(gridExtra)
library(ggridges)


#---------+---------+---------+---------+---------+---------+
# FUNCTIONS
#---------+---------+---------+---------+---------+---------+
segmentplot = function(dattemp,growthtemp,titlab){
  N = nrow(dattemp)
  g =  ggplot(data=dattemp,aes(x=date,y=unpaid_debt_bar,group=1,fill=date)) + ylim(0,57)+
    geom_col(data=dattemp,aes(x=date,y=unpaid_debt_bar,group=1),alpha=.3, show.legend = FALSE) +
    geom_text(data=dattemp,aes(x = date, y = unpaid_debt_bar,label=paste("$",prettyNum(round(dattemp$unpaid_debt_bar,2),big.mark = ","),sep="")),colour="black",size=1.5) +
    #    geom_line(data=dattemp,aes(x = date, y = debthat),arrow=arrow(length=unit(0.60,"cm")),col="darkred") +
    #    geom_text(data=dattemp[9,],aes(x = date, y = debthat*.85),label=paste(growthtemp,"%\ngrowth",sep=""),col="darkred",angle=30,lineheight=.65) +
    # COVID Timing
    geom_vline(xintercept=6,col="darkred",linetype="dashed") +
    geom_text(aes(x = 6,y=1,label="COVID"),col="darkred",size=3) +
    labs(x="date (year/quarter)",y="Unpaid Debt",title=paste("Debt Growth:",titlab)) +
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),text = element_text(size=4),axis.text = element_text(size=4),axis.text.x = element_text(size=4,angle=-45,hjust=0),axis.text.y = element_text(size=4))
}

mkdataseg = function(dtin,segment){
  dtout = dtin[,list(Rev=sum(Rev,na.rm=T),
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
                     DEM_hh_income= mean(hh_income,na.rm=T),
                     DEM_food_stamp = mean(food_stamp,na.rm=T),
                     DEM_unemployment = mean(unemployment,na.rm=T),
                     time = mean(time)), by = c("date",segment)]
}



#---------+---------+---------+---------+---------+---------+
# Collapse to tracts and time by quantiles of "% Black"
#---------+---------+---------+---------+---------+---------+
dt4 = mkdataseg(dt,"BlackQuantileGroup")
setkeyv(dt4,c("BlackQuantileGroup","date"))
keeper = !is.na(dt4$unpaid_debt) & !is.na(dt4$DEM_black) & dt4$time<=18 & !is.na(dt4$DEM_life_expectancy)
dt41 = dt4[keeper,]

# Compute Time Trends by Quantile
trendlines  = matrix(0,4,3)
growth = matrix(0,4,2)
for(bb in 1:4){
  ind = dt41$BlackQuantileGroup==bb
  trendlines[bb,] = as.matrix(c(bb,lm(dt41$unpaid_debt_bar[ind]~dt41$time[ind])$coefficients))
  ind1 = dt41$BlackQuantileGroup==bb & dt41$time==18
  ind2 = dt41$BlackQuantileGroup==bb & dt41$time==1
  growth[bb,] = cbind(bb,round((dt41$unpaid_debt_bar[ind1]-dt41$unpaid_debt_bar[ind2])/mean(dt41$unpaid_debt_bar[ind2],na.rm=T)*100))
}
trendlines = cbind(trendlines,cumsum(trendlines[,2]),cumsum(trendlines[,3]))
colnames(trendlines) = c("BlackQuantileGroup","intercept","trend","cumulative intercept","cumulative trend")
colnames(growth) = c("BlackQuantileGroup","growth")
dt41 = merge(x=growth,y=dt41, by="BlackQuantileGroup")
dt41 = merge(x=trendlines,y=dt41, by="BlackQuantileGroup")
dt41$debthat=dt41$`intercept`+dt41$`trend`*dt41$time
dt41$cumdebthat=dt41$`cumulative intercept`+dt41$`cumulative trend`*dt41$time


lab = c("1st quartile of % Black","2nd quartile of % Black","3rd quartile of % Black","4th quartile of % Black")
myplots_black = list()
for(bb in 1:4){
  dattemp = data.table(dt41[dt41$BlackQuantileGroup==bb,])
  growthtemp = growth[bb,2]
  myplots_black[[bb]] <- segmentplot(dattemp,growthtemp,titlab=lab[bb])
}
print(grid.arrange(myplots_black[[1]],myplots_black[[2]],myplots_black[[3]],myplots_black[[4]],nrow=2,ncol=2))
jpeg("output/figures/debtgrowth_black.jpeg",res=300,width=6,height=6,units="in")
grid.arrange(myplots_black[[1]],myplots_black[[2]],myplots_black[[3]],myplots_black[[4]],nrow=2,ncol=2)
dev.off()

print("debt growth")
print(paste("4th quartile % Black: $",round(dt41$unpaid_debt_bar[72]-dt41$unpaid_debt_bar[55],2)))
print(paste("1st quartile % Black: $",round(dt41$unpaid_debt_bar[18]-dt41$unpaid_debt_bar[1],2)))



#---------+---------+---------+---------+---------+---------+
# Collapse to tracts and time by quantiles of "% Hispanic"
#---------+---------+---------+---------+---------+---------+
dt5 = mkdataseg(dt,"HispanicQuantileGroup")
setkeyv(dt5,c("HispanicQuantileGroup","date"))
keeper = !is.na(dt5$unpaid_debt) & !is.na(dt5$DEM_hispanic) & dt5$time<=18 & !is.na(dt5$DEM_life_expectancy)
dt51 = dt5[keeper,]

# Compute Time Trends by Quantile
trendlines  = matrix(0,4,3)
growth = matrix(0,4,2)
for(bb in 1:4){
  ind = dt51$HispanicQuantileGroup==bb
  trendlines[bb,] = as.matrix(c(bb,lm(dt51$unpaid_debt_bar[ind]~dt51$time[ind])$coefficients))
  ind1 = dt51$HispanicQuantileGroup==bb & dt51$time==18
  ind2 = dt51$HispanicQuantileGroup==bb & dt51$time==1
  growth[bb,] = cbind(bb,round((dt51$unpaid_debt_bar[ind1]-dt51$unpaid_debt_bar[ind2])/mean(dt51$unpaid_debt_bar[ind2],na.rm=T)*100))
}
trendlines = cbind(trendlines,cumsum(trendlines[,2]),cumsum(trendlines[,3]))
colnames(trendlines) = c("HispanicQuantileGroup","intercept","trend","cumulative intercept","cumulative trend")
colnames(growth) = c("HispanicQuantileGroup","growth")
dt51 = merge(x=growth,y=dt51, by="HispanicQuantileGroup")
dt51 = merge(x=trendlines,y=dt51, by="HispanicQuantileGroup")
dt51$debthat=dt51$`intercept`+dt51$`trend`*dt51$time
dt51$cumdebthat=dt51$`cumulative intercept`+dt51$`cumulative trend`*dt51$time


lab = c("1st quartile of % Hispanic","2nd quartile of % Hispanic","3rd quartile of % Hispanic","4th quartile of % Hispanic")
myplots_hispanic = list()
for(bb in 1:4){
  dattemp = data.table(dt51[dt51$HispanicQuantileGroup==bb,])
  growthtemp = growth[bb,2]
  myplots_hispanic[[bb]] <- segmentplot(dattemp,growthtemp,titlab=lab[bb])
}
print(grid.arrange(myplots_hispanic[[1]],myplots_hispanic[[2]],myplots_hispanic[[3]],myplots_hispanic[[4]],nrow=2,ncol=2))
jpeg("output/figures/debtgrowth_hispanic.jpeg",res=300,width=6,height=6,units="in")
grid.arrange(myplots_hispanic[[1]],myplots_hispanic[[2]],myplots_hispanic[[3]],myplots_hispanic[[4]],nrow=2,ncol=2)
dev.off()

print("debt growth")
print(paste("4th quartile % Hispanic: $",round(dt51$unpaid_debt_bar[72]-dt51$unpaid_debt_bar[55],2)))
print(paste("1st quartile % Hispanic: $",round(dt51$unpaid_debt_bar[18]-dt51$unpaid_debt_bar[1],2)))



#---------+---------+---------+---------+---------+---------+
# Collapse to tracts and time by quantiles of "% Poor"
#---------+---------+---------+---------+---------+---------+
dt6 = mkdataseg(dt,"PovQuantileGroup")
setkeyv(dt6,c("PovQuantileGroup","date"))
keeper = !is.na(dt6$unpaid_debt) & !is.na(dt6$DEM_hh_poverty) & dt6$time<=18 & !is.na(dt6$DEM_life_expectancy)
dt61 = dt6[keeper,]

# Compute Time Trends by Quantile
trendlines  = matrix(0,4,3)
growth = matrix(0,4,2)
for(bb in 1:4){
  ind = dt61$PovQuantileGroup==bb
  trendlines[bb,] = as.matrix(c(bb,lm(dt61$unpaid_debt_bar[ind]~dt61$time[ind])$coefficients))
  ind1 = dt61$PovQuantileGroup==bb & dt61$time==18
  ind2 = dt61$PovQuantileGroup==bb & dt61$time==1
  growth[bb,] = cbind(bb,round((dt61$unpaid_debt_bar[ind1]-dt61$unpaid_debt_bar[ind2])/mean(dt61$unpaid_debt_bar[ind2],na.rm=T)*100))
}
trendlines = cbind(trendlines,cumsum(trendlines[,2]),cumsum(trendlines[,3]))
colnames(trendlines) = c("PovQuantileGroup","intercept","trend","cumulative intercept","cumulative trend")
colnames(growth) = c("PovQuantileGroup","growth")
dt61 = merge(x=growth,y=dt61, by="PovQuantileGroup")
dt61 = merge(x=trendlines,y=dt61, by="PovQuantileGroup")
dt61$debthat=dt61$`intercept`+dt61$`trend`*dt61$time
dt61$cumdebthat=dt61$`cumulative intercept`+dt61$`cumulative trend`*dt61$time


lab = c("1st quartile of % Poor","2nd quartile of % Poor","3rd quartile of % Poor","4th quartile of % Poor")
myplots_pov = list()
for(bb in 1:4){
  dattemp = data.table(dt61[dt61$PovQuantileGroup==bb,])
  growthtemp = growth[bb,2]
  myplots_pov[[bb]] <- segmentplot(dattemp,growthtemp,titlab=lab[bb])
}
print(grid.arrange(myplots_pov[[1]],myplots_pov[[2]],myplots_pov[[3]],myplots_pov[[4]],nrow=2,ncol=2))
jpeg("output/figures/debtgrowth_Pov.jpeg",res=300,width=6,height=6,units="in")
grid.arrange(myplots_pov[[1]],myplots_pov[[2]],myplots_pov[[3]],myplots_pov[[4]],nrow=2,ncol=2)
dev.off()

print("debt growth")
print(paste("4th quartile % Poor: $",round(dt61$unpaid_debt_bar[72]-dt61$unpaid_debt_bar[55],2)))
print(paste("1st quartile % Poor: $",round(dt61$unpaid_debt_bar[18]-dt61$unpaid_debt_bar[1],2)))


#---------+---------+---------+---------+---------+---------+
# Collapse to tracts and time by quantiles of "% Income"
#---------+---------+---------+---------+---------+---------+
dt7 = mkdataseg(dt,"IncomeQuantileGroup")
setkeyv(dt7,c("IncomeQuantileGroup","date"))
keeper = !is.na(dt7$unpaid_debt) & !is.na(dt7$DEM_hh_income) & dt7$time<=18 & !is.na(dt7$DEM_life_expectancy)
dt71 = dt7[keeper,]

# Compute Time Trends by Quantile
trendlines  = matrix(0,4,3)
growth = matrix(0,4,2)
for(bb in 1:4){
  ind = dt71$IncomeQuantileGroup==bb
  trendlines[bb,] = as.matrix(c(bb,lm(dt71$unpaid_debt_bar[ind]~dt71$time[ind])$coefficients))
  ind1 = dt71$IncomeQuantileGroup==bb & dt71$time==18
  ind2 = dt71$IncomeQuantileGroup==bb & dt71$time==1
  growth[bb,] = cbind(bb,round((dt71$unpaid_debt_bar[ind1]-dt71$unpaid_debt_bar[ind2])/mean(dt71$unpaid_debt_bar[ind2],na.rm=T)*100))
}
trendlines = cbind(trendlines,cumsum(trendlines[,2]),cumsum(trendlines[,3]))
colnames(trendlines) = c("IncomeQuantileGroup","intercept","trend","cumulative intercept","cumulative trend")
colnames(growth) = c("IncomeQuantileGroup","growth")
dt71 = merge(x=growth,y=dt71, by="IncomeQuantileGroup")
dt71 = merge(x=trendlines,y=dt71, by="IncomeQuantileGroup")
dt71$debthat=dt71$`intercept`+dt71$`trend`*dt71$time
dt71$cumdebthat=dt71$`cumulative intercept`+dt71$`cumulative trend`*dt71$time


lab = c("1st quartile of % Income","2nd quartile of % Income","3rd quartile of % Income","4th quartile of % Income")
myplots_income = list()
for(bb in 1:4){
  dattemp = data.table(dt71[dt71$IncomeQuantileGroup==bb,])
  growthtemp = growth[bb,2]
  myplots_income[[bb]] <- segmentplot(dattemp,growthtemp,titlab=lab[bb])
}
print(grid.arrange(myplots_income[[1]],myplots_income[[2]],myplots_income[[3]],myplots_income[[4]],nrow=2,ncol=2))
jpeg("output/figures/debtgrowth_Income.jpeg",res=300,width=6,height=6,units="in")
grid.arrange(myplots_income[[1]],myplots_income[[2]],myplots_income[[3]],myplots_income[[4]],nrow=2,ncol=2)
dev.off()

print("debt growth")
print(paste("4th quartile Avg Income: $",round(dt71$unpaid_debt_bar[72]-dt71$unpaid_debt_bar[55],2)))
print(paste("1st quartile Avg Income: $",round(dt71$unpaid_debt_bar[18]-dt71$unpaid_debt_bar[1],2)))



#---------+---------+---------+---------+---------+---------+
# Retain useable tract-quarter observations
#---------+---------+---------+---------+---------+---------+
keeper = !is.na(dt3$unpaid_debt) & !is.na(dt3$tract) & !is.na(dt3$DEM_black) & dt3$time<=18 & !is.na(dt3$DEM_life_expectancy)
dt31 = dt3[keeper,]
tractlist = sort(unique(dt31$tract))
tracts = length(tractlist)


#---------+---------+---------+---------+---------+---------+
# Regression Evidence
#---------+---------+---------+---------+---------+---------+
FEtime = matrix(0,nrow(dt31),18)
for(tt in 1:18){
  FEtime[,tt] = dt31$time==tt
}
FEtract = matrix(0,nrow(dt31),tracts)
for(tt in 1:tracts){
  FEtract[,tt] = dt31$tract==tractlist[tt]
}

X1 = FEtime[,1:17]
colnames(X1) = paste("time",c(1:17))
summary(lm(dt31$unpaid_debt_bar~X1))

X2 =cbind(FEtract[,1:(tracts-1)])
colnames(X2) = paste("tract",c(1:(tracts-1)))
summary(lm(dt31$unpaid_debt_bar~X2))

X21 =cbind(FEtime[,1:17],FEtract[,1:(tracts-1)])
colnames(X21) = c(paste("time",c(1:17)),paste("tract",c(1:(tracts-1))))
summary(lm(dt31$unpaid_debt_bar~X21))

X3 =as.matrix(cbind(FEtime[,1:17],dt31[,names(dt31)[grep("DEM",names(dt31))],with = FALSE]))
colnames(X3) = c(paste("time",c(1:17)),c(names(dt31)[grep("DEM",names(dt31))]))
summary(lm(dt31$unpaid_debt_bar~X3))

X31 =as.matrix(dt31[,names(dt31)[grep("DEM",names(dt31))],with = FALSE])
colnames(X31) = c(names(dt31)[grep("DEM",names(dt31))])
mod = lm(dt31$unpaid_debt_bar~X31)
summary(mod)

copy.table(as.matrix(mod$coefficients))
copy.table(sqrt(diag(vcov(mod))))
copy.table(abs(as.matrix(mod$coefficients)/sqrt(diag(vcov(mod)))))


# Black Effect
coefficients(mod)[names(mod$coefficients)=="X31DEM_black"] * (dt41$DEM_black[72]-dt41$DEM_black[18])
# Hispanic Effect
coefficients(mod)[names(mod$coefficients)=="X31DEM_hispanic"] * (dt51$DEM_hispanic[72]-dt51$DEM_hispanic[18])

