#=========================================================================#
#
# RCT 1: Preliminary Analysis of Discounts
#
#       by JP Dube, 2-16-2025
#
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+

library(data.table)
library(ggplot2)
library(dplyr)
library(glmnet)
library(doParallel)

if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# FUNCTIONS	#
#---------+---------+---------+---------+---------+---------+

# Make propensities for graphs
mkprop = function(dt,outnm,groupnm){
  eval(parse(text=paste("tabS = t(table(dt$",outnm,",dt$",groupnm,"))",sep="")))
  tabS = tabS[,2]/rowSums(tabS)*100
  eval(parse(text=paste("NS = rowSums(t(table(dt$",outnm,",dt$",groupnm,")))",sep="")))
  se = sqrt(tabS*(100-tabS)/NS)
  eval(parse(text=paste("regout = lm(dt$",outnm,"*100~dt$",groupnm,")",sep="")))
  return(list(tabS=tabS,se=se,regout=regout))
}

# Make Shares for graphs
mkshare = function(dt,outnm,groupnm){
  eval(parse(text=paste("z=aggregate(x = dt$",outnm,", by = list(cell=dt$",groupnm,"), FUN = mean, na.rm=T)",sep="")))
  tabS = z$x
  names(tabS) = z$cell
  eval(parse(text=paste("NS = rowSums(t(table(dt$",outnm,",dt$",groupnm,")))",sep="")))
  se = sqrt(tabS*(100-tabS)/NS)
  eval(parse(text=paste("regout = lm(dt$",outnm,"~dt$",groupnm,")",sep="")))
  return(list(tabS=tabS,se=se,regout=regout))
}

# Make Means for graphs
mkmean = function(dt,outnm,groupnm){
  eval(parse(text=paste("z=aggregate(x = dt$",outnm,", by = list(cell=dt$",groupnm,"), FUN = mean, na.rm=T)",sep="")))
  tabS = z$x
  names(tabS) = z$cell
  eval(parse(text=paste("z=aggregate(x = dt$",outnm,", by = list(cell=dt$",groupnm,"), FUN = sd, na.rm=T)",sep="")))
  eval(parse(text=paste("NS = rowSums(t(table(dt$",outnm,",dt$",groupnm,")))",sep="")))
  se = z$x/sqrt(NS)
  eval(parse(text=paste("regout = lm(dt$",outnm,"~dt$",groupnm,")",sep="")))
  return(list(tabS=tabS,se=se,regout=regout))
}

# Custom error bars
mkerrbars = function(xx,yy,yyup,yydown,col){
  for(ii in 1:length(xx)){
    arrows(x0=xx[ii],x1=xx[ii],y0=yydown[ii],y1=yyup[ii],lwd=1,col=col,angle=90,length=.05)
    arrows(x0=xx[ii],x1=xx[ii],y1=yydown[ii],y0=yyup[ii],lwd=1,col=col,angle=90,length=.05)
    points(xx[ii],yy[ii],pch=16,col="black")
  }
}

# Make bar charts
mkbar = function(out,title,xlab,ylab,ylim,reg){
  Pgrid = as.integer(names(out$tabS))
  plot(0,0,xlim=range(Pgrid),ylim=ylim,col='grey',xlab=xlab,ylab = ylab,type="n",xaxt="n",main=title,panel.first={par('xpd'=FALSE);grid(NULL,NULL)})
  axis(side=1,at=Pgrid)
  rect(xleft=Pgrid-2,xright=Pgrid+2,ytop=out$tabS,ybottom=out$tabS*0,col="grey")
  rect(xleft=Pgrid[Pgrid==99]-7,xright=Pgrid[Pgrid==99]+7,ytop=out$tabS[Pgrid==99],ybottom=out$tabS[Pgrid==99]*0,col="grey",border="darkred",lwd=3)
  mkerrbars(Pgrid,out$tabS,out$tabS-1.96*out$se,out$tabS+1.96*out$se,col="black")
  if(missing(reg)==FALSE){
    bet = matrix(coef(out$regout))
    xx = cbind(matrix(1,2,1),matrix(c(0,max(as.numeric(names(out$tabS))))))
    yy = xx%*%bet
    lines(matrix(xx[,2]),yy)
  }
}


#---------+---------+---------+---------+---------+---------+
# Load Current RCT 1 data
#   estimation_dataset
#   census_variables
#---------+---------+---------+---------+---------+---------+
load("data/analysis/estimation_dataset.RData")
raw = estimation_dataset
# Creat Vars
raw$B_t[raw$B_t<0] = 0
raw$w_t[raw$w_t<0] = 0
raw$lag_w_t[raw$lag_w_t<0] = 0
raw$payment = -raw$E_t
raw$pay = raw$payment>=raw$B_t
#raw$payshare = raw$payment/raw$B_t*100
#raw$payshare[raw$B_t==0]= NaN
raw$payshare = raw$payment/raw$O_t*100
raw$payshare[raw$O_t==0]= NaN
raw$payshare[raw$payshare<0]= 0
raw$deadbeat = 1*(raw$ufh==TRUE) + 2*(raw$ufh==FALSE & raw$below_median_income==TRUE) + 3*(raw$ufh==FALSE & raw$below_median_income==FALSE)
### FIX FACT THAT SOME PEOPLE OVER-PAY
#raw$payshare[raw$payshare>100]= 100
# Winsorize Payshare above at 97% by quarter
for(tt in unique(raw$t)){
  ind = raw$t==tt & !is.na(raw$payshare)
  raw$payshare[ind][raw$payshare[ind]>quantile(raw$payshare[ind],prob=.97,na.rm=T)] = quantile(raw$payshare[ind],prob=.97,na.rm=T)
}

# Current Set of accounts past payment deadline
Ncurr=nrow(estimation_dataset[estimation_dataset$t==0 &
                                (estimation_dataset$fa_type=="Tier2" | is.na(estimation_dataset$fa_type)) &
                                is.na(estimation_dataset$exit_reason),])
print(paste("theoretically currently available accounts: ",
            as.character(prettyNum(Ncurr,big.mark = ","))," (", round(Ncurr/20000,1)*100,"%)",sep=""))

# Filter
raw <- raw %>%
  group_by(id) %>%
  filter(any(t==1, na.rm=TRUE)) %>%
  ungroup()

raw <- raw %>%
  filter(is.na(exit_reason),
         account_status!="FINAL",
         !is_rebill,
         !first_bill)

# Create estimation datasets
dt = data.table(raw)
FAdrop = (dt$fa_type!="Tier2" | is.na(dt$fa_type))
dt = dt[FAdrop==1,]
#####
dt0 = dt[dt$t==-2,]
dt1 = dt[dt$t==-1,]
dt2 = dt[dt$t==0,]

print(paste("Actual currently available accounts: ",
            as.character(prettyNum(nrow(dt2),big.mark = ","))," (", round(nrow(dt2)/20000,1)*100,"%)",sep=""))


#---------+---------+---------+---------+---------+---------+
# Randomization checks during Q2 2024
#---------+---------+---------+---------+---------+---------+
varlist = c("income_quartile","credit_score","payment_plan","total_house_holds","B_t","lag_w_t","payshare","pay","delinquent_at_randomization","deadbeat")
output0 = data.frame(matrix(0,10,2*9))
for(rr in 1:length(varlist)){
  dd = as.matrix(dt0[,varlist[rr],with=FALSE])
  output0[rr,seq(from=1,to=2*9,by=2)] = round(unlist(tapply(dd,dt0$discount_grid,mean,na.rm=T)),4)
  output0[rr,seq(from=2,to=2*9,by=2)] = paste("(",round(unlist(tapply(dd,dt0$discount_grid,sd,na.rm=T))/sqrt(unlist(tapply(dd,dt0$discount_grid,length))),4),")",sep="")
}
rownames(output0)[1:length(varlist)] = varlist



#---------+---------+---------+---------+---------+---------+
# Treatment Effect during Q4 2024
#---------+---------+---------+---------+---------+---------+
varlist = c("income_quartile","credit_score","payment_plan","total_house_holds","B_t","lag_w_t","payshare","payment","pay","delinquent_at_randomization","deadbeat")
output1 = data.frame(matrix(0,10,2*9))
for(rr in 1:length(varlist)){
  dd = as.matrix(dt1[,varlist[rr],with=FALSE])
  output1[rr,seq(from=1,to=2*9,by=2)] = round(unlist(tapply(dd,dt1$discount_grid,mean,na.rm=T)),4)
  output1[rr,seq(from=2,to=2*9,by=2)] = paste("(",round(unlist(tapply(dd,dt1$discount_grid,sd,na.rm=T))/sqrt(unlist(tapply(dd,dt1$discount_grid,length))),4),")",sep="")
}
rownames(output1)[1:length(varlist)] = varlist


#---------+---------+---------+---------+---------+---------+
# Treatment Effect during Q1 2025
#---------+---------+---------+---------+---------+---------+
varlist = c("income_quartile","credit_score","payment_plan","total_house_holds","B_t","lag_w_t","payshare","payment","pay","delinquent_at_randomization","deadbeat")
output2 = data.frame(matrix(0,10,2*9))
for(rr in 1:length(varlist)){
  dd = as.matrix(dt2[,varlist[rr],with=FALSE])
  output2[rr,seq(from=1,to=2*9,by=2)] = round(unlist(tapply(dd,dt2$discount_grid,mean,na.rm=T)),4)
  output2[rr,seq(from=2,to=2*9,by=2)] = paste("(",round(unlist(tapply(dd,dt2$discount_grid,sd,na.rm=T))/sqrt(unlist(tapply(dd[!is.na(dd)],dt2$discount_grid[!is.na(dd)],length))),4),")",sep="")
}
rownames(output2)[1:length(varlist)] = varlist



#---------+---------+---------+---------+---------+---------+
# Treatment Effect during Q1 2025
#---------+---------+---------+---------+---------+---------+
## ADD QUARTILES DURING TEST QUARTER
Iquartiles = quantile(dt2$income,prob=c(.25,.5,.75),na.rm=T)
Cquartiles = quantile(dt2$credit_score,prob=c(.25,.5,.75),na.rm=T)

dt2$IQ = 1*(dt2$income<Iquartiles[1]) + 2*(dt2$income>=Iquartiles[1] & dt2$income<Iquartiles[2]) + 3*(dt2$income>=Iquartiles[2] & dt2$income<Iquartiles[3]) + 4*(dt2$income>=Iquartiles[3])
dt2$CQ = 1*(dt2$credit_score<Cquartiles[1])  +  2*(dt2$credit_score>=Cquartiles[1] & dt2$credit_score<Cquartiles[2])  +  3*(dt2$credit_score>=Cquartiles[2] & dt2$credit_score<Cquartiles[3])  +  4*(dt2$credit_score>=Cquartiles[3])


#---------+---------+---------+---------+---------+---------+
# Analyze by income group
#---------+---------+---------+---------+---------+---------+
titles = c("IQ1","IQ2","IQ3","IQ4")

# FULL PAYMENT RATES
png(filename="output/figures/ShareFullPay_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkprop(dt2[dt2$IQ==rr,],"pay","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Share of Full Payers by Income Quartile", side = 3, line = 0, outer = TRUE)
dev.off()


# PAYMENT SHARE
png(filename="output/figures/PayShare_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt2[dt2$IQ==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Income Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

# EXPECTED REVENUE (Per Customer)
png(filename="output/figures/ERev_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt2[dt2$IQ==rr,],"payment","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
}
mtext("Expected Revenue per Customer by Income Quartile", side = 3, line = 0, outer = TRUE)
dev.off()



#---------+---------+---------+---------+---------+---------+
# Analyze by DEADBEAT
#---------+---------+---------+---------+---------+---------+
titles = c("Below UFH","Below Median Income","Above Median Income")

# FULL PAYMENT RATES
png(filename="output/figures/ShareFullPay_byDead.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkprop(dt2[dt2$deadbeat==rr,],"pay","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Share of Full Payers by Income Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

# PAYMENT SHARE
png(filename="output/figures/PayShare_byDead.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkmean(dt2[dt2$deadbeat==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,200),reg=1)
}
mtext("Payment Share by Income Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

# EXPECTED REVENUE (Per Customer)
png(filename="output/figures/ERev_byDead.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkmean(dt2[dt2$deadbeat==rr,],"payment","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
}
mtext("Expected Revenue per Customer by Income Quartile", side = 3, line = 0, outer = TRUE)
dev.off()



#---------+---------+---------+---------+---------+---------+
# Analyze by Credit Status
#---------+---------+---------+---------+---------+---------+
titles = c("CQ1","CQ2","CQ3","CQ4")

# FULL PAYMENT RATES
png(filename="output/figures/ShareFullPay_byCQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkprop(dt2[dt2$CQ==rr,],"pay","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Propensity by Credit Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

png(filename="output/figures/PayShare_byCQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt2[dt2$CQ==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Credit Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

# EXPECTED REVENUE (Per Customer)
png(filename="output/figures/ERev_byCQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt2[dt2$CQ==rr,],"payment","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
}
mtext("Expected Revenue per Customer by Credit Quartile", side = 3, line = 0, outer = TRUE)
dev.off()



#---------+---------+---------+---------+---------+---------+
# Analyze by Delinquency Status
#---------+---------+---------+---------+---------+---------+
titles = c("Not Delinquent","Delinquent")

# FULL PAYMENT RATES
png(filename="output/figures/ShareFullPay_byDelinq.png")
par(mfrow=c(1,2))
for(rr in 0:1){
  out = mkprop(dt2[dt2$delinquent_at_randomization==rr,],"pay","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Propensity by Delinquency Status", side = 3, line = 0, outer = TRUE)
dev.off()

png(filename="output/figures/PayShare_byDelinq.png")
par(mfrow=c(1,2))
for(rr in 0:1){
  out = mkmean(dt2[dt2$delinquent_at_randomization==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Deqlinquency Status", side = 3, line = 0, outer = TRUE)
dev.off()


# EXPECTED REVENUE (Per Customer)
png(filename="output/figures/ERev_byDelinq.png")
par(mfrow=c(1,2))
for(rr in 0:1){
  out = mkmean(dt2[dt2$delinquent_at_randomization==rr,],"payment","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
}
mtext("Expected Revenue per Customer by Delinquency Status", side = 3, line = 0, outer = TRUE)
dev.off()


#---------+---------+---------+---------+---------+---------+
# Analyze by Deliquency and Income Status
#---------+---------+---------+---------+---------+---------+
titles = c(paste("Not Delinquent,",c("IQ1","IQ2","IQ3","IQ4") ,sep=""),paste("Delinquent,",c("IQ1","IQ2","IQ3","IQ4"),sep=""))

# FULL PAYMENT RATES
for(ss in 0:1){
  png(filename="output/figures/ShareFullPay_byDelinqIQ.png")
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:4){
    out = mkprop(dt2[dt2$IQ==rr & dt2$delinquent_at_randomization==ss,],"pay","discount_grid")
    mkbar(out,titles[ss*4+rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
  }
  mtext("Share of Full Payers by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

# PAYMENT SHARE
png(filename="output/figures/PayShare_byDelinqIQ.png")
for(ss in 0:1){
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:4){
    out = mkshare(dt2[dt2$IQ==rr & dt2$delinquent_at_randomization==ss,],"payshare","discount_grid")
    mkbar(out,titles[ss*4+rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
  }
  mtext("Payment Share by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

# EXPECTED REVENUE (Per Customer)
for(ss in 0:1){
  png(filename="output/figures/ERev_byDelinqIQ.png")
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:4){
    out = mkmean(dt2[dt2$IQ==rr & dt2$delinquent_at_randomization==ss,],"payment","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
  }
  mtext("Expected Revenue per Customer by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}



#---------+---------+---------+---------+---------+---------+
# Analyze by Deliquency and Deadbeat
#---------+---------+---------+---------+---------+---------+
titles = c(paste("Not Delinquent, ",c("Below UFH","Below Median Income","Above Median Income") ,sep=""),
           paste("Delinquent, ",c("Below UFH","Below Median Income","Above Median Income"),sep=""))
titles1 = c("Not Delinquent","Delinquent")

# FULL PAYMENT RATES
for(ss in 0:1){
  png(filename=paste("output/figures/ShareFullPay_",titles1[ss+1],"_byDead.png",sep=""))
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:3){
    out = mkprop(dt2[dt2$IQ==rr & dt2$delinquent_at_randomization==ss,],"pay","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
  }
  mtext("Share of Full Payers by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

# PAYMENT SHARE
for(ss in 0:1){
  png(filename=paste("output/figures/PayShare_",titles1[ss+1],"_byDead.png",sep=""))
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:3){
    out = mkshare(dt2[dt2$IQ==rr & dt2$delinquent_at_randomization==ss,],"payshare","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
  }
  mtext("Payment Share by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

# EXPECTED REVENUE (Per Customer)
for(ss in 0:1){
  png(filename=paste("output/figures/ERev_",titles1[ss+1],"_byDead.png",sep=""))
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:3){
    out = mkmean(dt2[dt2$IQ==rr & dt2$delinquent_at_randomization==ss,],"payment","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
  }
  mtext("Expected Revenue per Customer by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}




### CATE
zz = factor(dt2$discount_grid,levels=sort(unique(dt2$discount_grid)),labels=paste(as.character(sort(unique(dt2$discount_grid))),"%",sep=""))
zz1 = factor(dt2$IQ,levels=sort(unique(dt2$IQ)),labels=paste("IQ",as.character(sort(unique(dt2$IQ))),sep=""))
zz2 = factor(dt2$CQ,levels=sort(unique(dt2$CQ)),labels=paste("CQ",as.character(sort(unique(dt2$IQ))),sep=""))
dt2$Dincome = dt2$income/1000 - mean(dt2$income/1000)
dt2$Dcredit_score = dt2$credit_score - mean(dt2$credit_score)
dt2$Ddelinq = dt2$delinquent_at_randomization - mean(dt2$delinquent_at_randomization)
dt2$Dunemp = dt2$unemployment_rate_in_labor_force - mean(dt2$unemployment_rate_in_labor_force,na.rm=T)
dt2$Dblack = dt2$percent_of_population_includes_black - mean(dt2$percent_of_population_includes_black,na.rm=T)
dt2$Dlagwt = dt2$lag_w_t - mean(dt2$lag_w_t,na.rm=T)
dt2$cell = zz
dt2$IQfact = zz1
dt2$CQfact = zz2

form = ~cell*IQfact+cell*CQfact+cell*delinquent_at_randomization-1
mm = (model.matrix(form,data=dt2))
form1 = ~cell*delinquent_at_randomization-1
mm1 = (model.matrix(form1,data=dt2))
form2 = ~cell*income+cell*credit_score+cell*delinquent_at_randomization-1
mm2 = (model.matrix(form2,data=dt2))
form3 = ~cell*Dincome+cell*Dcredit_score+cell*Ddelinq-1
mm3 = (model.matrix(form3,data=dt2))
form4 = ~cell*Dincome+cell*Dcredit_score+cell*Ddelinq+cell*Dunemp+cell*Dblack-1
mm4 = (model.matrix(form4,data=dt2))

summary(lm(dt2$payshare~mm-1))
summary(lm(dt2$payshare~mm1-1))
summary(lm(dt2$payshare~mm2-1))
summary(lm(dt2$payshare~mm3-1))
summary(lm(dt2$payshare[!is.na(dt2$Dblack) & !is.na(dt2$Dunemp)]~mm4-1))





###
# Set params
###
K <- ncol(mm)     # Dimension of Variables (in design matrix)
N <- nrow(mm)
penalty <- rbind( matrix(0, 9, 1), matrix(1, K - 9,1))
NB=30
N = nrow(mm)

out <- cv.glmnet(mm, dt2$payshare, penalty.factor = penalty)
#out <- cv.glmnet(mm, dt2$payshare)
coef(out, s = "lambda.min")


#---------+---------+---------+---------+---------+---------+
# Weighted Likelihood Bootstrap Lasso
#---------+---------+---------+---------+---------+---------+
###
# Define Model
###
form = ~cell*income+cell*credit_score+cell+cell*delinquent_at_randomization+cell*lag_w_t+cell*has_environment_discount-1
#form = ~cell*IQfact+cell*CQfact+cell+cell*delinquent_at_randomization+cell*lag_w_t+cell*has_environment_discount-1
#form = ~cell*IQfact+cell*CQfact+cell+cell*Ddelinq+cell*Dblack-1
#form = ~cell*Dincome+cell*Dcredit_score+cell*Ddelinq+cell*Dblack-1
mm = (model.matrix(form,data=dt2))
#y = dt2$payment[!is.na(dt2$Dblack)]
y = dt2$payment[!is.na(dt2$lag_w_t)]
y[y<0] = 0

###
# Set params
###
K <- ncol(mm)     # Dimension of Variables (in design matrix)
N <- nrow(mm)
penalty <- rbind( matrix(0, 9, 1), matrix(1, K - 9,1))
NB=30
N = nrow(mm)

###
# initialize outputs
###
glm.out = gmd.out =  gm.out=list()
set.seed(2)

###
# Bootstrap loop
###
for(bb in 1:NB)
{
  wts <- rexp(N)                                # Rubin's Dirichlet Weighting approach
  gm.out[[bb]] = cv.glmnet(mm,y,family='gaussian',penalty.factor = penalty,weights = wts) 	# Run Cross Validated Gamlr
  gm.set = summary(coef(gm.out[[bb]], s = "lambda.min"))$i  				# Active Set Index
  gm.set = gm.set[2:length(gm.set)]-1  					# Active Set
  gmd.out[[bb]] = glm(y~mm[,gm.set],family='gaussian',weights = wts) 		    # Post Selection GLM
  cat("Bootstrap ",bb," completed.\n")
}

###
# Retain Coefficients based on Minimum Lambda
###
gm.cfs = matrix(0,nrow(coef(gm.out[[1]])),NB)
for (rr in 1:NB) {
  bb = as.matrix(coef(gm.out[[rr]]))
  gm.cfs[,rr] = bb
}

###
# SAVE Coeffs (gm.cfs)
###
vars = colnames(mm)
cells = vars[1:9]
save(gm.cfs,vars,cells,file="output/gmcfs.Rdata")

###
# POSTERIOR HETEROGENEITY
###
load("output/gmcfs.Rdata")
X = cbind(matrix(1,N,1),mm)
colnames(X)[1] = "intercept"
main = -grep("cell",colnames(X))
a = X[,main]%*%gm.cfs[main,]
cellinter = FeatInters = b = treatsim = NULL
TREAT = matrix(1,nrow(mm),9)
count=1
for(ii in 1:length(cells)){
  cellinter[[ii]] = grep(paste(cells[ii],":",sep=""),colnames(X))
  cellmain = grep(paste("\\b",cells[ii],"\\b",sep=""),colnames(X))
  FeatInters[[ii]] = gsub(paste(cells[ii],":",sep=""),"",colnames(X)[grep(paste(cells[ii],":",sep=""),colnames(X))])
  total = c(cellmain,inter[[ii]])
  b[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gm.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gm.cfs[cellinter[[ii]],]
  treatsim[[ii]] = a + b[[ii]]
  TREAT[,ii] = apply(treatsim[[ii]],1,mean)
}

policy = max.col(TREAT)
table(policy)

