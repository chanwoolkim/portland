#=========================================================================#
#
# RCT 1: Preliminary Analysis of Information
#
#       by JP Dube, 2-19-2025
#
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+

library(data.table)
library(ggplot2)
library(dplyr)

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

Ncurr=nrow(estimation_dataset[estimation_dataset$t==-1 &
                                (estimation_dataset$fa_type=="Tier2" | is.na(estimation_dataset$fa_type)) &
                                estimation_dataset$account_status!="FINAL",])
print(paste("theoretically currently available accounts: ",
            as.character(prettyNum(Ncurr,big.mark = ","))," (", round(Ncurr/20000,1)*100,"%)",sep=""))

estimation_dataset <- estimation_dataset %>%
  filter(due_date<"2025-02-27",
         account_status!="FINAL",
         !is_rebill,
         !switch_to_severe_fa,
         (!zero_discount | t!=0))

dt = data.table(estimation_dataset)
FAdrop = (dt$fa_type!="Tier2" | is.na(dt$fa_type))
dt = dt[FAdrop==1,]
dt$payment = -dt$E_t
dt$pay = dt$payment>0
dt$payshare = dt$payment/dt$B_t*100
dt$payshare[dt$B_t==0]= NaN
dt$deadbeat = 1*(dt$ufh==TRUE) + 2*(dt$ufh==FALSE & dt$below_median_income==TRUE) + 3*(dt$ufh==FALSE & dt$below_median_income==FALSE)
### FIX FACT THAT SOME PEOPLE OVER-PAY
dt$payshare[dt$payshare>100]= 100
#####
dt1 = dt[dt$t==-1,]
dt2 = dt[dt$t==0,]
dt2$lag_w_t[dt2$lag_w_t<0] = 0

## ADD QUARTILES DURING TEST QUARTER
Iquartiles = quantile(dt1$income,prob=c(.25,.5,.75),na.rm=T)
Cquartiles = quantile(dt1$credit_score,prob=c(.25,.5,.75),na.rm=T)

dt1$IQ = 1*(dt1$income<Iquartiles[1]) + 2*(dt1$income>=Iquartiles[1] & dt1$income<Iquartiles[2]) + 3*(dt1$income>=Iquartiles[2] & dt1$income<Iquartiles[3]) + 4*(dt1$income>=Iquartiles[3])
dt1$CQ = 1*(dt1$credit_score<Cquartiles[1])  +  2*(dt1$credit_score>=Cquartiles[1] & dt1$credit_score<Cquartiles[2])  +  3*(dt1$credit_score>=Cquartiles[2] & dt1$credit_score<Cquartiles[3])  +  4*(dt1$credit_score>=Cquartiles[3])

## ADD CURRENT WATER USAGE
dt1$w_t = NaN
for(hh in sort(unique(dt1$id))){
  if(sum(dt2$id==hh)>0 & sum(dt1$id==hh)>0){
    dt1$w_t[dt1$id==hh & dt1$t==-1] = mean(dt2$lag_w_t[dt2$id==hh & dt2$t==0],na.rm=T)
  }
}


#---------+---------+---------+---------+---------+---------+
# Analyze by income group
#---------+---------+---------+---------+---------+---------+
titles = c("IQ1","IQ2","IQ3","IQ4")

# FULL PAYMENT RATES
#png(filename="output/figures/ShareFullPay_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkprop(dt1[dt1$IQ==rr,],"pay","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Share of Full Payers by Income Quartile", side = 3, line = 0, outer = TRUE)
#dev.off()


# PAYMENT SHARE
#png(filename="output/figures/PayShare_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt1[dt1$IQ==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Income Quartile", side = 3, line = 0, outer = TRUE)
#dev.off()

# EXPECTED REVENUE (Per Customer)
#png(filename="output/figures/ERev_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt1[dt1$IQ==rr,],"payment","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,500))
}
mtext("Expected Revenue per Customer by Income Quartile", side = 3, line = 0, outer = TRUE)
#dev.off()


# WATER USAGE (Per Customer)
#png(filename="output/figures/ERev_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt1[dt1$IQ==rr & !is.na(dt1$w_t),],"w_t","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Water Used (cubic feet)",ylim=c(0,20))
}
mtext("Water Usage by Income Quartile", side = 3, line = 0, outer = TRUE)
#dev.off()


#---------+---------+---------+---------+---------+---------+
# Analyze by DEADBEAT
#---------+---------+---------+---------+---------+---------+
titles = c("Below UFH","Below Median Income","Above Median Income")

# FULL PAYMENT RATES
#png(filename="output/figures/ShareFullPay_byDead.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkprop(dt1[dt1$deadbeat==rr,],"pay","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Share of Full Payers by Means", side = 3, line = 0, outer = TRUE)
#dev.off()

# PAYMENT SHARE
#png(filename="output/figures/PayShare_byDead.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkmean(dt1[dt1$deadbeat==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Means", side = 3, line = 0, outer = TRUE)
#dev.off()

# EXPECTED REVENUE (Per Customer)
#png(filename="output/figures/ERev_byDead.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkmean(dt1[dt1$deadbeat==rr,],"payment","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
}
mtext("Expected Revenue per Customer by Means", side = 3, line = 0, outer = TRUE)
#dev.off()


# WATER USAGE (Per Customer)
#png(filename="output/figures/ERev_byIQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:3){
  out = mkmean(dt1[dt1$deadbeat==rr & !is.na(dt1$w_t),],"w_t","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Water Used (cubic feet)",ylim=c(0,20))
}
mtext("Water Usage by Means", side = 3, line = 0, outer = TRUE)
#dev.off()


#---------+---------+---------+---------+---------+---------+
# Analyze by Credit Status
#---------+---------+---------+---------+---------+---------+
titles = c("CQ1","CQ2","CQ3","CQ4")

# FULL PAYMENT RATES
png(filename="output/figures/ShareFullPay_byCQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkprop(dt1[dt1$CQ==rr,],"pay","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Propensity by Credit Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

png(filename="output/figures/PayShare_byCQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt1[dt1$CQ==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Credit Quartile", side = 3, line = 0, outer = TRUE)
dev.off()

# EXPECTED REVENUE (Per Customer)
png(filename="output/figures/ERev_byCQ.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 1:4){
  out = mkmean(dt1[dt1$CQ==rr,],"payment","discount_grid")
  mkbar(out,titles[rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
}
mtext("Expected Revenue per Customer by Credit Quartile", side = 3, line = 0, outer = TRUE)
dev.off()



#---------+---------+---------+---------+---------+---------+
# Analyze by Deliquency Status
#---------+---------+---------+---------+---------+---------+
titles = c("Not Deliquent","Delinquent")

# FULL PAYMENT RATES
#png(filename="output/figures/ShareFullPay_byDelinq.png")
par(mfrow=c(1,2))
for(rr in 0:1){
  out = mkprop(dt1[dt1$delinquent_at_randomization==rr,],"pay","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Propensity by Delinquency Status", side = 3, line = 0, outer = TRUE)
#dev.off()

png(filename="output/figures/PayShare_byDelinq_Q42024.png")
par(mfrow=c(1,2))
for(rr in 0:1){
  out = mkmean(dt1[dt1$delinquent_at_randomization==rr,],"payshare","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
}
mtext("Payment Share by Deqlinquency Status", side = 3, line = 0, outer = TRUE)
dev.off()

# EXPECTED REVENUE (Per Customer)
#png(filename="output/figures/ERev_byDelinq.png")
par(mfrow=c(1,2))
for(rr in 0:1){
  out = mkmean(dt1[dt1$delinquent_at_randomization==rr,],"payment","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,500))
}
mtext("Expected Revenue per Customer by Delinquency Status", side = 3, line = 0, outer = TRUE)
#dev.off()

# WATER USAGE (Per Customer)
png(filename="output/figures/water_usage_Q42024.png")
par(mfrow=c(2,2), oma=c(0,0,3,0))
for(rr in 0:1){
  out = mkmean(dt1[dt1$delinquent_at_randomization==rr & !is.na(dt1$w_t),],"w_t","discount_grid")
  mkbar(out,titles[rr+1],xlab="discount (%)",ylab= "Water Used (cubic feet)",ylim=c(0,20))
}
mtext("Water Usage by Delinquency Status", side = 3, line = 0, outer = TRUE)
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
    out = mkprop(dt1[dt1$IQ==rr & dt1$delinquent_at_randomization==ss,],"pay","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)", ylab= "Share of Full Payers (%)",ylim=c(0,100),reg=1)
  }
  mtext("Share of Full Payers by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

# PAYMENT SHARE
png(filename="output/figures/PayShare_byDelinqIQ.png")
for(ss in 0:1){
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:4){
    out = mkshare(dt1[dt1$IQ==rr & dt1$delinquent_at_randomization==ss,],"payshare","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)", ylab= "Payment Share (%)",ylim=c(0,100),reg=1)
  }
  mtext("Payment Share by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

# EXPECTED REVENUE (Per Customer)
for(ss in 0:1){
  png(filename="output/figures/ERev_byDelinqIQ.png")
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  for(rr in 1:4){
    out = mkmean(dt1[dt1$IQ==rr & dt1$delinquent_at_randomization==ss,],"payment","discount_grid")
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
    out = mkprop(dt1[dt1$IQ==rr & dt1$delinquent_at_randomization==ss,],"pay","discount_grid")
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
    out = mkshare(dt1[dt1$IQ==rr & dt1$delinquent_at_randomization==ss,],"payshare","discount_grid")
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
    out = mkmean(dt1[dt1$IQ==rr & dt1$delinquent_at_randomization==ss,],"payment","discount_grid")
    mkbar(out,titles[ss*3+rr],xlab="discount (%)",ylab= "Expected Revenue ($/customer)",ylim=c(0,400))
  }
  mtext("Expected Revenue per Customer by Income Quartile", side = 3, line = 0, outer = TRUE)
  dev.off()
}

