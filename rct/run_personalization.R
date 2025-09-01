#=========================================================================#
#---------+---------+---------+---------+
# Variables for Analysis: 
#  y = Actual Payment ($)
#  -- bottom-coded at $0
# treatment = Matrix of treatment assignments
# features = Matrix of features
#---------+---------+---------+---------+
keeper = !is.na(dt$lag_w_t) & !is.na(dt$income)
# Dollar Payments
y1 = dt$payment[keeper]
y1[y1<0] = 0
# Share of total bill (excluding debt)
y2 = (dt$payment/(dt$B_t+dt$D_t))[keeper]
y2[y1==0 & is.na(y2)] = 0
# Share of total amount owed (including debt)
y3 = (dt$payment/(dt$B_t))[keeper]
y3[y1==0 & is.na(y3)] = 0
# Dummy for delinquent (i.e., some portion bill unpaid)
y4 = (dt$payment<(dt$B_t+dt$D_t))[keeper]
# Dummy for delinquent (i.e., some portion bill unpaid)
y5 = (dt$payment<(dt$B_t))[keeper]

treatment = model.matrix(~dt$cell-1)

featurelist = c("lag_w_t","income","delinquent",census_namelist, aspire_namelist)     # SHOULD WE INCLUDE DEBT (D_t) IN HERE???
features = dt[,featurelist]


#---------+---------+---------+---------+---------+---------+---------+
# RUN MARGINAL REGRESSIONS
#  Uses p-values (thought probably better to use correlations)
#---------+---------+---------+---------+---------+---------+---------+
outMargReg = matrix(0,length(featurelist),2)
for(rr in 1:length(featurelist)){
  x = as.matrix(dt[keeper,featurelist[rr]])
  if(sum(is.na(x))==0 & length(unique(x))>1){
    #formtemp = formula(paste("~cell*",featurelist[rr],sep=""))
    #mtemp = model.matrix(formtemp,data=dt)
    out = lm(y1~x)
    outMargReg[rr,] = cbind(summary(out)$coefficients[2,4],summary(out)$r.squared)}
  else {outMargReg[rr,] = c(1,0)}
}
keepvar = sqrt(outMargReg[,2])>= .05


#---------+---------+---------+---------+---------+---------+
# Regularization & Bayesian Decision Theory
# Weighted Likelihood Bootstrap Lasso
# (1) expected payment (linear)
# (2) expected % paid (linear prob)
# (3) paid (logit)
#---------+---------+---------+---------+---------+---------+
###
# Define Model
###
modellist = featurelist[keepvar]
form = paste("~",paste("cell*",modellist,"+",sep="",collapse=""),sep="")
form = formula(paste(substr(form,1,nchar(form)-1),"-1"))
mm = (model.matrix(form,data=dt))
dt$overpay = dt$payment>dt$B_t
# credit-related variables
id = dt[keeper,c("id","discount_grid","fa_type","delinquent_at_randomization","fa_eligible","income","below_median_income","payment_plan","tract_id","crisis_voucher","linc_tier_type_at_bill","D_t","B_t","O_t","overpay","cell","payment","IQ", "aspire_con_prof")]
id$linc_tier_type_at_bill[is.na(id$linc_tier_type_at_bill)] = "Not Enrolled"
id$D_t[id$D_t<0] = NaN


###
# Set params
###
K <- ncol(mm)     # Dimension of Variables (in design matrix)
N <- nrow(mm)
penalty <- rbind( matrix(0, 9, 1), matrix(1, K - 9,1))  # force glm to retain intercept and main effects of treatment arms
NB=30
N = nrow(mm)

###
# initialize outputs
###
gmRev.out=gmPayShare.out=gmPay.out=list()
set.seed(2)


###
# Bootstrap loop
###

## (1) Revenue
start = proc.time()
gmRev.cfs <- foreach(
  bb = 1:NB, 
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y1,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmRev.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Revenue model: ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_Rev.Rdata",sep="")
save(gmRev.cfs,vars,cells,file=outfile)


## (2) Payment Share (total amount owed)
start = proc.time()
gmPayShare.cfs <- foreach(
  bb = 1:NB, 
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y2,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmPayShare.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Pay Share (Total): ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_PayShare.Rdata",sep="")
save(gmPayShare.cfs,vars,cells,file=outfile)


## (3) Payment Share (bill excluding debt)
start = proc.time()
gmPayShareBill.cfs <- foreach(
  bb = 1:NB, 
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y3,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmPayShareBill.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Pay Share (Bill): ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_PayShareBill.Rdata",sep="")
save(gmPayShareBill.cfs,vars,cells,file=outfile)


## (4) Pay Indicator (total amount owed)
start = proc.time()
gmPay.cfs <- foreach(
  bb = 1:NB, 
  #  .combine = 'c',
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y4,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmPay.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Pay Indicator model: ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_Pay.Rdata",sep="")
save(gmPay.cfs,vars,cells,file=outfile)


## (5) Pay Indicator (bill excluding debt)
start = proc.time()
gmPayBill.cfs <- foreach(
  bb = 1:NB, 
  #  .combine = 'c',
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y5,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmPayBill.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Pay Indicator model (Bill): ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_PayBill.Rdata",sep="")
save(gmPayBill.cfs,vars,cells,file=outfile)


###
# SAVE All Coeffs (gm.cfs)
###
outfile = paste(output_dir,"/gmcfs.Rdata",sep="")
save(gmRev.cfs,gmPayShare.cfs,gmPayShareBill.cfs,gmPay.cfs,gmPayBill.cfs,vars,cells,file=outfile)


#---------+---------+---------+---------+
# Compute Heterog Treatment Effects
#---------+---------+---------+---------+
coefs = list(gmRev.cfs,gmPayShare.cfs,gmPayShareBill.cfs,gmPay.cfs,gmPayBill.cfs)
objectives = c("Revenue","Pay Share (total)","Pay Share (Bill)","Deliquent (total)","Delinquent (Bill)")
X = cbind(matrix(1,N,1),mm)
colnames(X)[1] = "intercept"
cellnames = colnames(X)[2:10]
main = -grep("cell",colnames(X))
#aRev = X[,main]%*%gmRev.cfs[main,]
#aPayShare = X[,main]%*%gmPayShare.cfs[main,]
#aPay = X[,main]%*%gmPay.cfs[main,]
#cellinter = FeatInters = bRev = bPayShare = bPay = treatsimRev = treatsimPayShare = treatsimPay = NULL
#ERev = EPayShare = EPay = matrix(1,nrow(mm),9)
E = TE = a = b = cellinter = FeatInters = treatsim = NULL
for(jj in 1:length(coefs)){
  #cellinter = FeatInters = bRev = bPayShare = bPay = treatsimRev = treatsimPayShare = treatsimPay = NULL
  gm = coefs[[jj]]
  a[[jj]] = X[,main]%*%gm[main,]
  E[[jj]] = matrix(1,N,length(cells))
  btemp = treattemp = NULL
  for(ii in 1:length(cells)){
    cellinter[[ii]] = grep(paste(cells[ii],":",sep=""),colnames(X))
    cellmain = grep(paste("\\b",cells[ii],"\\b",sep=""),colnames(X))
    FeatInters[[ii]] = gsub(paste(cells[ii],":",sep=""),"",colnames(X)[grep(paste(cells[ii],":",sep=""),colnames(X))])
    total = c(cellmain,cellinter[[ii]])
    btemp[[ii]] = matrix(1,N,1)%*%matrix(gm[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gm[cellinter[[ii]],]
    treattemp[[ii]] = a[[jj]] + btemp[[ii]]
    E[[jj]][,ii] = apply(treattemp[[ii]],1,mean)
    colnames(E[[jj]]) = cellnames
  }
  b[[jj]] = btemp
  treatsim[[jj]] = treattemp
  TE[[jj]] = matrix(E[[jj]][,2:length(cells)]-matrix(rep(E[[jj]][,1],length(cells)-1),ncol=length(cells)-1))
}


#  }
#  # Revenue
#  bRev[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gmRev.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gmRev.cfs[cellinter[[ii]],]
#  treatsimRev[[ii]] = aRev + bRev[[ii]]
#  ERev[,ii] = apply(treatsimRev[[ii]],1,mean)
#  # Pay Share
#  bPayShare[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gmPayShare.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gmPayShare.cfs[cellinter[[ii]],]
#  treatsimPayShare[[ii]] = aPayShare + bPayShare[[ii]]
#  EPayShare[,ii] = apply(treatsimPayShare[[ii]],1,mean)
#  # Pay
#  bPay[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gmPay.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gmPay.cfs[cellinter[[ii]],]
#  treatsimPay[[ii]] = aPay + bPay[[ii]]
#  EPay[,ii] = apply(treatsimPay[[ii]],1,mean)
#}
#colnames(ERev) = colnames(EPayShare) = colnames(EPay) = colnames(X)[2:10]

#TERev = matrix(ERev[,2:9]-matrix(rep(ERev[,1],8),ncol=8))
#TEPayShare = matrix(EPayShare[,2:9]-matrix(rep(EPayShare[,1],8),ncol=8))*100
#TEPay = matrix(EPay[,2:9]-matrix(rep(EPay[,1],8),ncol=8))


#---------+---------+---------+---------+---------+---------+---------+---------+
# Plot distributions of Treatment Effects
#---------+---------+---------+---------+---------+---------+---------+---------+
df = data.frame(T_Rev=TERev,T_PayShare=TEPayShare,T_Pay=TEPay,cell=as.matrix(paste(kronecker(matrix(seq(from=10,by=10,to=80)),matrix(1,N,1)),"%",sep="")))

ggplot(df, aes(x =`T_Rev`,y=`cell`,fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.005, alpha = 0.8, color = "white", size = 0.2) +
  #theme_ridges(font_size = 12) +
  theme_ridges(font_size = 12, grid = TRUE) +
  guides(fill = "none") +
  xlim(-400,100) +
  labs(title = "Treatment Effect on Payments ($)",x = "treament effect ($)", y = "cell")


ggplot(df, aes(x =`T_PayShare`,y=`cell`,fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.005, alpha = 0.8, color = "white", size = 0.2) +
  #theme_ridges(font_size = 12) +
  theme_ridges(font_size = 12, grid = TRUE) +
  guides(fill = "none") +
  xlim(-10,5) +
  labs(title = "Treatment Effect on Payment Share (%)",x = "treament effect (%)", y = "cell")


ggplot(df, aes(x =`T_Pay`,y=`cell`,fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.005, alpha = 0.8, color = "white", size = 0.2) +
  #theme_ridges(font_size = 12) +
  theme_ridges(font_size = 12, grid = TRUE) +
  guides(fill = "none") +
  #xlim(-400,100) +
  labs(title = "Treatment Effect on Delinquency (%)",x = "treament effect (%)", y = "cell")


#---------+---------+---------+---------+---------+---------+---------+---------+
# PRICING POLICIES
# Use Bayesian Decision Theory
# -- i.e., via posterior means
#---------+---------+---------+---------+---------+---------+---------+---------+
load("output/gmcfs.Rdata")


# NO DISCOUNT
YRevunif = ERev[,1]
print(paste0("Expected Revenue with No Discount: $", round(mean(YRevunif),2)))
cat()
YPayShareunif = EPayShare[,1]
print(paste0("Expected Pay Share with No Discount: ", round(mean(YPayShareunif)*100,2),"%"))
cat()
YPayunif = EPay[,1]
print(paste0("Expected Pay Propensity with No Discount: ", round(mean(YPayunif)*100,2),"%"))

# Revenue-Maximizing DISCOUNT
policyRev = max.col(ERev)
YRev = YRevunif*0
for(ii in 1:length(YRev)){YRev[ii] = ERev[ii,policyRev[ii]]}
print(paste0("Optimized Expected Revenue: $", round(mean(YRev)*100,2)))
# Who gets discount?
# (1) People already receiving a discount?
table(id$linc_tier_type_at_bill)/nrow(id)
table(id$linc_tier_type_at_bill[policyRev>1])/sum(policyRev>1)

# (2) Low income?
summary(id$income)
summary(id$income[policyRev>1])


# (3) Outstanding debt?
summary(id$D_t)
summary(id$D_t[policyRev>1])


# PayShare-Maximizing DISCOUNT
policyPayShare = max.col(EPayShare)
YPayShare = Yunif*0
for(ii in 1:length(YPayShare)){YPayShare[ii] = EPayShare[ii,policyPayShare[ii]]}
print(paste0("Optimized PayShare: ", round(mean(YPayShare)*100,2),"%"))
# Who gets discount?
# (1) People already receiving a discount?
table(id$linc_tier_type_at_bill)/nrow(id)
table(id$linc_tier_type_at_bill[policyPayShare>1])/sum(policyPayShare>1)


# Pay Propensity-Maximizing DISCOUNT
policyPay = max.col(EPay)
YPay = Yunif*0
for(ii in 1:length(YPay)){YPay[ii] = EPay[ii,policyPay[ii]]}
print(paste0("Optimized Pay Propensity: ", round(mean(YPay)*100,2),"%"))
# Who gets discount?
# (1) People already receiving a discount?
table(id$linc_tier_type_at_bill)/nrow(id)
table(id$linc_tier_type_at_bill[policyPay>1])/sum(policyPay>1)


#---------+---------+---------+---------+---------+---------+---------+---------+
# Description of who over-pays
#---------+---------+---------+---------+---------+---------+---------+---------+
# payment ($ and %)
aggregate(payment~cell,data=id,FUN=mean)
aggregate((payment)/(B_t)*100~cell,data=id,FUN=mean)
aggregate((payment)/(B_t+D_t)*100~cell,data=id,FUN=mean)

# Pay in full vs no-pay
aggregate(((payment)/(B_t)>=1)~cell,data=id,FUN=mean)
aggregate(((payment)/(B_t+D_t)>=1)~cell,data=id,FUN=mean)
aggregate((payment>0)~cell,data=id,FUN=mean)

# With vs without debt
aggregate(payment,data=id,FUN=mean,by=list(cell,D_t>0),na.rm=T)
aggregate((payment)/(B_t)*100,data=id,FUN=mean,by=list(cell,D_t>0),na.rm=T)
aggregate((payment)/(B_t+D_t)*100,data=id,FUN=mean,by=list(cell,D_t>0),na.rm=T)

aggregate(((id$payment)/(id$B_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0),na.rm=T)
aggregate(((id$payment)/(id$B_t+id$D_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0),na.rm=T)
aggregate((id$payment>0),FUN=mean,by=list(id$cell,id$D_t>0),na.rm=T)


# (With vs without debt and above/below $300 shut-off threshold)
aggregate(id$payment,FUN=mean,by=list(id$cell,id$D_t>0,id$D_t+id$B_t>300),na.rm=T)
aggregate(((id$payment)/(id$B_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,id$D_t+id$B_t>300),na.rm=T)
aggregate(((id$payment)/(id$B_t+id$D_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,id$D_t+id$B_t>300),na.rm=T)
aggregate((id$payment>0),FUN=mean,by=list(id$cell,id$D_t>0,id$D_t+id$B_t>300),na.rm=T)


# (With vs without debt and above/below income quartiles)
aggregate(id$payment,FUN=mean,by=list(id$cell,id$D_t>0,id$below_median_income),na.rm=T)
aggregate(((id$payment)/(id$B_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,id$below_median_income),na.rm=T)
aggregate(((id$payment)/(id$B_t+id$D_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,id$below_median_income),na.rm=T)
aggregate((id$payment>0),FUN=mean,by=list(id$cell,id$D_t>0,id$below_median_income),na.rm=T)

# (With vs without debt and above/below $50K)
aggregate(id$payment,FUN=mean,by=list(id$cell,id$D_t>0,id$income<50),na.rm=T)
aggregate(((id$payment)/(id$B_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,id$income<50),na.rm=T)
aggregate(((id$payment)/(id$B_t+id$D_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,id$income<50),na.rm=T)
aggregate((id$payment>0),FUN=mean,by=list(id$cell,id$D_t>0,id$income<50),na.rm=T)


# (With vs without debt and above/below Profit Score (1 is highest score))
aggregate(id$payment,FUN=mean,by=list(id$cell,id$D_t>0,as.numeric(id$aspire_con_prof)>4),na.rm=T)
aggregate(((id$payment)/(id$B_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,as.numeric(id$aspire_con_prof)>4),na.rm=T)
aggregate(((id$payment)/(id$B_t+id$D_t)>=1),FUN=mean,by=list(id$cell,id$D_t>0,as.numeric(id$aspire_con_prof)>4),na.rm=T)
aggregate((id$payment>0),FUN=mean,by=list(id$cell,id$D_t>0,as.numeric(id$aspire_con_prof)<4),na.rm=T)
