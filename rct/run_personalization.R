#=========================================================================#
#
#                 Run the WLB using parallelization
#
#
#       by JP Dube, 8-28-2025
#
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# PRELIMINARIES
#---------+---------+---------+---------+---------+---------+
set.seed(1)

if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
} else if (Sys.info()[4]=="jdube01"){
  wd = "/data/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(wd, "/output")

# Load libraries
library(glmnet)
library(ggplot2)
library(ggridges)

# Parallelization
library(foreach)
parallel::detectCores()
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)



load(file=paste0(working_data_dir, "/servus/analysis/personalization_dataset.RData"))

#---------+---------+---------+---------+
# Variables for Analysis: 
#  y = Actual Payment ($)
#  -- bottom-coded at $0
# treatment = Matrix of treatment assignments
# features = Matrix of features
#---------+---------+---------+---------+
keeper = !is.na(dt$lag_w_t) & !is.na(dt$income)
y = dt$payment[keeper]
y[y<0] = 0
y1 = dt$payshare[keeper]
y1[y==0 & is.na(y1)] = 0
y2 = dt$pay[keeper]

treatment = model.matrix(~dt$cell-1)

featurelist = c("lag_w_t","income","delinquent",census_namelist, aspire_namelist)
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
    out = lm(y~x)
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
vars = colnames(mm)
cells = vars[1:9]


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
  bdrawtemp     = cv.glmnet(mm,y,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmRev.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Revenue model: ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_Rev.Rdata",sep="")
save(gmRev.cfs,vars,cells,file=outfile)


## (2) Payment Share
start = proc.time()
gmPayShare.cfs <- foreach(
  bb = 1:NB, 
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y1,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmPayShare.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Pay Share: ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_PayShare.Rdata",sep="")
save(gmPayShare.cfs,vars,cells,file=outfile)


## (3) Pay Indicator
start = proc.time()
gmPay.cfs <- foreach(
  bb = 1:NB, 
  #  .combine = 'c',
  .combine = 'cbind',
  .packages = c("glmnet")
) %dopar% {
  wts <- rexp(N)                                                                                              # Rubin's Dirichlet Weighting approach
  bdrawtemp     = cv.glmnet(mm,y2,family='gaussian',penalty.factor = penalty,weights = wts) 	            # Run Cross Validated
  gmPay.cfs = as.matrix(coef(bdrawtemp,s="lambda.min"))
}
cat("Running time for Pay Indicator model: ",(proc.time()-start)[3],"seconds")

outfile = paste(output_dir,"/gmcfs_Pay.Rdata",sep="")
save(gmPay.cfs,vars,cells,file=outfile)


###
# SAVE All Coeffs (gm.cfs)
###
outfile = paste(output_dir,"/gmcfs.Rdata",sep="")
save(gmRev.cfs,gmPayShare.cfs,gmPay.cfs,vars,cells,file=outfile)


#---------+---------+---------+---------+
# Compute Heterog Treatment Effects
#---------+---------+---------+---------+
X = cbind(matrix(1,N,1),mm)
colnames(X)[1] = "intercept"
main = -grep("cell",colnames(X))
aRev = X[,main]%*%gmRev.cfs[main,]
aPayShare = X[,main]%*%gmPayShare.cfs[main,]
aPay = X[,main]%*%gmPay.cfs[main,]
cellinter = FeatInters = bRev = bPayShare = bPay = treatsimRev = treatsimPayShare = treatsimPay = NULL
ERev = EPayShare = EPay = matrix(1,nrow(mm),9)
count=1
for(ii in 1:length(cells)){
  cellinter[[ii]] = grep(paste(cells[ii],":",sep=""),colnames(X))
  cellmain = grep(paste("\\b",cells[ii],"\\b",sep=""),colnames(X))
  FeatInters[[ii]] = gsub(paste(cells[ii],":",sep=""),"",colnames(X)[grep(paste(cells[ii],":",sep=""),colnames(X))])
  total = c(cellmain,cellinter[[ii]])
  # Revenue
  bRev[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gmRev.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gmRev.cfs[cellinter[[ii]],]
  treatsimRev[[ii]] = aRev + bRev[[ii]]
  ERev[,ii] = apply(treatsimRev[[ii]],1,mean)
  # Pay Share
  bPayShare[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gmPayShare.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gmPayShare.cfs[cellinter[[ii]],]
  treatsimPayShare[[ii]] = aPayShare + bPayShare[[ii]]
  EPayShare[,ii] = apply(treatsimPayShare[[ii]],1,mean)
  # Pay
  bPay[[ii]] = matrix(1,nrow(mm),1)%*%matrix(gmPay.cfs[cellmain,],nrow=1) + matrix(X[,FeatInters[[ii]]],nrow=nrow(X))%*%gmPay.cfs[cellinter[[ii]],]
  treatsimPay[[ii]] = aPay + bPay[[ii]]
  EPay[,ii] = apply(treatsimPay[[ii]],1,mean)
}
colnames(ERev) = colnames(EPayShare) = colnames(EPay) = colnames(X)[2:10]

TERev = matrix(ERev[,2:9]-matrix(rep(ERev[,1],8),ncol=8))
TEPayShare = matrix(EPayShare[,2:9]-matrix(rep(EPayShare[,1],8),ncol=8))*100
TEPay = matrix(EPay[,2:9]-matrix(rep(EPay[,1],8),ncol=8))


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


#ggplot(df, aes(x =`T_PayShare`,y=`cell`,fill = after_stat(x))) +
#  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.005, alpha = 0.8, color = "white", size = 0.2) +
#  #theme_ridges(font_size = 12) +
#  theme_ridges(font_size = 12, grid = TRUE) +
#  guides(fill = "none") +
#  xlim(-10,5) +
#  labs(title = "Treatment Effect on Payment Share (%)",x = "treament effect (%)", y = "cell")


#ggplot(df, aes(x =`T_Pay`,y=`cell`,fill = after_stat(x))) +
#  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.005, alpha = 0.8, color = "white", size = 0.2) +
#  #theme_ridges(font_size = 12) +
#  theme_ridges(font_size = 12, grid = TRUE) +
#  guides(fill = "none") +
#  #xlim(-400,100) +
#  labs(title = "Treatment Effect on Delinquency (%)",x = "treament effect (%)", y = "cell")


#---------+---------+---------+---------+---------+---------+---------+---------+
# PRICING POLICIES
# Use Bayesian Decision Theory
# -- i.e., via posterior means
#---------+---------+---------+---------+---------+---------+---------+---------+
load("output/gmcfs.Rdata")



# NO DISCOUNT
Yunif = ERev[,1]
mean(Yunif)

# Revenue-Maximizing DISCOUNT
policy = max.col(ERev)
Yoptim = Yunif*0
for(ii in 1:length(Yoptim)){Yoptim[ii] = ERev[ii,policy[ii]]}
mean(Yoptim)
# Who gets discount?
# (1) People already receiving a discount?
table(id$linc_tier_type_at_bill)/nrow(id)
table(id$linc_tier_type_at_bill[policy>1])/sum(policy>1)

# (2) Low income?
summary(id$income)
summary(id$income[policy>1])


# (3) Outstanding debt?
summary(id$D_t)
summary(id$D_t[policy>1])




#---------+---------+---------+---------+---------+---------+---------+---------+
# Description of who over-pays
#---------+---------+---------+---------+---------+---------+---------+---------+
# payment ($ and %)
aggregate(dt$payment~cell,data=id,FUN=mean)
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
