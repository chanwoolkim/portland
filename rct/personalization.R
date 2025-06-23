#=========================================================================#
# personalization.R
# 
# Personalization algorithm for discounts
#
# June 18, 2025
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis/servus")
output_dir <- paste0(wd, "/../../Dropbox/Apps/Overleaf/Water Pricing/output")

library(glmnet)


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
# RCT participants only
load(paste0(working_data_dir, "/estimation_dataset.RData"))


#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
n_current <- estimation_dataset %>% filter(t==0) %>% distinct(id) %>% nrow()
print(paste0("All available accounts net of those that got final bill during information treatment: ",
             as.character(prettyNum(n_current, big.mark=",")),
             " (", round(n_current/20000, 2)*100, "%)"))

# Only select relevant sample
estimation_dataset <- estimation_dataset %>%
  filter(# PWB-officially recorded "exits" from RCT
    is.na(exit_reason),
    # Still have a handful of accounts with final bills
    account_status!="FINAL",
    !is_rebill,
    !first_bill,
    # Severe FAs received 50-80% discount
    (fa_type != "Tier2" | is.na(fa_type))) %>%
  mutate(B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t),
         # Bill defined as how much they owed (for RCT, net of previous debt)
         bill=if_else(t==0, O_t-D_t, O_t),
         payment=-E_t,
         pay=payment<bill,
         # Top-code payshare at 1
         payshare=if_else(O_t==0, NaN, pmin(pmax(payment/bill, 0), 1)),
         deadbeat=case_when(
           ufh ~ "Below UFH",
           !ufh & below_median_income ~ "Below Median Income",
           !ufh & !below_median_income ~ "Above Median Income"),
         delinquent=ifelse(D_t>0, "Have Unpaid Debt", "No Unpaid Debt"),
         iq=case_when(
           ufh ~ "Below UFH",
           !ufh & fa_eligible ~ "UFH to Means-Tested Cap",
           !ufh & !fa_eligible ~ "Above Means-Tested Cap"),
         cq=case_when(
           credit_quartile==1 ~ "Sub-Prime",
           credit_quartile==2 ~ "Near-Prime",
           credit_quartile==3 ~ "Prime",
           credit_quartile==4 ~ "Super-Prime"))

# Information (t==-1)
info_treat_data <- estimation_dataset %>% filter(t==-1)

# Discount (t==0)
dt <- estimation_dataset %>% filter(t==0)

n_analysis <- dt %>% filter(t==0) %>% distinct(id) %>% nrow()
print(paste0("Analysis sample size for discount treatment: ",
             as.character(prettyNum(n_analysis, big.mark=",")),
             " (", round(n_analysis/20000, 2)*100, "%)"))


#---------+---------+---------+---------+---------+---------+
# Personalization
#---------+---------+---------+---------+---------+---------+
# Note to JP: Quasi-Codebook
# 1. Use "bill" to look at what customers perceived as "amount due"
# 2. Use "payment" to look at what customers actually paid
# "delinquent" and "delinquent_at_randomization" are different
# - "delinquent" is having previous debt at the point of RCT bill
# All Census features have the prefix "census_"
# All Aspire North features have the prefix "aspire_"
dt$income = dt$tu_income


## ADD QUARTILES DURING TEST QUARTER
Iquartiles = quantile(dt$income,prob=c(.25,.5,.75),na.rm=T)
Cquartiles = quantile(dt$tu_credit_score,prob=c(.25,.5,.75),na.rm=T)
dt$IQ = 1*(dt$income<Iquartiles[1]) + 2*(dt$income>=Iquartiles[1] & dt$income<Iquartiles[2]) + 3*(dt$income>=Iquartiles[2] & dt$income<Iquartiles[3]) + 4*(dt$income>=Iquartiles[3])
dt$CQ = 1*(dt$tu_credit_score<Cquartiles[1])  +  2*(dt$tu_credit_score>=Cquartiles[1] & dt$tu_credit_score<Cquartiles[2])  +  3*(dt$tu_credit_score>=Cquartiles[2] & dt$tu_credit_score<Cquartiles[3])  +  4*(dt$tu_credit_score>=Cquartiles[3])


zz = factor(dt$discount_grid,levels=sort(unique(dt$discount_grid)),labels=paste(as.character(sort(unique(dt$discount_grid))),"%",sep=""))
zz1 = factor(dt$IQ,levels=sort(unique(dt$IQ)),labels=paste("IQ",as.character(sort(unique(dt$IQ))),sep=""))
zz2 = factor(dt$CQ,levels=sort(unique(dt$CQ)),labels=paste("CQ",as.character(sort(unique(dt$IQ))),sep=""))
dt$Dincome = dt$income/1000 - mean(dt$income/1000,na.rm=T)
dt$Dcredit_score = dt$tu_credit_score - mean(dt$tu_credit_score,na.rm=T)
dt$Ddelinq = dt$delinquent_at_randomization - mean(dt$delinquent_at_randomization)
dt$Dunemp = dt$census_unemployment_rate_in_labor_force - mean(dt$census_unemployment_rate_in_labor_force,na.rm=T)
dt$Dblack = dt$census_percent_of_population_includes_black - mean(dt$census_percent_of_population_includes_black,na.rm=T)
dt$Dlagwt = dt$lag_w_t - mean(dt$lag_w_t,na.rm=T)
dt$cell = zz
dt$IQfact = zz1
dt$CQfact = zz2

form = ~cell*IQfact+cell*CQfact+cell*delinquent_at_randomization-1
mm = (model.matrix(form,data=dt))
form1 = ~cell*delinquent_at_randomization-1
mm1 = (model.matrix(form1,data=dt))
form2 = ~cell*income+cell*tu_credit_score+cell*delinquent_at_randomization-1
mm2 = (model.matrix(form2,data=dt))
form3 = ~cell*Dincome+cell*Dcredit_score+cell*Ddelinq-1
mm3 = (model.matrix(form3,data=dt[!is.na(dt$Dincome),]))
form4 = ~cell*Dincome+cell*Dcredit_score+cell*Ddelinq+cell*Dunemp+cell*Dblack-1
mm4 = (model.matrix(form4,data=dt))

ind = !is.na(dt$tu_income)
summary(lm(dt$payshare[ind]~mm-1))
summary(lm(dt$payshare~mm1-1))
summary(lm(dt$payshare[ind]~mm2-1))
summary(lm(dt$payshare[ind]~mm3-1))
summary(lm(dt$payshare[ind]~mm4-1))


#---------+---------+---------+---------+---------+---------+
# Weighted Likelihood Bootstrap Lasso
#---------+---------+---------+---------+---------+---------+
###
# Define Model
###
form = ~cell*income+cell*tu_credit_score+cell+cell*delinquent_at_randomization+cell*lag_w_t+cell*has_environment_discount-1
#form = ~cell*IQfact+cell*CQfact+cell+cell*delinquent_at_randomization+cell*lag_w_t+cell*has_environment_discount-1
#form = ~cell*IQfact+cell*CQfact+cell+cell*Ddelinq+cell*Dblack-1
#form = ~cell*Dincome+cell*Dcredit_score+cell*Ddelinq+cell*Dblack-1
mm = (model.matrix(form,data=dt))
#y = dt$payment[!is.na(dt$Dblack)]
y = dt$payment[!is.na(dt$lag_w_t) & !is.na(dt$income)]
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
#save(gm.cfs,vars,cells,file="output/gmcfs.Rdata")

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
