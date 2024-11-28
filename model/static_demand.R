#=========================================================================#
# STATIC_DEMAND.R
# * Use pre-RCT data to estimate the reduced-form water usage and
#  payment propensities as functions of the state variables.
# * For now, use Linear model with interaction effects for heterogeneity
# 
# 
# Note:
#   * data are cross-section for a given quarter
#   * data contain 3 separate quarters (we can use each for robustness checks)
#
# Key Hypotheses at this stage:
#   - Water coefficients are zero (i.e., consumption exogenous to debt and money owed)
#   - Expenditure coefficients are one (i.e., people pay debt (civic duty))
#   - Expenditure coefficients on outstanding debt higher than current water bill (i.e., people understand incentives to avoid penalties)
#
# Challenge: confounds with self-selection (especially on expenditure policy)
#
#
# JP Dube, September 5, 2024
#   - edited November 18, 2024
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
library(data.table)
library(ggplot2)
library(glmnet)
library(censReg)

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
output_dir <- paste0(wd, "/output")


#---------+---------+---------+---------+---------+---------+
# Functions
#---------+---------+---------+---------+---------+---------+
mkgraphdat = function(modellist,m,mm,N){
  coeffs1 = coeffs2 = coeffs3 = coeffs4 = coeffs5 = matrix(0,N,3)
  oo = matrix(1,N,1)
  
  # Base Model
  beta = as.matrix(modellist[[1]]$coefficients)
  coeffs1[,1] = beta[1]*oo
  coeffs1[,2] = beta[2]*oo
  coeffs1[,3] = beta[3]*oo
  
  # Model with SES controls
  X = cbind(oo,m)
  colnames(X)[1] = "intercept"
  beta = as.matrix(modellist[[2]]$coefficients)
  coeffs2[,1] = beta[1]*oo + X[,4:ncol(X)]%*%as.matrix(beta[4:ncol(X)])
  coeffs2[,2] = beta[2]*oo
  coeffs2[,3] = beta[3]*oo
  
  # Interactions Model
  X = cbind(oo,mm)
  colnames(X)[1] = "intercept"
  beta = as.matrix(modellist[[3]]$coefficients)
  interactions = unlist(gregexpr(":", colnames(X)))>0
  interactions1 = unlist(gregexpr("D_t:", colnames(X)))>0 | unlist(gregexpr(":D_t", colnames(X)))>0
  interactions2 = unlist(gregexpr("B_t:", colnames(X)))>0 | unlist(gregexpr(":B_t", colnames(X)))>0
  D_ind = which(colnames(X)=="D_t")
  B_ind = which(colnames(X)=="B_t")
  coeffs3[,1] = X[,interactions==FALSE][,-c(D_ind,B_ind)]%*%beta[interactions==FALSE][-c(D_ind,B_ind)]
  coeffs3[,2] = X[,D_ind]%*%as.matrix(beta[D_ind]) + X[,interactions1==TRUE]%*%beta[interactions1==TRUE]
  coeffs3[,3] = X[,B_ind]%*%as.matrix(beta[B_ind]) + X[,interactions2==TRUE]%*%beta[interactions2==TRUE]
  
  # Cross-Validated Lasso
  X = cbind(oo,mm)
  colnames(X)[1] = "intercept"
  beta = as.matrix(coef(modellist[[4]]))
  interactions = unlist(gregexpr(":", colnames(X)))>0
  interactions1 = unlist(gregexpr("D_t:", colnames(X)))>0 | unlist(gregexpr(":D_t", colnames(X)))>0
  interactions2 = unlist(gregexpr("B_t:", colnames(X)))>0 | unlist(gregexpr(":B_t", colnames(X)))>0
  D_ind = which(colnames(X)=="D_t")
  B_ind = which(colnames(X)=="B_t")
  coeffs4[,1] = X[,interactions==FALSE][,-c(D_ind,B_ind)]%*%beta[interactions==FALSE][-c(D_ind,B_ind)]
  coeffs4[,2] = X[,D_ind]%*%as.matrix(beta[D_ind]) + X[,interactions1==TRUE]%*%beta[interactions1==TRUE]
  coeffs4[,3] = X[,B_ind]%*%as.matrix(beta[B_ind]) + X[,interactions2==TRUE]%*%beta[interactions2==TRUE]
  
  # Censored Regression
  X = cbind(oo,m)
  colnames(X)[1] = "intercept"
  beta = as.matrix(coef(modellist[[5]]))
  coeffs5[,1] = beta[1]*oo + X[,4:ncol(X)]%*%as.matrix(beta[4:ncol(X)])
  coeffs5[,2] = beta[2]*oo
  coeffs5[,3] = beta[3]*oo
  
  # output
  dat = data.frame(
    mod1_a=coeffs1[,1], mod1_b1=coeffs1[,2], mod1_b2=coeffs1[,3],
    mod2_a=coeffs2[,1], mod2_b1=coeffs2[,2], mod2_b2=coeffs2[,3],
    mod3_a=coeffs3[,1], mod3_b1=coeffs3[,2], mod3_b2=coeffs3[,3],
    mod4_a=coeffs4[,1], mod4_b1=coeffs4[,2], mod4_b2=coeffs4[,3],
    mod5_a=coeffs5[,1], mod5_b1=coeffs5[,2], mod5_b2=coeffs5[,3])
  

  return(dat=dat)
}


#---------+---------+---------+---------+---------+---------+
# Load data
#---------+---------+---------+---------+---------+---------+
#load(file=paste0(working_data_dir, "/portland_transunion.RData"))
#df = portland_transunion
load(file=paste0(working_data_dir, "/portland_cross_section_sample.RData "))
df=data.table(portland_cross_section_sample)


#---------+---------+---------+---------+---------+---------+
# Set up Data
#---------+---------+---------+---------+---------+---------+
# Drop missing data
df = df[apply(is.na(df),1,sum)==0,]
# remove redundant "lag" variables
df[ ,colnames(df)[grep("lag",colnames(df))]:=NULL]
# remove redundant "owed" variable
df[ ,colnames(df)[grep("O_t",colnames(df))]:=NULL]


## Outcome Variables
Outcomes = c("w_t","E_t")

## State Variables
States = c("D_t","B_t")

## Features
HHvars = c("credit_score","etie","hh_size","unemployment","hh_income","food_stamp","hh_poverty","hh_nocar","hispanic","black",colnames(df)[grep("prev",colnames(df))])


#---------+---------+---------+---------+---------+---------+
# Loop Over Quarters
#---------+---------+---------+---------+---------+---------+
output = NULL
count = 0
for(qq in 0:-2){
  ## Main effects
  m = as.matrix(df[df$year==qq ,c(States,HHvars),with=FALSE])
  N = nrow(m)

  ## Create Interaction Variables (for each quarter)
    InterVars = "~"
  for(tt in 1:length(HHvars)) {InterVars = paste(InterVars,HHvars[tt],"*D_t + ",sep="")}
  for(tt in 1:length(HHvars)) {
    if(tt<length(HHvars)){
      InterVars = paste(InterVars,HHvars[tt],"*B_t + ",sep="")
    } else{InterVars = paste(InterVars,HHvars[tt],"*B_t - 1",sep="")}
  }
  mm = model.matrix(as.formula(InterVars),data=df[df$year==qq,])
  
  ## Create Outcomes
  # Quarterly Water Consumption
  w = as.matrix(df$w_t[df$year==qq])

  # Quarterly Payments (top and bottom code between [$0,$1,000])
  E = as.matrix(df$E_t[df$year==qq])
  E[E<0] = 0
  E[E>1000] = 1000

  # Bill (top and bottom code Debt between [$0,$1,000])
  D = df$D_t[df$year==qq]
  D[D<0] = 0
  D[D>1000] = 1000
  
  B = df$B_t[df$year==qq]
  O = D+B

    
  #---------+---------+---------+---------+---------+---------+-----
  # Estimate Reduced-Form Water Consumption & Expenditure Policies
  #---------+---------+---------+---------+---------+---------+-----
  modellist_w = modellist_E = NULL
  
  # WATER
  modellist_w[[1]] = lm(w~m[,States])
  modellist_w[[2]] = lm(w~m)
  modellist_w[[3]] = lm(w~mm)
  mod_temp = cv.glmnet(mm,w,intercept=TRUE)
  modellist_w[[4]] = glmnet(x=mm,y=w,lambda=mod_temp$lambda.min,intercept=TRUE)
  modellist_w[[5]] = censReg(w~m,left = 0,right = 1000)

  # EXPENDITURE
  modellist_E[[1]] = lm(E~m[,States])
  modellist_E[[2]] = lm(E~m)
  modellist_E[[3]] = lm(E~mm)
  mod_temp = cv.glmnet(mm,E,intercept=TRUE)
  modellist_E[[4]] = glmnet(x=mm,y=E,lambda=mod_temp$lambda.min,intercept=TRUE)
  modellist_E[[5]] = censReg(E~m,left = 0,right = 1000)


  
  #---------+---------+---------+---------+---------+---------+
  # Assemble Water Consumption Results
  #  --> extract intercept and slopes (D and B) for each account
  #---------+---------+---------+---------+---------+---------+
  dat_w = mkgraphdat(modellist=modellist_w,m=m,mm=mm,N=N)
  dat_E = mkgraphdat(modellist=modellist_E,m=m,mm=mm,N=N)
  

  #---------+---------+---------+---------+---------+---------+
  # Water Consumption Plots
  #---------+---------+---------+---------+---------+---------+
  ggplot(data=dat_w,aes(x=mod3_a)) +
    geom_density(color="black", alpha=0.9) + xlim(-10,35) + 
    geom_density(aes(x=mod4_a),color="orange", alpha=0.9) + 
    geom_density(aes(x=mod2_a),col="darkred") +
    geom_vline(aes(xintercept=mod1_a[1]),linewidth=.5,col="darkgreen") +
    geom_density(aes(x=mod5_a),col="blue") +
    labs(x="Intercepts",y="density",title="Water Consumption Policy (Intercepts)",color="Legend") +
    theme(plot.title = element_text(hjust = 0.5,size=5),plot.subtitle = element_text(hjust = 0.5,size=4),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5),axis.title.x = element_text(size=5),axis.title.y = element_text(size=5))  

  ggplot(data=dat_w,aes(x=mod3_b1)) +
    geom_density(color="black", alpha=0.9) + xlim(-0.5,0.5) + 
    geom_density(aes(x=mod4_b1),color="orange", alpha=0.9) + 
    geom_vline(aes(xintercept=mod2_b1[1]),linewidth=.5,col="darkred") +
    geom_vline(aes(xintercept=mod1_b1[1]),linewidth=.5,col="darkgreen") +
    geom_vline(aes(xintercept=mod5_b1[1]),linewidth=.5,col="darkblue") +
    labs(x="Intercepts",y="density",title="Water Consumption Policy (Slope D_t)") +
    theme(plot.title = element_text(hjust = 0.5,size=5),plot.subtitle = element_text(hjust = 0.5,size=4),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5),axis.title.x = element_text(size=5),axis.title.y = element_text(size=5))  

  ggplot(data=dat_w,aes(x=mod3_b2)) +
    geom_density(color="black", alpha=0.9) + xlim(-15,15) + 
    geom_density(aes(x=mod4_b2),color="orange", alpha=0.9) + 
    geom_vline(aes(xintercept=mod2_b2[1]),linewidth=.5,col="darkred") +
    geom_vline(aes(xintercept=mod1_b2[1]),linewidth=.5,col="darkgreen") +
    geom_vline(aes(xintercept=mod5_b1[1]),linewidth=.5,col="darkblue") +
    labs(x="Intercepts",y="density",title="Water Consumption Policy (Slope B_t)") +
    theme(plot.title = element_text(hjust = 0.5,size=5),plot.subtitle = element_text(hjust = 0.5,size=4),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5),axis.title.x = element_text(size=5),axis.title.y = element_text(size=5))  
  
  
  #---------+---------+---------+---------+---------+---------+
  # Expenditure Plots
  #---------+---------+---------+---------+---------+---------+
  ggplot(data=dat_E,aes(x=mod3_a))+
    geom_density(color="black", alpha=0.9) + xlim(-100,300) + 
    geom_density(aes(x=mod4_a),color="orange", alpha=0.9) + 
    geom_density(aes(x=mod2_a),col="darkred") +
    geom_vline(aes(xintercept=mod1_a[1]),linewidth=.5,col="darkgreen") +
    labs(x="Intercepts",y="density",title="Expenditure Policy (Intercepts)",color="Legend") +
    geom_density(aes(x=mod5_a),col="darkblue") +
    theme(plot.title = element_text(hjust = 0.5,size=5),plot.subtitle = element_text(hjust = 0.5,size=4),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5),axis.title.x = element_text(size=5),axis.title.y = element_text(size=5))  
  
  ggplot(data=dat_E,aes(x=mod3_b1))+
    geom_density(color="black", alpha=0.9) + xlim(-0.15,0.15) + 
    geom_density(aes(x=mod4_b1),color="orange", alpha=0.9) + 
    geom_vline(aes(xintercept=mod2_b1[1]),linewidth=.5,col="darkred") +
    geom_vline(aes(xintercept=mod1_b1[1]),linewidth=.5,col="darkgreen") +
    geom_vline(aes(xintercept=mod5_b1[1]),linewidth=.5,col="darkblue") +
    labs(x="Intercepts",y="density",title="Expenditure Policy (Slope D_t)") +
    theme(plot.title = element_text(hjust = 0.5,size=5),plot.subtitle = element_text(hjust = 0.5,size=4),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5),axis.title.x = element_text(size=5),axis.title.y = element_text(size=5))  
  
  ggplot(data=dat_E,aes(x=mod3_b2))+
    geom_density(color="black", alpha=0.9) + xlim(-50,275) + 
    geom_density(aes(x=mod4_b2),color="orange", alpha=0.9) + 
    geom_vline(aes(xintercept=mod2_b2[1]),linewidth=.5,col="darkred") +
    geom_vline(aes(xintercept=mod1_b2[1]),linewidth=.5,col="darkgreen") +
    geom_vline(aes(xintercept=mod5_b1[1]),linewidth=.5,col="darkblue") +
    labs(x="Intercepts",y="density",title="Expenditure Policy (Slope B_t)") +
    theme(plot.title = element_text(hjust = 0.5,size=5),plot.subtitle = element_text(hjust = 0.5,size=4),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5),axis.title.x = element_text(size=5),axis.title.y = element_text(size=5))
  
  # store output
  count = count+1
  output[[count]] = list(N=N,m=m,mm=mm,w=w,E=E,modellist_E=modellist_E,modellist_w=modellist_w)
}

save(output,file=paste(output_dir,"/Results.Rdata",sep=""))
