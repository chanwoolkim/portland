#=========================================================================#
# WLB.R
# * Use pre-RCT data to estimate the reduced-form water usage and
#  payment propensities as functions of the state variables via WLB.
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
# JP Dube, December 13, 2024
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

mkRev = function(R,Ba,Bb,Bc,Bd,Be,Bf,Bg,D,B){
  Rev = mean( Ba + Bb*D + Bc*B*R + Bd*D^2 + Be*(B*R)^2 + Bf*D^3 + Bg*(B*R)^3 )
  return(Rev)
}


#---------+---------+---------+---------+---------+---------+
# Load data
#---------+---------+---------+---------+---------+---------+
load(paste(output_dir,"/estimdata.Rdata",sep=""))
NB = 3 		# Number of Bootstraps


#---------+---------+---------+---------+---------+---------+
# Set Uo data for most recent quarter
#---------+---------+---------+---------+---------+---------+
qq=0

## Main effects
m = as.matrix(df[df$year==qq ,c(States,HHvars),with=FALSE])
N = nrow(m)

## Create Interaction Variables (for each quarter)
df$D_t2 = df$D_t^2
df$B_t2 = df$B_t^2
df$D_t3 = df$D_t^3
df$B_t3 = df$B_t^3
InterVars = "~"
for(tt in 1:length(HHvars)) {
  InterVars = paste(InterVars,HHvars[tt],"*D_t + ",sep="")
  InterVars = paste(InterVars,HHvars[tt],"*D_t2 + ",sep="")
  InterVars = paste(InterVars,HHvars[tt],"*D_t3 + ",sep="")
}
for(tt in 1:length(HHvars)) {
  if(tt<length(HHvars)){
    InterVars = paste(InterVars,HHvars[tt],"*B_t + ",sep="")
    InterVars = paste(InterVars,HHvars[tt],"*B_t2 + ",sep="")
    InterVars = paste(InterVars,HHvars[tt],"*B_t3 + ",sep="")
  } else{InterVars = paste(InterVars,HHvars[tt],"*B_t3 - 1",sep="")}
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
# Estimate WLB Reduced-Form Water Consumption & Expenditure Policies
#---------+---------+---------+---------+---------+---------+-----
## SECOND RUN GLMnet BOOTSTRAP
gmd_w.out =  gm_w.out=list()
gmd_E.out =  gm_E.out=list()
set.seed(1234)
for(bb in 1:NB)
{
  wts = rexp(N)^1.8 							              # Rubin's Dirichlet Weighting approach
  idx = sample(1:N,N,prob=wts,replace=TRUE) 	  # Sample based on Dirichlet Weights
  ## (1) Water Consumption
  gm_w.out[[bb]] = cv.glmnet(mm[idx,],w[idx],intercept=TRUE) 	# Run Cross Validated Glmnet
  gmd_w.out[[bb]] = glmnet(mm[idx,],w[idx],lambda=gm_w.out[[bb]]$lambda.min,intercept=TRUE) 		    # Post Selection GLM
  
  ## (2) Expenditure
  gm_E.out[[bb]] = cv.glmnet(mm[idx,],E[idx],intercept=TRUE) 	# Run Cross Validated Glmnet
  gmd_E.out[[bb]] = glmnet(mm[idx,],E[idx],lambda=gm_E.out[[bb]]$lambda.min,intercept=TRUE) 		    # Post Selection GLM
  
  cat("Bootstrap ",bb," completed.\n")
}


#---------+---------+---------+---------+---------+---------+-----
# Decision Theory
#---------+---------+---------+---------+---------+---------+-----
beta_w = beta_E = matrix(0,nrow(coef(gmd_E.out[[1]])),NB)
for(bb in 1:NB){
  beta_w[,bb] = as.matrix(coef(gmd_w.out[[bb]]))
  beta_E[,bb] = as.matrix(coef(gmd_E.out[[bb]]))
}
m = as.matrix(df[df$year==qq ,c(States,HHvars),with=FALSE])
R = Ehat = Estar = matrix(0,N,1)
coeffs = list(a=matrix(0,N,NB),b=matrix(0,N,NB),c=matrix(0,N,NB),d=matrix(0,N,NB),e=matrix(0,N,NB),f=matrix(0,N,NB),g=matrix(0,N,NB))
for(nn in 1:N){
  X = cbind(1,matrix(mm[nn,],nrow=1))
  colnames(X) = c("intercept",colnames(mm))
  interactions = unlist(gregexpr(":", colnames(X)))>0
  interactions1 = unlist(gregexpr("D_t:", colnames(X)))>0 | unlist(gregexpr(":D_t", colnames(X)))>0
  interactions2 = unlist(gregexpr("B_t:", colnames(X)))>0 | unlist(gregexpr(":B_t", colnames(X)))>0
  interactions11 = unlist(gregexpr("D_t2:", colnames(X)))>0 | unlist(gregexpr(":D_t2", colnames(X)))>0
  interactions22 = unlist(gregexpr("B_t2:", colnames(X)))>0 | unlist(gregexpr(":B_t2", colnames(X)))>0
  interactions111 = unlist(gregexpr("D_t3:", colnames(X)))>0 | unlist(gregexpr(":D_t3", colnames(X)))>0
  interactions222 = unlist(gregexpr("B_t3:", colnames(X)))>0 | unlist(gregexpr(":B_t3", colnames(X)))>0
  D_ind = which(colnames(X)=="D_t")
  B_ind = which(colnames(X)=="B_t")
  D2_ind = which(colnames(X)=="D_t2")
  B2_ind = which(colnames(X)=="B_t2")
  D3_ind = which(colnames(X)=="D_t3")
  B3_ind = which(colnames(X)=="B_t3")
  coeffs$a[nn,] = matrix(X[,interactions==FALSE],nrow=1)[,-c(D_ind,B_ind)]%*%beta_E[interactions==FALSE,][-c(D_ind,B_ind),]
  coeffs$b[nn,] = matrix(X[,D_ind],nrow=1)%*%matrix(beta_E[D_ind,],ncol=NB) + matrix(X[,interactions1==TRUE],nrow=1)%*%beta_E[interactions1==TRUE,]
  coeffs$c[nn,] = matrix(X[,B_ind],nrow=1)%*%matrix(beta_E[B_ind,],ncol=NB) + matrix(X[,interactions2==TRUE],nrow=1)%*%beta_E[interactions2==TRUE,]
  coeffs$d[nn,] = matrix(X[,D2_ind],nrow=1)%*%matrix(beta_E[D2_ind,],ncol=NB) + matrix(X[,interactions11==TRUE],nrow=1)%*%beta_E[interactions11==TRUE,]
  coeffs$e[nn,] = matrix(X[,B2_ind],nrow=1)%*%matrix(beta_E[B2_ind,],ncol=NB) + matrix(X[,interactions22==TRUE],nrow=1)%*%beta_E[interactions22==TRUE,]
  coeffs$f[nn,] = matrix(X[,B3_ind],nrow=1)%*%matrix(beta_E[B3_ind,],ncol=NB) + matrix(X[,interactions111==TRUE],nrow=1)%*%beta_E[interactions111==TRUE,]
  coeffs$g[nn,] = matrix(X[,B3_ind],nrow=1)%*%matrix(beta_E[B3_ind,],ncol=NB) + matrix(X[,interactions222==TRUE],nrow=1)%*%beta_E[interactions222==TRUE,]
  a=coeffs$a[nn,]
  b=coeffs$b[nn,]
  c=coeffs$c[nn,]
  d=coeffs$d[nn,]
  e=coeffs$e[nn,]
  f=coeffs$f[nn,]
  g=coeffs$g[nn,]
  
  R[nn] = optimize(f=mkRev,c(0,1),maximum=T,Ba=a,Bb=b,Bc=c,Bd=d,Be=e,Bf=f,Bg=g,D=D[nn],B=B[nn])$maximum
  Estar[nn] = mkRev(R=R[nn],Ba=a,Bb=b,Bc=c,Bd=d,Be=e,Bf=f,Bg=g,D=D[nn],B=B[nn])
  Ehat[nn] = mkRev(R=0,Ba=a,Bb=b,Bc=c,Bd=d,Be=e,Bf=f,Bg=g,D=D[nn],B=B[nn])
  
}
