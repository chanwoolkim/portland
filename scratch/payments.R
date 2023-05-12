#=========================================================================#
# payments.R
#
# Analyze bill panel data
#
# JP Dube, May 8, 2023
#
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
rm(list=ls())
start_time <- Sys.time()

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# LOAD DATA
#---------+---------+---------+---------+---------+---------+
load(paste0(working_data_dir,"/delinquency_status.RData"))
dt = data.table(delinquency_status)
setorder(dt,ACCOUNT_NO,-DUE_DT)


#---------+---------+---------+---------+---------+---------+
# CREATE VARs
#---------+---------+---------+---------+---------+---------+
dt[,Btminus2:=c(NA,PREV_BILL_AMT[-.N]),by=ACCOUNT_NO]
id = dt$ACCOUNT_NO
Dt = dt$PREV_BILL_AMT
Etminus1 = dt$TOTAL_PAYMENTS
Btminus1 = dt$PREV_BILL_AMT
Btminus2 = dt$Btminus2
