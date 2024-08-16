#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
library(data.table)
library(lfe)
library(plm)

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
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
# Load data
#---------+---------+---------+---------+---------+---------+
load(file=paste0(working_data_dir, "/portland_transunion.RData"))
df = portland_transunion

#---------+---------+---------+---------+---------+---------+
# set up estimation data
# (1) Create Variables for analysis
#---------+---------+---------+---------+---------+---------+
df$month = month(df$due_date)
df$year = year(df$due_date)
df = df[df$year>=2019,]
df$time = (df$year-min(df$year))*12+ df$month
df$total_payments = -df$total_payments


#---------+---------+---------+---------+---------+---------+
# set up estimation data
# (2) Filters
#  - type_code=="FIRST" (no previous bills so first invoice so off-cycle)
#  - type_code=="FINAL" (final bill so off-cycle)
#  - type_code=="RESUME" (like a first bill after account frozen)
#  - agg == "CHOP" (problems with our aggreation period)
#  - cycle_code != "QUARTER" (i.e., monthly and bi-monthly bills dropped)
#---------+---------+---------+---------+---------+---------+

# define filters
keep_types = (df$type_code!="FIRST" & 
                df$type_code!="FINAL" & 
                df$type_code!="RESUME" & 
                df$agg!="CHOP" & 
                df$cycle_code=="QUARTER")
df = df[keep_types==1,]

# retain periods with complete account information
keep_time = df$time<=67
df = df[df$time<=67,]

# drop negative payments
keep_pay = df$total_payments>=0

df = df[df$total_payments>=0,]


df1 = pdata.frame(df,index = c("tu_id","time"))

summary(felm( df$delinquent ~ df$price_water+df$price_sewer + df$price_discount| df$ACCOUNT_NO + df$time))
summary(felm( df$delinquent ~ df$bill_water_cons + df$price_water+df$price_sewer + df$price_discount| df$ACCOUNT_NO+df$time))

summary(felm( (df$total_payments==0) ~ df$price_water+df$price_sewer + df$price_discount| df$ACCOUNT_NO+df$time))


summary(felm( df$total_payments ~ df$price_water+df$price_sewer + df$price_discount| df$ACCOUNT_NO + df$time))
summary(felm( df$total_payments ~ log(df$price_water)+log(df$price_sewer) + log(df$price_discount)| df$ACCOUNT_NO + df$time))


censReg(total_payments~price_water + price_sewer + price_discount, data=df1)

felm(df$price_water | df$time)



acctlist = sort(unique(df$ACCOUNT_NO))
zz = matrix(1,length(acctlist),1)
for(ii in acctlist){
 index = df$ACCOUNT_NO==ii
 cc = table(df$time[index])
 if(max(cc)>1){stop}
}


zz = (as.numeric(df$ACCOUNT_NO)-min(as.numeric(df$ACCOUNT_NO)))*55 + df$time
