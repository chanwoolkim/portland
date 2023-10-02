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




# Load data
load(file=paste0(working_data_dir, "/portland_panel.RData"))

# set up estimation data
df = portland_panel
# Preclean ####
# Couple of dollars left on a bill is not officially delinquent
#df <- df %>%mutate()

df$month = month(df$DUE_DT)
df$year = year(df$DUE_DT)
df$time = (df$year-min(df$year))*12+ df$month
df = df[df$time<=67,]

df$total_payments = -df$total_payments
df = df[df$total_payments>=0,]

df1 = pdata.frame(df,index = c("ACCOUNT_NO","time"))

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


