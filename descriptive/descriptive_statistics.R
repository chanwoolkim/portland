#=========================================================================#
# descriptive_statistics.R
# 
# Description of Data
#  -Water Bills
#
# June 5, 2025
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
if (Sys.info()[4]=="JDUBE-LT3"){
  wd="C:/Users/jdube/Box/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(wd, "/../../Dropbox/Apps/Overleaf/Water Pricing/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
# Full sample of everyone
load(paste0(working_data_dir, "/estimation_dataset_all.RData"))
account_count <- read_csv(paste0(working_data_dir, "/servus_query/2024q4_financial/account_count.csv"))
shutoff_count <- read_csv(paste0(working_data_dir, "/servus_query/2024q4_financial/account_count_shutoff.csv"))
pso_count <- read_csv(paste0(working_data_dir, "/servus_query/2024q4_financial/account_count_pso.csv"))


#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
# Only choose regular bills (not final, rebill, or first)
estimation_dataset_all <- estimation_dataset_all %>%
  filter(!is_rebill,
         !first_bill,
         account_status!="FINAL") %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  fill(tu_income, .direction="downup") %>%
  fill(tu_credit_score, .direction="downup") %>%
  ungroup() %>%
  mutate(tu_income=ifelse(year(bill_date)>=2024, tu_income*1.037*4, tu_income*4),
         tu_credit_score=ifelse(tu_credit_score<300, NA, tu_credit_score))


#---------+---------+---------+---------+---------+---------+
# Descriptive Stats
#---------+---------+---------+---------+---------+---------+
total_unique_account_count <- estimation_dataset_all %>%
  distinct(id) %>%
  nrow()

export_tex(prettyNum(total_unique_account_count, 
                     big.mark=",", scientific=FALSE), 
           "total_unique_account_count")

total_average_account_count <- estimation_dataset_all %>%
  group_by(t) %>%
  summarise(n=n_distinct(id)) %>%
  ungroup() %>%
  filter(t>=-20, t<1) %>%
  summarise(n=mean(n, na.rm=TRUE)) %>%
  pull(n)

export_tex(prettyNum(round(total_average_account_count, 0),
                     big.mark=",", scientific=FALSE), 
           "total_average_account_count")

# All Water Bills in 2024 Q4
#For lag_w_t,O_t,B_t,tu_income,tu_credit_score, get N, mean, SD, min, 25th p, 75th p, max
descriptive_generate <- function(df, file) {
  descriptive_statistics <- df %>%
    mutate(lag_w_t=ifelse(lag_w_t<0 | is.na(lag_w_t), 0, lag_w_t),
           B_t=ifelse(B_t<0, NA, B_t),
           O_t=ifelse(O_t<0, 0, O_t)) %>%
    filter(t==0) %>%
    summarise(
      N_lag_w_t=sum(!is.na(lag_w_t)),
      mean_lag_w_t=mean(lag_w_t, na.rm=TRUE),
      median_lag_w_t=median(lag_w_t, na.rm=TRUE),
      sd_lag_w_t=sd(lag_w_t, na.rm=TRUE),
      min_lag_w_t=min(lag_w_t, na.rm=TRUE),
      max_lag_w_t=max(lag_w_t, na.rm=TRUE),
      
      N_O_t=sum(!is.na(O_t)),
      mean_O_t=mean(O_t, na.rm=TRUE),
      median_O_t=median(O_t, na.rm=TRUE),
      sd_O_t=sd(O_t, na.rm=TRUE),
      min_O_t=min(O_t, na.rm=TRUE),
      max_O_t=max(O_t, na.rm=TRUE),
      
      N_B_t=sum(!is.na(B_t)),
      mean_B_t=mean(B_t, na.rm=TRUE),
      median_B_t=median(B_t, na.rm=TRUE),
      sd_B_t=sd(B_t, na.rm=TRUE),
      min_B_t=min(B_t, na.rm=TRUE),
      max_B_t=max(B_t, na.rm=TRUE),
      
      N_delinquent_t=sum(!is.na(O_t+E_t>0)),
      mean_delinquent_t=mean(O_t+E_t>0, na.rm=TRUE),
      median_delinquent_t=median(O_t+E_t>0, na.rm=TRUE),
      sd_delinquent_t=sd(O_t+E_t>0, na.rm=TRUE),
      min_delinquent_t=min(O_t+E_t>0, na.rm=TRUE),
      max_delinquent_t=max(O_t+E_t>0, na.rm=TRUE),
      
      N_tu_income=sum(!is.na(tu_income)),
      mean_tu_income=mean(tu_income, na.rm=TRUE),
      median_tu_income=median(tu_income, na.rm=TRUE),
      sd_tu_income=sd(tu_income, na.rm=TRUE),
      min_tu_income=min(tu_income, na.rm=TRUE),
      max_tu_income=max(tu_income, na.rm=TRUE),
      
      N_tu_credit_score=sum(!is.na(tu_credit_score)),
      mean_tu_credit_score=mean(tu_credit_score, na.rm=TRUE),
      median_tu_credit_score=median(tu_credit_score, na.rm=TRUE),
      sd_tu_credit_score=sd(tu_credit_score, na.rm=TRUE),
      min_tu_credit_score=min(tu_credit_score, na.rm=TRUE),
      max_tu_credit_score=max(tu_credit_score, na.rm=TRUE)) %>%
    pivot_longer(cols=everything(), 
                 names_to=c(".value", "variable"), 
                 names_pattern="^([^_]+)_(.+)$")
  
  tab <- TexRow(c("Variable", "N", "Mean", "Median", "SD", "Min", "Max")) +
    TexMidrule() +
    TexRow("Billed Water Used (ccf)") /
    TexRow(descriptive_statistics[1, 2:7] %>% as.numeric(), dec=0) +
    TexRow("Total Owed (\\$)") /
    TexRow(descriptive_statistics[2, 2:7] %>% as.numeric(), dec=0) +
    TexRow("Water Bill (\\$)") /
    TexRow(descriptive_statistics[3, 2:7] %>% as.numeric(), dec=0) +
    TexRow("Delinquency (\\%)") /
    TexRow(c(descriptive_statistics[4, 2] %>% as.numeric(),
             descriptive_statistics[4, 3:7] %>% as.numeric()*100), dec=0) +
    TexRow("Annual Income (\\$)") /
    TexRow(descriptive_statistics[5, 2:7] %>% as.numeric(), dec=0) +
    TexRow("Credit Score") /
    TexRow(descriptive_statistics[6, 2:7] %>% as.numeric(), dec=0)
  
  TexSave(tab, filename=paste0(output_dir, "/tables/descriptive_statistics_", file),
          positions=c('l', rep('c', 7)))
  
  return(descriptive_statistics)
}

descriptive_statistics_all <-
  descriptive_generate(estimation_dataset_all, "all")


# SD in water usage within household
sd_w_t <- estimation_dataset_all %>%
  filter(lag_w_t>=0) %>%
  group_by(id, year(bill_date)) %>%
  mutate(sd_w_t=sd(w_t, na.rm=TRUE)) %>%
  ungroup() %>%
  summarise(mean_w_t=median(sd_w_t, na.rm=TRUE)) %>%
  pull(mean_w_t)

export_tex(paste0(round(sd_w_t, 2), " ccf"), 
           "sd_w_t")

# Low income usage
low_income_usage <- estimation_dataset_all %>%
  mutate(lag_w_t=ifelse(lag_w_t<0 | is.na(lag_w_t), 0, lag_w_t),
         ufh=ifelse(is.na(ufh), TRUE, ufh),
         below_median_income=ifelse(is.na(below_median_income), TRUE, below_median_income)) %>%
  filter(t==0, lag_w_t>=0) %>%
  group_by(ufh, below_median_income) %>%
  summarise(mean_w_t=mean(lag_w_t, na.rm=TRUE)) %>%
  ungroup()

export_tex(paste0(round(low_income_usage$mean_w_t[1]-
                          low_income_usage$mean_w_t[3], 2), " ccf"), 
           "low_income_usage_diff")

# Shut-off rate in 2024Q4
shutoff_rate <- shutoff_count %>%
  mutate(n_shutoff=n) %>%
  select(-n) %>%
  left_join(pso_count %>%
              mutate(n_pso=n) %>%
              select(-n),
            by=c("ufh_status", "fa_status", "median_status", "credit_quartile")) %>%
  left_join(account_count, 
            by=c("ufh_status", "fa_status", "median_status", "credit_quartile"))

shutoff_rate_median <- shutoff_rate %>%
  group_by(median_status) %>%
  summarise(n_shutoff=sum(n_shutoff, na.rm=TRUE),
            n_pso=sum(n_pso, na.rm=TRUE),
            n_account=sum(n, na.rm=TRUE),
            shutoff_pso=n_shutoff/n_pso*100,
            shutoff_rate=n_shutoff/n_account*100) %>%
  ungroup() %>%
  filter(!median_status)

shutoff_rate <- shutoff_rate %>%
  group_by(ufh_status, fa_status) %>%
  summarise(n_shutoff=sum(n_shutoff, na.rm=TRUE),
            n_pso=sum(n_pso, na.rm=TRUE),
            n_account=sum(n, na.rm=TRUE),
            shutoff_pso=n_shutoff/n_pso*100,
            shutoff_rate=n_shutoff/n_account*100) %>%
  ungroup() %>%
  filter(!is.na(ufh_status), !is.na(fa_status))

shutoff_rate_total <- shutoff_rate %>%
  summarise(n_shutoff=sum(n_shutoff, na.rm=TRUE),
            n_account=sum(n_account, na.rm=TRUE),
            shutoff_rate=n_shutoff/n_account*100)

tab <- TexRow(c("Income Group", "N Shut-Offs", "N Accounts", "Shutoff Rate (\\%)")) +
  TexMidrule() +
  TexRow("UFH and Below") /
  TexRow(shutoff_rate[3, 3:5] %>% as.numeric(), dec=c(rep(0, 2), 2)) +
  TexRow("UFH to Means-Tested Cap") /
  TexRow(shutoff_rate[2, 3:5] %>% as.numeric(), dec=c(rep(0, 2), 2)) +
  TexRow("Above Means-Tested Cap") /
  TexRow(shutoff_rate[1, 3:5] %>% as.numeric(), dec=c(rep(0, 2), 2)) +
  TexMidrule(list(c(1, 1), c(2, 4))) +
  TexRow("(Above Median Income)") /
  TexRow(shutoff_rate_median[1, 2:4] %>% as.numeric(), dec=c(rep(0, 2), 2)) +
  TexMidrule() +
  TexRow("Total") /
  TexRow(shutoff_rate_total[1, 1:3] %>% as.numeric(), dec=c(rep(0, 2), 2))

TexSave(tab, filename=paste0(output_dir, "/tables/shutoff_rate_2024q4"),
        positions=c('l', rep('c', 3)))
