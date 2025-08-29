#=========================================================================#
# descriptive_statistics.R
# 
# Description of Data
#  -Water Bills
#
# June 5, 2025
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
# Full sample of everyone
load(paste0(working_data_dir, "/transunion/analysis/estimation_dataset_all.RData"))


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
descriptive_statistics <- estimation_dataset_all %>%
  mutate(lag_w_t=ifelse(lag_w_t<0, NA, lag_w_t),
         B_t=ifelse(B_t<0, NA, B_t),
         O_t=ifelse(O_t<0, 0, O_t)) %>%
  filter(t==0) %>%
  summarise(
    N_lag_w_t=sum(!is.na(lag_w_t)),
    mean_lag_w_t=mean(lag_w_t, na.rm=TRUE),
    sd_lag_w_t=sd(lag_w_t, na.rm=TRUE),
    min_lag_w_t=min(lag_w_t, na.rm=TRUE),
    p25_lag_w_t=quantile(lag_w_t, 0.25, na.rm=TRUE),
    p75_lag_w_t=quantile(lag_w_t, 0.75, na.rm=TRUE),
    max_lag_w_t=max(lag_w_t, na.rm=TRUE),
    
    N_O_t=sum(!is.na(O_t)),
    mean_O_t=mean(O_t, na.rm=TRUE),
    sd_O_t=sd(O_t, na.rm=TRUE),
    min_O_t=min(O_t, na.rm=TRUE),
    p25_O_t=quantile(O_t, 0.25, na.rm=TRUE),
    p75_O_t=quantile(O_t, 0.75, na.rm=TRUE),
    max_O_t=max(O_t, na.rm=TRUE),
    
    N_B_t=sum(!is.na(B_t)),
    mean_B_t=mean(B_t, na.rm=TRUE),
    sd_B_t=sd(B_t, na.rm=TRUE),
    min_B_t=min(B_t, na.rm=TRUE),
    p25_B_t=quantile(B_t, 0.25, na.rm=TRUE),
    p75_B_t=quantile(B_t, 0.75, na.rm=TRUE),
    max_B_t=max(B_t, na.rm=TRUE),
    
    N_tu_income=sum(!is.na(tu_income)),
    mean_tu_income=mean(tu_income, na.rm=TRUE),
    sd_tu_income=sd(tu_income, na.rm=TRUE),
    min_tu_income=min(tu_income, na.rm=TRUE),
    p25_tu_income=quantile(tu_income, 0.25, na.rm=TRUE),
    p75_tu_income=quantile(tu_income, 0.75, na.rm=TRUE),
    max_tu_income=max(tu_income, na.rm=TRUE),
    
    N_tu_credit_score=sum(!is.na(tu_credit_score)),
    mean_tu_credit_score=mean(tu_credit_score, na.rm=TRUE),
    sd_tu_credit_score=sd(tu_credit_score, na.rm=TRUE),
    min_tu_credit_score=min(tu_credit_score, na.rm=TRUE),
    p25_tu_credit_score=quantile(tu_credit_score, 0.25, na.rm=TRUE),
    p75_tu_credit_score=quantile(tu_credit_score, 0.75, na.rm=TRUE),
    max_tu_credit_score=max(tu_credit_score, na.rm=TRUE)) %>%
  pivot_longer(cols=everything(), 
               names_to=c(".value", "variable"), 
               names_pattern="^([^_]+)_(.+)$")

tab <- TexRow(c("Variable", "N", "Mean", "SD", "Min", "P25", "P75", "Max")) +
  TexMidrule() +
  TexRow("Billed Water Used (ccf)") /
  TexRow(descriptive_statistics[1, 2:8] %>% as.numeric(), dec=0) +
  TexRow("Total Owed (\\$)") /
  TexRow(descriptive_statistics[2, 2:8] %>% as.numeric(), dec=0) +
  TexRow("Water Bill (\\$)") /
  TexRow(descriptive_statistics[3, 2:8] %>% as.numeric(), dec=0) +
  TexRow("Annual Income (\\$)") /
  TexRow(descriptive_statistics[4, 2:8] %>% as.numeric(), dec=0) +
  TexRow("Credit Score") /
  TexRow(descriptive_statistics[5, 2:8] %>% as.numeric(), dec=0)

TexSave(tab, filename=paste0(output_dir, "/tables/descriptive_statistics"),
        positions=c('l', rep('c', 7)))
