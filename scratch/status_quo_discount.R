#=========================================================================#
# status_quo_discount.R
# 
# What happens if they keep the status quo discount program?
#
# July 15, 2025
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
working_data_dir <- paste0(wd, "/data/analysis")
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
load(paste0(working_data_dir, "/servus/analysis/estimation_dataset_all.RData"))
load(paste0(working_data_dir, "/transunion/analysis/estimation_dataset_all.RData"))
estimation_dataset_all <- estimation_dataset_all %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  fill(tu_income, .direction="downup") %>%
  ungroup()

#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
linc_criteria <- data.frame(hh_size=1:8,
                            tier1_income=c(4344, 4964, 5585, 6205, 6826, 7446, 8067, 8687),
                            tier2_income=c(2172, 2482, 2792, 3103, 3414, 3725, 4036, 4347))

linc_criteria <- linc_criteria %>%
  mutate(full_income=tier1_income/0.6,
         discount_10_income=full_income*0.9,
         discount_20_income=full_income*0.8,
         discount_30_income=full_income*0.7,
         discount_40_income=full_income*0.6,
         discount_50_income=full_income*0.5,
         discount_60_income=full_income*0.4,
         discount_70_income=full_income*0.3,
         discount_80_income=full_income*0.2)

# Only select relevant sample
estimation_dataset_all <- estimation_dataset_all %>%
  filter(account_status!="FINAL",
         !is_rebill,
         !first_bill) %>%
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
           !ufh & !fa_eligible ~ "Above Means-Tested Cap"))

# Information (t==-1)
info_treat_data <- estimation_dataset_all %>% filter(t==-1)
info_treat_data <- info_treat_data %>%
  left_join(linc_criteria, by=c("aspire_household_size"="hh_size")) %>%
  mutate(eligible_tier1=aspire_income*1000/12<=tier1_income,
         eligible_tier2=aspire_income*1000/12<=tier2_income,
         actual_tier1=coalesce(linc_tier_type_at_bill=="Tier1", FALSE),
         actual_tier2=coalesce(linc_tier_type_at_bill=="Tier2", FALSE))

info_treat_data <- info_treat_data %>%
  mutate(eligibility_status=case_when(
    aspire_income*1000/12<=discount_80_income ~ 0.7,
    aspire_income*1000/12<=discount_70_income ~ 0.6,
    aspire_income*1000/12<=discount_60_income ~ 0.5,
    aspire_income*1000/12<=discount_50_income ~ 0.4,
    aspire_income*1000/12<=discount_40_income ~ 0.3,
    aspire_income*1000/12<=discount_30_income ~ 0.2,
    aspire_income*1000/12<=discount_20_income ~ 0.1,
    aspire_income*1000/12>discount_10_income & (actual_tier1 | actual_tier2) ~ 0.1,
    .default=0),
    eligibility_status=ifelse(delinquent=="No Unpaid Debt", eligibility_status-0.3, eligibility_status),
    eligibility_status=ifelse(eligibility_status<0, 0, eligibility_status),
    discount_level=B_t*eligibility_status,
    discount_level=ifelse(discount_level<0, 0, discount_level))

info_treat_data <- info_treat_data %>%
  mutate(eligible_tier1=tu_income*1000*1.037/12<=6205,
         eligible_tier2=tu_income*1000*1.037/12<=3103,
         actual_tier1=coalesce(linc_tier_type_at_bill=="Tier1", FALSE),
         actual_tier2=coalesce(linc_tier_type_at_bill=="Tier2", FALSE))

info_treat_data %>% 
  group_by(eligible_tier1) %>% 
  summarise(n_actual_tier1=sum(actual_tier1, na.rm=TRUE),
            n_actual_tier2=sum(actual_tier2, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(share_actual_tier1=n_actual_tier1/n,
         share_actual_tier2=n_actual_tier2/n)

info_treat_data %>%
  group_by(eligible_tier2) %>%
  summarise(n_actual_tier1=sum(actual_tier1, na.rm=TRUE),
            n_actual_tier2=sum(actual_tier2, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(share_actual_tier1=n_actual_tier1/n,
         share_actual_tier2=n_actual_tier2/n)

info_treat_data %>%
  group_by(actual_tier1) %>%
  summarise(n_eligible_tier1=sum(eligible_tier1, na.rm=TRUE),
            n_eligible_tier2=sum(eligible_tier2, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(share_eligible_tier1=n_eligible_tier1/n,
         share_eligible_tier2=n_eligible_tier2/n)

info_treat_data %>%
  group_by(actual_tier2) %>%
  summarise(n_eligible_tier1=sum(eligible_tier1, na.rm=TRUE),
            n_eligible_tier2=sum(eligible_tier2, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(share_eligible_tier1=n_eligible_tier1/n,
         share_eligible_tier2=n_eligible_tier2/n)

info_miss <- info_treat_data %>%
  filter(eligible_tier1,
         !actual_tier1, !actual_tier2)


View(estimation_dataset_all %>%
       mutate(year=year(bill_date),
              quarter=quarter(bill_date)) %>%
       group_by(year, quarter, linc_tier_type_at_bill) %>%
       summarise(fa_spending=sum(-linc_discount, na.rm=TRUE),
                 n=sum(linc_discount<0, na.rm=TRUE)) %>%
       ungroup() %>%
       group_by(year, linc_tier_type_at_bill) %>%
       mutate(total_spending=sum(fa_spending, na.rm=TRUE),
              total_n=mean(n, na.rm=TRUE)) %>%
       ungroup() %>%
       mutate(average_spending=fa_spending/n) %>%
       filter(!is.na(linc_tier_type_at_bill)))

summary(lm(as.formula(paste0("aspire_income ~ ",
                     paste(colnames(info_treat_data) %>% 
                             str_subset("census"), 
                           collapse=" + "),
                     " + delinquent + factor(linc_tier_type_at_bill) + is_honored_citizen")), data=info_treat_data))

