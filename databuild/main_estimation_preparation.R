# LOAD DATA
load(file=paste0(working_data_dir, "/portland_transunion.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))


# Renaming ####
portland_estimation_sample <- portland_transunion %>%
  left_join(portland_demographics_tract_wide %>% 
              mutate(tract=as.numeric(tract)),
            by="tract") %>%
  filter(tract!=0,
         !senior_disabilities,
         !is.na(credit_score), 
         !is.na(etie))

x <- c(
  #TU
  "credit_score", "etie", 
  #Census
  "hh_size", "unemployment", "hh_income",
  "food_stamp", "hh_poverty", "hh_nocar",
  "hispanic", "black")

portland_estimation_sample <- portland_estimation_sample %>%
  arrange(tu_id, bill_date) %>%
  select(h=tu_id,
         t=bill_date,
         E=-total_payments,
         O=current_bill,
         B=usage_bill_amount,
         w_lag=water_cons,
         any_of(x)) %>%
  group_by(h) %>%
  mutate(D=O-B,
         w=lead(w_lag)) %>%
  ungroup()

# Save the dataset
save(portland_estimation_sample, x,
     file=paste0(working_data_dir, "/portland_estimation_sample.RData"))
