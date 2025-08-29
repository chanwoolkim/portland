# LOAD DATA
load(file=paste0(working_data_dir, "/portland_transunion.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))


# Precleaning ####
# Remove seniors and the disabled and those missing Census or TU info
# Also remove those on monthly payment plan whose last bill was chopped
portland_panel_sample <- portland_transunion %>%
  left_join(portland_demographics_tract_wide %>% 
              mutate(tract=as.numeric(tract)),
            by="tract") %>%
  filter(tract!=0,
         !senior_disabilities,
         !is.na(credit_score), 
         !is.na(etie),
         agg!="CHOP",
         type_code=="REGLR")

# Remove those who have a gap in their billing
portland_panel_sample <- portland_panel_sample %>%
  group_by(tu_id) %>%
  mutate(previous_end_date=lag(end_date),
         previous_bill_date=lag(bill_date)) %>%
  ungroup() %>%
  mutate(end_to_start=difftime(start_date, previous_end_date, units="days") %>% as.numeric(),
         end_to_start=ifelse(is.na(end_to_start), 1, end_to_start),
         bill_to_bill=difftime(bill_date, previous_bill_date, units="days") %>% as.numeric(),
         bill_to_bill=ifelse(is.na(bill_to_bill), 90, bill_to_bill)) %>%
  group_by(tu_id) %>%
  filter(!(any(end_to_start>1) | any(bill_to_bill<30))) %>%
  ungroup()

# Remove those whose bill was severely off-cycle (more than 15 days)
portland_panel_sample <- portland_panel_sample %>%
  mutate(bill_days=difftime(end_date, start_date, units="days") %>% as.numeric()) %>%
  group_by(tu_id) %>%
  filter(!(any(bill_days<75) | any(bill_days>105))) %>%
  ungroup()

portland_panel_sample <- portland_panel_sample %>%
  group_by(tu_id) %>%
  mutate(ever_delinquent=lag(cumsum(replace_na(delinquent, 0)), 0)>0,
         ever_payment_arrange=lag(cumsum(replace_na(payment_arrange, 0)), 0)>0,
         ever_financial_assist=lag(cumsum(replace_na(financial_assist, 0)), 0)>0,
         ever_cutoff=lag(cumsum(replace_na(cutoff, 0)), 0)>0) %>%
  ungroup()

x <- c(
  # TU
  "credit_score", "etie", 
  # Census
  "hh_size", "unemployment", "hh_income",
  "food_stamp", "hh_poverty", "hh_nocar",
  "hispanic", "black",
  # History
  "ever_delinquent", "ever_payment_arrange", "ever_financial_assist", "ever_cutoff")

# Rename
portland_panel_sample <- portland_panel_sample %>%
  arrange(tu_id, bill_date) %>%
  select(h=tu_id,
         bill_t=bill_date,
         E_t=total_payments,
         O_t=current_bill,
         B_t=usage_bill_amount,
         w_lag=water_cons,
         any_of(x)) %>%
  group_by(h) %>%
  mutate(bill_t=lead(bill_t),
         E_t=lead(-E_t),
         D_t=O_t-B_t,
         w_t=lead(w_lag)) %>%
  ungroup() %>%
  filter(!is.na(bill_t))

# Manipulate t
portland_panel_sample <- portland_panel_sample %>%
  mutate(t=ifelse(year(bill_t)==2024 & month(bill_t) %in% c(3, 4, 5), -3, NA)) %>%
  group_by(h) %>%
  filter(!all(is.na(t))) %>%
  mutate(lag_t=lag(t)) %>%
  ungroup() %>%
  mutate(t=ifelse(t==-3 & lag_t==-3 & !is.na(lag_t), NA, t)) %>%
  select(-lag_t)

while (!all(!is.na(portland_panel_sample$t))) {
  portland_panel_sample <- portland_panel_sample %>%
    group_by(h) %>%
    mutate(t=ifelse(is.na(t), lead(t)-1, t),
           t=ifelse(is.na(t), lag(t)+1, t)) %>%
    ungroup()
}

portland_panel_sample <- portland_panel_sample %>%
  arrange(h, t) %>%
  select(h, bill_t, t, O_t, E_t, B_t, D_t, w_t, w_lag, any_of(x))

portland_cross_section_sample <- portland_panel_sample %>%
  group_by(h) %>%
  mutate(across(all_of(c("O_t", "E_t", "B_t", "D_t", "w_t", "w_lag")), 
                ~lag(.),
                .names = "prev1_{col}"),
         across(all_of(c("O_t", "E_t", "B_t", "D_t", "w_t", "w_lag")), 
                ~lag(., 2),
                .names = "prev2_{col}"),
         across(all_of(c("O_t", "E_t", "B_t", "D_t", "w_t", "w_lag")), 
                ~lag(., 3),
                .names = "prev3_{col}")) %>%
  ungroup() %>%
  filter(t %in% c(-3, -7, -11)) %>%
  mutate(year=case_when(t==-3 ~ 0,
                        t==-7 ~ -1,
                        t==-11 ~ -2,
                        .default=NA)) %>%
  select(-t) %>%
  group_by(h) %>%
  filter(any(year==0)) %>%
  ungroup()

# Round appropriately so we can work with equals
portland_cross_section_sample <- portland_cross_section_sample %>%
  mutate(across(all_of(c("O_t", "E_t", "B_t", "D_t")), 
                ~round(., 2)),
         across(contains("prev"),
                ~round(., 2)),
         across(all_of(c("h", "year",
                         "w_t", "w_lag", "credit_score", "etie")), 
                ~round(., 0)))

# Save the dataset
save(portland_panel_sample, x,
     file=paste0(working_data_dir, "/portland_panel_sample.RData"))

save(portland_cross_section_sample, x,
     file=paste0(working_data_dir, "/portland_cross_section_sample.RData"))
