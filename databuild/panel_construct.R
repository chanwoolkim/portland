# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))
load(file=paste0(working_data_dir, "/usage_financial.RData"))
load(file=paste0(working_data_dir, "/financial_info_leftover.RData"))
load(file=paste0(working_data_dir, "/delinquency_status.RData"))


# Create final panel dataset ####
# Add in usage and detailed bill info
portland_panel <- delinquency_status %>%
  select(-delinquent, -delinquent_amount) %>%
  left_join(usage_info, by=c("tu_id", "bill_date"="bill_run_date")) %>%
  left_join(financial_info, by=c("tu_id", "bill_date")) %>%
  select(-bill_water_cons, -bill_sewer_cons, -bill_payment)

# Define fixed and variable prices
portland_panel <- portland_panel %>%
  mutate(price_water=water_var_price,
         price_sewer=rowSums(select(., c("sewer_var_price",       
                                         "sewer_phs_var_price",
                                         "bod_var_price",
                                         "tss_var_price",
                                         "cleanriver_var_price",
                                         "clnrvrcrd_var_price")),
                             na.rm=TRUE),
         price_fixed=rowSums(select(., contains("_fixed")), na.rm=TRUE),
         price_donation=swr_donation_price,
         price_discount=rowSums(select(., starts_with("linc_")), na.rm=TRUE)) %>%
  select(!ends_with("_price")) %>%
  mutate_at(vars(starts_with("price_")),
            ~ ifelse(.==0 | is.nan(.), NA, .))

# Merge in location from financial info
portland_panel <- portland_panel %>%
  left_join(location_account_relation %>%
              select(tu_id, tract_id) %>%
              distinct(),
            by="tu_id") %>%
  rename(census_tract=tract_id)

# Drop those with duplicate bills (issues with TU_ID assignment)
portland_panel <- portland_panel %>%
  distinct(tu_id, due_date, bill_date, .keep_all=TRUE)

# Get financial assistance info
financial_assist_account <- financial_assist_detail %>%
  filter(!is.na(tu_id)) %>%
  select(tu_id, bill_date) %>%
  mutate(match=TRUE)

portland_panel_account <- portland_panel %>%
  left_join(financial_assist_account,
            by=c("tu_id", "bill_date")) %>%
  filter(match) %>%
  left_join(financial_assist_detail %>%
              filter(!is.na(tu_id)) %>%
              distinct(tu_id, bill_date, .keep_all=TRUE) %>%
              select(tu_id, bill_date, linc_tier_type,
                     net_bill_amount, billed_amount_before_discount, linc_discount_amount,
                     crisis_voucher_amount, senior_disabilities),
            by=c("tu_id", "bill_date"))

portland_panel_location <- portland_panel %>%
  left_join(financial_assist_account,
            by=c("tu_id", "bill_date")) %>%
  filter(is.na(match)) %>%
  left_join(financial_assist_detail %>%
              filter(is.na(tu_id)) %>%
              select(tu_id, bill_date, linc_tier_type,
                     net_bill_amount, billed_amount_before_discount, linc_discount_amount,
                     crisis_voucher_amount, senior_disabilities),
            by=c("tu_id", "bill_date"))

portland_panel <- bind_rows(portland_panel_account, portland_panel_location) %>%
  select(-match)

# Add in writeoff info
account_writeoff <- account_info %>%
  filter(status_code=="WRTOF") %>%
  select(tu_id, last_bill_date, writeoff_amount=last_bill_amount) %>%
  mutate(last_bill_date=mdy(last_bill_date))

portland_panel <- portland_panel %>%
  left_join(account_writeoff,
            by=c("tu_id", "bill_date"="last_bill_date"))

# Add in collection info
collection_amount <- collection_amount %>%
  mutate(sent_date=mdy(sent_date)) %>%
  arrange(tu_id, sent_date) %>%
  group_by(tu_id) %>%
  mutate(rnum=row_number()) %>%
  filter(rnum==max(rnum)) %>%
  ungroup()

portland_final <- portland_panel %>%
  filter(type_code=="FINAL")

portland_non_final <- portland_panel %>%
  filter(type_code!="FINAL") %>%
  mutate(amount_due=NA,
         collection_amount=NA)

portland_final <- portland_final %>%
  left_join(collection_amount %>%
              select(tu_id, amount_due, collection_amount),
            by=c("tu_id"))

portland_panel <- bind_rows(portland_final, portland_non_final)

# Aggregate for monthly payments
portland_panel_sub <- portland_panel %>%
  filter(source_code %in% c("QB1", "QB2", "QB3"))

portland_panel_na <- portland_panel %>%
  filter(is.na(usage_bill_amount), is.na(source_code))

portland_panel <- portland_panel %>%
  filter(is.na(source_code),
         !is.na(usage_bill_amount)) %>%
  mutate(across(usage_bill_amount:bill_leaf, ~ replace_na(.x, 0)))

# First sequence must start with QB1, assign group numbers
portland_panel_sub <- portland_panel_sub %>%
  arrange(tu_id, bill_date) %>%
  mutate(source_num=substr(source_code, 3, 3) %>% as.numeric(),
         source_lag=lag(source_num),
         source_lag_lag=lag(source_lag),
         to_keep=case_when(
           source_num==1 ~ TRUE,
           source_num==2 & source_lag==1 ~ TRUE,
           source_num==2 & source_lag!=1 ~ FALSE,
           source_num==3 & source_lag==2 & source_lag_lag==1 ~ TRUE,
           source_num==3 & source_lag==2 & source_lag_lag!=1 ~ FALSE,
           source_num==3 & source_lag!=2 ~ FALSE)) %>%
  filter(to_keep)

portland_panel_sub <- portland_panel_sub %>%
  arrange(source_num, tu_id, bill_date) %>%
  group_by(tu_id) %>%
  mutate(group_num=row_number()) %>%
  ungroup() %>%
  arrange(tu_id, bill_date) %>%
  mutate(group_num=ifelse(source_num!=1, NA, group_num)) %>%
  group_by(tu_id) %>%
  fill(group_num) %>%
  ungroup()

portland_panel_sub <- portland_panel_sub %>%
  mutate(ar_due_before_bill=replace_na(ar_due_before_bill, 0),
         across(usage_bill_amount:bill_leaf, ~ replace_na(.x, 0))) %>%
  select(-source_num, -source_lag, -source_lag_lag, -to_keep, -group_num)

portland_panel <- bind_rows(portland_panel_sub, portland_panel, portland_panel_na) %>%
  arrange(tu_id, bill_date) %>%
  rename(tract=census_tract,
         previous_bill=previous_bill_amount,
         total_payments=total_payments,
         leftover_debt=ar_due_before_bill,
         current_bill=ar_due_after_bill,
         net_after_assistance=net_bill_amount,
         bill_before_assistance=billed_amount_before_discount,
         discount_assistance=linc_discount_amount,
         crisis_voucher_amount=crisis_voucher_amount,
         writeoff_amount=writeoff_amount,
         collection_sent_amount=amount_due,
         collection_collected_amount=collection_amount) %>%
  select(-account) %>%
  distinct()

# Select payments to final bill
portland_panel_fin <- portland_panel %>%
  group_by(tu_id) %>%
  mutate(next_bill_date=lead(bill_date)) %>%
  filter(type_code=="FINAL") %>%
  mutate(next_bill_date=if_else(is.na(next_bill_date),
                                mdy("12/31/2099"),
                                next_bill_date))

financial_info_leftover <- financial_info_leftover %>%
  mutate(transaction_date=mdy(transaction_date),
         bill_date=mdy(bill_date)) %>%
  left_join(portland_panel_fin %>%
              select(tu_id, bill_date, next_bill_date),
            by=c("tu_id", "bill_date")) %>%
  filter(!is.na(bill_date),
         transaction_date>=bill_date,
         transaction_date<next_bill_date) %>%
  group_by(tu_id, bill_date) %>%
  summarise(final_payment=sum(adjusted_amount, na.rm=TRUE))

portland_panel <- portland_panel %>%
  left_join(financial_info_leftover,
            by=c("tu_id", "bill_date"))

# Add in final bill as debt, 2018-12-31 as 2019-01-01
portland_panel <- portland_panel %>%
  mutate(final_payment=ifelse(type_code=="FINAL" & is.na(final_payment),
                              0,
                              final_payment),
         final_writeoff=ifelse(type_code=="FINAL",
                               current_bill+final_payment,
                               0),
         bill_date=if_else(bill_date==mdy("12/31/2018"),
                           mdy("1/1/2019"),
                           bill_date))

# Flag "assumed" resumption (same account)
portland_panel <- portland_panel %>%
  mutate(last_leftover=current_bill+final_payment) %>%
  arrange(tu_id, bill_date) %>%
  group_by(tu_id) %>%
  mutate(lag_leftover=lag(last_leftover)) %>%
  ungroup() %>%
  mutate(type_code=
           case_when(type_code=="RESUME" &
                       previous_bill==lag_leftover ~ "ASSUME_ACCOUNT",
                     .default=type_code)) %>%
  select(-last_leftover, -lag_leftover)

# Save the dataset
save(portland_panel,
     file=paste0(working_data_dir, "/portland_panel.RData"))
