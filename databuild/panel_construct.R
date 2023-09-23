# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData.gz"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))
load(file=paste0(working_data_dir, "/usage_financial.RData"))
load(file=paste0(working_data_dir, "/delinquency_status.RData"))
load(file=paste0(working_data_dir, "/location_financial.RData"))

# Create final panel dataset ####
# Add in usage and detailed bill info
portland_panel <- delinquency_status %>%
  left_join(usage_info, by=c("ACCOUNT_NO", "BILL_RUN_DT")) %>%
  left_join(financial_info, by=c("ACCOUNT_NO", "BILL_RUN_DT"))

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
  left_join(location_financial, by=c("ACCOUNT_NO", "DUE_DT", "BILL_RUN_DT"="BILL_DT"))

# Get location relation
location_relation <- location_relation %>%
  mutate(ACTUAL_END_DT=ifelse(ACTUAL_END_DT=="", "12/31/2099", ACTUAL_END_DT),
         EFFECTIVE_DT=mdy(EFFECTIVE_DT),
         ACTUAL_END_DT=mdy(ACTUAL_END_DT),
         PERSON_NO=as.numeric(PERSON_NO),
         LOCATION_NO=as.numeric(LOCATION_NO),
         OCCUPANCY=ifelse(OWNER_YN, "Owner", "Tenant"))

portland_panel <- portland_panel %>%
  left_join(location_relation %>%
              select(ACCT_TO_FRC_CONNECT,
                     PERSON_NO,
                     LOCATION_NO,
                     EFFECTIVE_DT,
                     ACTUAL_END_DT,
                     OCCUPANCY),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT",
                 "PERSON_NO"))

# Prioritise location relation
portland_panel_loc_rel <- portland_panel %>%
  filter(!is.na(LOCATION_NO.y)) %>%
  mutate(LOCATION_NO=LOCATION_NO.y) %>%
  filter(!is.na(EFFECTIVE_DT),
         !is.na(ACTUAL_END_DT)) %>%
  filter(between(BILL_RUN_DT, EFFECTIVE_DT, ACTUAL_END_DT))

portland_panel_fin <- portland_panel %>%
  filter(is.na(LOCATION_NO.y)) %>%
  mutate(LOCATION_NO=LOCATION_NO.x)

portland_panel <- rbind(portland_panel_loc_rel, portland_panel_fin) %>%
  select(-LOCATION_NO.x, -LOCATION_NO.y) %>%
  group_by(ACCOUNT_NO, PERSON_NO) %>%
  fill(LOCATION_NO, .direction="downup") %>%
  filter(!is.na(LOCATION_NO)) %>%
  ungroup() %>%
  distinct(ACCOUNT_NO, PERSON_NO, DUE_DT, BILL_RUN_DT, .keep_all=TRUE)

portland_panel <- portland_panel %>%
  left_join(geocode_address_info_subset %>%
              mutate(LOCATION_NO=as.numeric(LOCATION_NO)) %>%
              select(LOCATION_NO, census_tract),
            by="LOCATION_NO") %>%
  select(-EFFECTIVE_DT, -ACTUAL_END_DT)

# Get financial assistance info
portland_panel <- portland_panel %>%
  left_join(financial_assist_detail %>%
              select(LOCATION_NO, BILL_DT,
                     NET_BILL_AMT, BILLED_AMT_BEFORE_DIS, LINC_DISCOUNT_AMT,
                     CRISIS_VOUCHER_AMT),
            by=c("LOCATION_NO", "BILL_RUN_DT"="BILL_DT"))

# Add in writeoff info
account_writeoff <- account_info %>%
  filter(ACCOUNT_STAT=="WRTOF") %>%
  select(ACCOUNT_NO, LAST_BILL_DT, WRITEOFF_AMT=LAST_BILL_AMT) %>%
  mutate(LAST_BILL_DT=mdy(LAST_BILL_DT),
         WRITEOFF_AMT=as.numeric(WRITEOFF_AMT))

portland_panel <- portland_panel %>%
  left_join(account_writeoff,
            by=c("ACCOUNT_NO", "BILL_RUN_DT"="LAST_BILL_DT"))

# Add in collection info
collection_amount <- collection_amount %>%
  mutate(SS_ACCOUNT_NO=as.character(SS_ACCOUNT_NO),
         CREATE_DT=mdy(CREATE_DT)) %>%
  arrange(SS_ACCOUNT_NO, CREATE_DT) %>%
  group_by(SS_ACCOUNT_NO) %>%
  mutate(rnum=row_number()) %>%
  filter(rnum==max(rnum)) %>%
  ungroup()

portland_final <- portland_panel %>%
  filter(BILL_TP=="FINAL")

portland_non_final <- portland_panel %>%
  filter(BILL_TP!="FINAL") %>%
  mutate(AMT_DUE=NA,
         ACT_COL_AMT=NA)

portland_final <- portland_final %>%
  left_join(collection_amount %>%
              select(SS_ACCOUNT_NO, AMT_DUE, ACT_COL_AMT),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO"))

portland_panel <- rbind(portland_final, portland_non_final)

# Aggregate for monthly payments
portland_panel_sub <- portland_panel %>%
  filter(SOURCE_CD %in% c("QB1", "QB2", "QB3"))

portland_panel_na <- portland_panel %>%
  filter(is.na(usage_bill_amount), SOURCE_CD=="")

portland_panel <- portland_panel %>%
  filter(SOURCE_CD=="",
         !is.na(usage_bill_amount)) %>%
  mutate(across(usage_bill_amount:bill_leaf, ~ replace_na(.x, 0)))

# First sequence must start with QB1, assign group numbers
portland_panel_sub <- portland_panel_sub %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT) %>%
  mutate(source_num=substr(SOURCE_CD, 3, 3) %>% as.numeric(),
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
  arrange(source_num, ACCOUNT_NO, BILL_RUN_DT) %>%
  group_by(ACCOUNT_NO) %>%
  mutate(group_num=row_number()) %>%
  ungroup() %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT) %>%
  mutate(group_num=ifelse(source_num!=1, NA, group_num)) %>%
  group_by(ACCOUNT_NO) %>%
  fill(group_num) %>%
  ungroup()

portland_panel_sub <- portland_panel_sub %>%
  mutate(AR_DUE_BEFORE_BILL=replace_na(AR_DUE_BEFORE_BILL, 0),
         across(usage_bill_amount:bill_leaf, ~ replace_na(.x, 0))) %>%
  select(-source_num, -source_lag, -source_lag_lag, -to_keep, -group_num)

portland_panel <- rbind(portland_panel_sub, portland_panel, portland_panel_na) %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT) %>%
  rename(tract=census_tract,
         previous_bill=PREV_BILL_AMT,
         total_payments=TOTAL_PAYMENTS,
         leftover_debt=AR_DUE_BEFORE_BILL,
         current_bill=AR_DUE_AFTER_BILL,
         net_after_assistance=NET_BILL_AMT,
         bill_before_assistance=BILLED_AMT_BEFORE_DIS,
         discount_assistance=LINC_DISCOUNT_AMT,
         crisis_voucher_amount=CRISIS_VOUCHER_AMT,
         writeoff_amount=WRITEOFF_AMT,
         collection_sent_amount=AMT_DUE,
         collection_collected_amount=ACT_COL_AMT) %>%
  select(-account) %>%
  unique()

# Add in final bill as debt, 2018-12-31 as 2019-01-01
portland_panel <- portland_panel %>%
  mutate(final_writeoff=ifelse(BILL_TP=="FINAL",
                               current_bill-leftover_debt,
                               0),
         BILL_RUN_DT=if_else(BILL_RUN_DT==mdy("12/31/2018"),
                             mdy("1/1/2019"),
                             BILL_RUN_DT))

# Save the dataset
save(portland_panel,
     file=paste0(working_data_dir, "/portland_panel.RData"))
