# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))
load(file=paste0(working_data_dir, "/usage_financial.RData"))
load(file=paste0(working_data_dir, "/delinquency_status.RData"))

# Create final panel dataset ####
# Add in usage and detailed bill info
portland_panel <- delinquency_status %>%
  left_join(usage_info, by=c("ACCOUNT_NO", "BILL_RUN_DT")) %>%
  left_join(financial_info, by=c("ACCOUNT_NO", "BILL_RUN_DT"))

# Get location relation
location_relation <- location_relation %>%
  mutate(ACTUAL_END_DT=ifelse(ACTUAL_END_DT=="", "12/31/2099", ACTUAL_END_DT),
         EFFECTIVE_DT=mdy(EFFECTIVE_DT),
         ACTUAL_END_DT=mdy(ACTUAL_END_DT),
         PERSON_NO=as.numeric(PERSON_NO),
         LOCATION_NO=as.numeric(LOCATION_NO))

portland_panel <- portland_panel %>%
  left_join(location_relation %>%
              select(ACCT_TO_FRC_CONNECT,
                     PERSON_NO,
                     LOCATION_NO,
                     EFFECTIVE_DT,
                     ACTUAL_END_DT),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT",
                 "PERSON_NO")) %>%
  filter(!is.na(EFFECTIVE_DT),
         !is.na(ACTUAL_END_DT)) %>%
  filter(between(BILL_RUN_DT, EFFECTIVE_DT, ACTUAL_END_DT))

portland_panel <- portland_panel %>%
  left_join(geocode_address_info_subset %>%
              mutate(LOCATION_NO=as.numeric(LOCATION_NO)) %>%
              select(LOCATION_NO, census_tract),
            by="LOCATION_NO") %>%
  select(-EFFECTIVE_DT, -ACTUAL_END_DT)

# Get financial assistance info
financial_assist_detail <- financial_assist_detail %>%
  mutate(ACCOUNT_NO=as.character(ACCOUNT_NO),
         BILL_RUN_DT=mdy(BILL_DT))

portland_panel <- portland_panel %>%
  left_join(financial_assist_detail %>%
              select(ACCOUNT_NO, LOCATION_NO, BILL_RUN_DT,
                     NET_BILL_AMT, BILLED_AMT_BEFORE_DIS, LINC_DISCOUNT_AMT,
                     CRISIS_VOUCHER_AMT),
            by=c("ACCOUNT_NO", "LOCATION_NO", "BILL_RUN_DT"))

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
  group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO, census_tract, group_num) %>%
  summarise(DUE_DT=max(DUE_DT),
            PREV_BILL_AMT=first(PREV_BILL_AMT),
            TOTAL_PAYMENTS=sum(TOTAL_PAYMENTS, na.rm=TRUE),
            PERIOD_FROM_DT=min(PERIOD_FROM_DT),
            PERIOD_TO_DT=max(PERIOD_TO_DT),
            BILL_RUN_DT=min(BILL_RUN_DT),
            across(account:payment_arrange, ~ sum(.x, na.rm=TRUE)),
            payment_arrange_status=sum(payment_arrange_status, na.rm=FALSE),
            across(financial_assist:CRISIS_VOUCHER_AMT, ~ sum(.x, na.rm=TRUE))) %>%
  ungroup() %>%
  transmute(ACCOUNT_NO, PERSON_NO, LOCATION_NO, DUE_DT, census_tract,
            bill_year=year(BILL_RUN_DT),
            delinquent=PREV_BILL_AMT+TOTAL_PAYMENTS>0,
            delinquent_amount=PREV_BILL_AMT+TOTAL_PAYMENTS,
            PREV_BILL_AMT, TOTAL_PAYMENTS,
            AR_DUE_BEFORE_BILL=PREV_BILL_AMT+TOTAL_PAYMENTS,
            AR_DUE_AFTER_BILL=AR_DUE_BEFORE_BILL+usage_bill_amount,
            NET_BILL_AMT, BILLED_AMT_BEFORE_DIS, LINC_DISCOUNT_AMT, CRISIS_VOUCHER_AMT,
            SOURCE_CD="",
            PERIOD_FROM_DT, PERIOD_TO_DT, BILL_RUN_DT,
            account=account>0,
            payment_arrange=payment_arrange>0,
            payment_arrange_status=case_when(payment_arrange_status>0 ~ TRUE,
                                             payment_arrange_status==0 ~ FALSE),
            financial_assist=financial_assist>0,
            cutoff=cutoff>0,
            usage_bill_amount, usage_bill_water_cons, usage_bill_sewer_cons,
            water_cons, sewer_cons, bill_sewer_cons, bill_water_cons,
            bill_payment, bill_penalty, bill_donate, bill_bankrupt, bill_leaf)

portland_panel_sub <- portland_panel_sub %>%
  mutate(AR_DUE_AFTER_BILL=ifelse(NET_BILL_AMT!=0,
                                  AR_DUE_BEFORE_BILL+NET_BILL_AMT,
                                  AR_DUE_AFTER_BILL),
         delinquent_amount=ifelse(delinquent_amount<0, 0, delinquent_amount),
         BILLED_AMT_BEFORE_DIS=ifelse(NET_BILL_AMT==0, NA, BILLED_AMT_BEFORE_DIS),
         LINC_DISCOUNT_AMT=ifelse(NET_BILL_AMT==0, NA, LINC_DISCOUNT_AMT),
         NET_BILL_AMT=ifelse(NET_BILL_AMT==0, NA, NET_BILL_AMT))

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
         crisis_voucher_amount=CRISIS_VOUCHER_AMT) %>%
  select(-SOURCE_CD, -account)

# Save the dataset
save(portland_panel,
     file=paste0(working_data_dir, "/portland_panel.RData"))
