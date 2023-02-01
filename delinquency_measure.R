# Measure Delinquency

load(file=paste0(working_data_dir, "/analysis_info.RData"))

bill_info <- bill_info %>% 
  #filter(BILL_RUN_DT!="") %>%
  #select(-BILL_RUN_TM) %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT)

bill_info_filtered <- bill_info %>% 
  filter(CANCELED_BILL_YN!="Y",
         PERIOD_FROM_DT!="", 
         PERIOD_TO_DT!="",
         #PREV_BILL_AMT!=0,
         ERROR_YN==FALSE,
         AUDIT_OR_LIVE=="L",
         BILL_TP!="REVRS",
         CORRECTED_BILL_YN==FALSE)

debt_accumulation_bills <- bill_info_filtered %>%
  #Identify the correct criteria to consider it a "Non Paid" bill
  filter(PREV_BILL_AMT+TOTAL_PAYMENTS>0) %>% 
  select(
    ACCOUNT_NO, BILL_RUN_DT, BILL_DT, BILL_TP, PERSON_NO, PERIOD_FROM_DT, PERIOD_TO_DT, 
    BILLING_DAYS, PREV_BILL_AMT, DUE_DT, DISCOUNT_AMT_DUE, DISCOUNT_DUE_DT, 
    DISCOUNT_PCT, TOTAL_PAYMENTS, BILL_GENERATED_CHGS, AR_NET_AFTER_BILL) %>% 
  arrange(ACCOUNT_NO)

debt_accumulation_accounts <- bill_info_filtered %>%
  filter(ACCOUNT_NO %in% debt_accumulation_bills$ACCOUNT_NO) %>% 
  group_by(ACCOUNT_NO) %>% 
  summarise(bills_accumulated=n(),
            accumulated_debt=sum(PREV_BILL_AMT)+sum(TOTAL_PAYMENTS),
            AR_NET_AFTER_BILL=max(AR_NET_AFTER_BILL),
            check=max(PREV_BILL_AMT)+max(BILL_GENERATED_CHGS),
            date_check=max(PERIOD_TO_DT)) 

date_check <- data.table(dc=unique(debt_accumulation_accounts$date_check)) %>% 
  mutate(dcf=mdy(dc))

debt_accumulation_accounts <- debt_accumulation_accounts %>% 
  left_join(date_check, by=c("date_check"="dc"))

location_relation$ACCT_TO_FRC_CONNECT <- as.double(location_relation$ACCT_TO_FRC_CONNECT)

debt_accumulation_accounts <- debt_accumulation_accounts %>% 
  left_join(location_relation %>%
              select(ACCT_TO_FRC_CONNECT, LOCATION_NO),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT"))

debt_accumulation_accounts <- debt_accumulation_accounts %>% 
  left_join(unique(geocode_address_info_subset))

debt_accumulation_accounts_valid_location <- 
  data.table(debt_accumulation_accounts)[!is.na(LOCATION_NO)]

debt_accumulation_accounts_valid_location <- 
  debt_accumulation_accounts_valid_location %>% 
  mutate(had_shutoff=(ACCOUNT_NO %in% cutoff_info$ACCOUNT_NO))

no_debt_bills <- 
  data.table(bill_info_filtered)[!(ACCOUNT_NO %in% unique(debt_accumulation_accounts$ACCOUNT_NO))]

no_debt_accounts <- no_debt_bills %>% 
  group_by(ACCOUNT_NO) %>% 
  summarise(bills_accumulated=0,
            accumulated_debt=sum(PREV_BILL_AMT)+sum(TOTAL_PAYMENTS),
            AR_NET_AFTER_BILL=max(AR_NET_AFTER_BILL),
            check=max(PREV_BILL_AMT)+max(BILL_GENERATED_CHGS),
            date_check=max(PERIOD_TO_DT)) 

date_check2 <- data.table(date_check=unique(no_debt_accounts$date_check)) %>% 
  mutate(dcf=mdy(date_check))

no_debt_accounts <- left_join(no_debt_accounts, date_check2)

no_debt_accounts <- no_debt_accounts %>% 
  left_join(location_relation %>%
              select(ACCT_TO_FRC_CONNECT, LOCATION_NO),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT"))

no_debt_accounts <- no_debt_accounts %>% 
  left_join(unique(geocode_address_info_subset))

no_debt_accounts_valid_location <- 
  data.table(no_debt_accounts)[!is.na(LOCATION_NO)]

no_debt_accounts_valid_location <- 
  no_debt_accounts_valid_location %>% 
  mutate(had_shutoff=(ACCOUNT_NO %in% cutoff_info$ACCOUNT_NO))

debt_dataset <- 
  rbind(debt_accumulation_accounts_valid_location, 
        no_debt_accounts_valid_location) %>% 
  filter(match_indicator=="Match") %>% 
  mutate(payment_plan=ACCOUNT_NO %in% financial_assist$ACCOUNT_NO)

save(debt_dataset, file=paste0(working_data_dir, "/debt_dataset.RData"))
