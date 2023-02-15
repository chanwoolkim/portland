# Measures of Delinquency/Cutoff

load(file=paste0(working_data_dir, "/analysis_info.RData"))

bill_info <- bill_info %>%
  mutate(PERIOD_FROM_DT=mdy(PERIOD_FROM_DT),
         PERIOD_TO_DT=mdy(PERIOD_TO_DT),
         DUE_DT=mdy(DUE_DT))

bill_info_filtered <- bill_info %>% 
  filter(!CANCELED_BILL_YN,
         !is.na(PERIOD_FROM_DT), 
         !is.na(PERIOD_TO_DT),
         !is.na(DUE_DT),
         !ERROR_YN,
         AUDIT_OR_LIVE=="L",
         BILL_TP %in% c("REGLR", "MSTMT"),
         SOURCE_CD %in% c("", "QB1", "QB2", "QB3"),
         !CORRECTED_BILL_YN)

# Those who are not on a payment plan
no_plan_bill <- bill_info_filtered %>%
  filter(SOURCE_CD=="") %>%
  mutate(delinquent=PREV_BILL_AMT+TOTAL_PAYMENTS>0,
         delinquent_amount=ifelse(PREV_BILL_AMT+TOTAL_PAYMENTS>0,
                                  PREV_BILL_AMT+TOTAL_PAYMENTS,
                                  0),
         due_year=year(DUE_DT)) %>%
  group_by(ACCOUNT_NO, due_year) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=sum(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  select(ACCOUNT_NO, due_year, n_bill, delinquent, delinquent_amount)

# Those on a payment plan
plan_bill <- bill_info_filtered %>%
  filter(SOURCE_CD!="") %>%
  mutate(delinquent=AR_DUE_BEFORE_BILL>0,
         delinquent_amount=ifelse(AR_DUE_BEFORE_BILL>0,
                                  AR_DUE_BEFORE_BILL,
                                  0),
         due_year=year(DUE_DT)) %>%
  group_by(ACCOUNT_NO, due_year) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=sum(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  select(ACCOUNT_NO, due_year, n_bill, delinquent, delinquent_amount)

delinquency_status <- rbind(no_plan_bill, plan_bill) %>%
  group_by(ACCOUNT_NO, due_year) %>%
  summarise(n_bill=sum(n_bill, na.rm=TRUE),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=sum(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill) %>%
  pivot_wider(id_cols=ACCOUNT_NO, 
              names_from=due_year, 
              values_from=c("n_bill",
                            "delinquent",
                            "delinquency_rate",
                            "delinquent_amount"),
              values_fill=0)


# Cutoff by year ####
cutoff_reconnect <-
  left_join(cutoff_info %>%
              mutate(CUTOFF_DATE=mdy(EFFECTIVE_DT)) %>%
              select(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE),
            reconnect_info %>%
              mutate(RECONNECT_DATE=mdy(EFFECTIVE_DT)) %>%
              select(ACCOUNT_NO, PERSON_NO, LOCATION_NO, RECONNECT_DATE),
            by=c("ACCOUNT_NO", "PERSON_NO", "LOCATION_NO"))

cutoff_reconnect$RECONNECT_DATE[is.na(cutoff_reconnect$RECONNECT_DATE)] <-
  "2099-12-31"

cutoff_reconnect <- cutoff_reconnect %>%
  mutate(ACCOUNT_NO=as.character(ACCOUNT_NO),
         PERSON_NO=as.character(PERSON_NO),
         LOCATION_NO=as.character(LOCATION_NO)) %>%
  rowwise() %>%
  mutate(cutoff_2019=
           between(2019, year(CUTOFF_DATE), year(RECONNECT_DATE)),
         cutoff_2020=
           between(2020, year(CUTOFF_DATE), year(RECONNECT_DATE)),
         cutoff_2021=
           between(2021, year(CUTOFF_DATE), year(RECONNECT_DATE)),
         cutoff_2022=
           between(2022, year(CUTOFF_DATE), year(RECONNECT_DATE))) %>%
  group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO) %>%
  summarise(cutoff_2019=sum(cutoff_2019),
            cutoff_2020=sum(cutoff_2020),
            cutoff_2021=sum(cutoff_2021),
            cutoff_2022=sum(cutoff_2022)) %>%
  ungroup() %>%
  mutate(cutoff_2019=cutoff_2019>0,
         cutoff_2020=cutoff_2020>0,
         cutoff_2021=cutoff_2021>0,
         cutoff_2022=cutoff_2022>0)

# Save data
save(delinquency_status,
     cutoff_reconnect,
     file=paste0(working_data_dir, "/delinquency_info.RData"))
