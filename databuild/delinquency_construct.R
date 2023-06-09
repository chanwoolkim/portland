# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))

# Only consider single family
account_info_subset <- account_info_merge %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST")) %>%
  select(ACCOUNT_NO) %>%
  mutate(account=TRUE)

# Average payment arrangement amount
bill_info <- bill_info %>%
  mutate(PERIOD_FROM_DT=mdy(PERIOD_FROM_DT),
         PERIOD_TO_DT=mdy(PERIOD_TO_DT),
         DUE_DT=mdy(DUE_DT),
         BILL_RUN_DT=mdy(BILL_RUN_DT))

bill_info_filtered <- bill_info %>% 
  filter(!CANCELED_BILL_YN,
         !is.na(PERIOD_FROM_DT), 
         !is.na(PERIOD_TO_DT),
         !is.na(DUE_DT),
         !is.na(BILL_RUN_DT),
         !ERROR_YN,
         AUDIT_OR_LIVE=="L",
         BILL_TP %in% c("REGLR", "MSTMT"),
         SOURCE_CD %in% c("", "QB1", "QB2", "QB3"),
         !CORRECTED_BILL_YN)

# Those who are not on a payment plan
no_plan_bill <- bill_info_filtered %>%
  filter(SOURCE_CD=="") %>%
  mutate(delinquent=PREV_BILL_AMT+TOTAL_PAYMENTS>0,
         delinquent=ifelse(is.na(delinquent), FALSE, delinquent),
         delinquent_amount=ifelse(PREV_BILL_AMT+TOTAL_PAYMENTS>0,
                                  PREV_BILL_AMT+TOTAL_PAYMENTS,
                                  0),
         delinquent_amount=ifelse(is.na(delinquent_amount),
                                  0,
                                  delinquent_amount),
         bill_year=year(BILL_RUN_DT)) %>%
  select(ACCOUNT_NO, PERSON_NO, DUE_DT, BILL_RUN_DT,
         bill_year, delinquent, delinquent_amount,
         PREV_BILL_AMT, TOTAL_PAYMENTS, AR_DUE_BEFORE_BILL, AR_DUE_AFTER_BILL, SOURCE_CD,
         PERIOD_FROM_DT, PERIOD_TO_DT)

# Those on a payment plan
plan_bill <- bill_info_filtered %>%
  filter(SOURCE_CD!="") %>%
  mutate(delinquent=AR_DUE_BEFORE_BILL>0,
         delinquent=ifelse(is.na(delinquent), FALSE, delinquent),
         delinquent_amount=ifelse(AR_DUE_BEFORE_BILL>0,
                                  AR_DUE_BEFORE_BILL,
                                  0),
         delinquent_amount=ifelse(is.na(delinquent_amount),
                                  0,
                                  delinquent_amount),
         bill_year=year(BILL_RUN_DT)) %>%
  select(ACCOUNT_NO, PERSON_NO, DUE_DT, BILL_RUN_DT,
         bill_year, delinquent, delinquent_amount,
         PREV_BILL_AMT, TOTAL_PAYMENTS, AR_DUE_BEFORE_BILL, AR_DUE_AFTER_BILL, SOURCE_CD,
         PERIOD_FROM_DT, PERIOD_TO_DT)

delinquency_status <- rbind(no_plan_bill, plan_bill) %>%
  mutate(ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
  left_join(account_info_subset,
            by="ACCOUNT_NO") %>%
  filter(account)

# Payment arrangement amount
payment_arrange_amount <- payment_arrangement_info %>%
  mutate(amount_paid=AMOUNT_DUE-OUTSTANDING_AMT) %>%
  group_by(PAY_ARRANGEMENT_REF) %>%
  summarise(amount_due=sum(AMOUNT_DUE, na.rm=TRUE),
            amount_paid=sum(amount_paid, na.rm=TRUE),
            amount_outstanding=sum(OUTSTANDING_AMT, na.rm=TRUE))

payment_arrange_amount <- payment_arrangement %>%
  filter(!grepl("^[0-9]", STATUS_CD)) %>%
  mutate(STATUS_CD=trimws(STATUS_CD),
         STATUS_CD=ifelse(STATUS_CD=="T", "T", "P"),
         payment_arrange_start=mdy(START_DT),
         payment_arrange_end=mdy(END_DT),
         payment_arrange_start_year=year(payment_arrange_start),
         payment_arrange_end_year=year(payment_arrange_end),
         ARRANGEMENT_AMT=as.numeric(ARRANGEMENT_AMT)) %>%
  right_join(payment_arrange_amount, by="PAY_ARRANGEMENT_REF") %>%
  filter(payment_arrange_start_year>=2019 |
           payment_arrange_end_year>=2019)

# Payment arrangement
payment_arrange_time <- payment_arrange_amount %>%
  group_by(SS_ACCOUNT_NO) %>%
  arrange(payment_arrange_start, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(payment_arrange_start)) >
                            cummax(as.numeric(payment_arrange_end)))[-n()])) %>%
  group_by(SS_ACCOUNT_NO, indx) %>%
  summarise(terminated=any(STATUS_CD=="T"),
            payment_arrange_start=min(payment_arrange_start), 
            payment_arrange_end=max(payment_arrange_end)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(payment_arrange_start=ifelse(is.na(payment_arrange_start),
                                      mdy("12/31/2099"),
                                      payment_arrange_start),
         payment_arrange_end=ifelse(is.na(payment_arrange_end),
                                    mdy("12/31/2099"),
                                    payment_arrange_end))

payment_arrange_time_count <- payment_arrange_time %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(count=n())

payment_arrange_time_count_1 <- payment_arrange_time_count %>%
  filter(count==1)

payment_arrange_time_count_above_1 <- payment_arrange_time_count %>%
  filter(count>1)

payment_arrange_time_above_1 <- payment_arrange_time %>%
  filter(SS_ACCOUNT_NO %in% payment_arrange_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_none <- delinquency_status %>%
  filter(!(ACCOUNT_NO %in% c(payment_arrange_time_count_above_1$SS_ACCOUNT_NO,
                             payment_arrange_time_count_1$SS_ACCOUNT_NO))) %>%
  mutate(payment_arrange=FALSE,
         payment_arrange_status=NA)

delinquency_status_sub <- delinquency_status %>%
  filter(ACCOUNT_NO %in% payment_arrange_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_rest <- delinquency_status %>%
  filter(ACCOUNT_NO %in% payment_arrange_time_count_1$SS_ACCOUNT_NO) %>%
  left_join(payment_arrange_time %>%
              filter(SS_ACCOUNT_NO %in% payment_arrange_time_count_1$SS_ACCOUNT_NO),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO")) %>%
  rowwise() %>%
  mutate(payment_arrange=between(BILL_RUN_DT, payment_arrange_start, payment_arrange_end),
         payment_arrange_status=ifelse(payment_arrange, terminated, NA)) %>%
  ungroup() %>%
  mutate(payment_arrange=ifelse(is.na(payment_arrange), FALSE, payment_arrange)) %>%
  select(-payment_arrange_start, -payment_arrange_end, -terminated)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(payment_arrange=any(BILL_RUN_DT %between%
                               list(subset(payment_arrange_time_above_1,
                                           SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_start,
                                    subset(payment_arrange_time_above_1,
                                           SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_end)),
         payment_arrange_status=
           ifelse(payment_arrange,
                  subset(payment_arrange_time_above_1,
                         SS_ACCOUNT_NO==ACCOUNT_NO)$terminated[
                           which(BILL_RUN_DT %between%
                                   list(subset(payment_arrange_time_above_1,
                                               SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_start,
                                        subset(payment_arrange_time_above_1,
                                               SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_end))],
                  NA)) %>%
  ungroup()

delinquency_status <- rbind(delinquency_status_sub,
                            delinquency_status_rest,
                            delinquency_status_none)

# Attach financial info onto payment arrangement
financial_assist_account <- financial_assist %>%
  mutate(financial_assist_start=mdy(EFFECTIVE_DT),
         financial_assist_end=mdy(EXPIRY_DT)) %>%
  filter(year(financial_assist_start)>=2019 |
           year(financial_assist_end)>=2019) %>%
  select(ACCOUNT_NO, financial_assist_start, financial_assist_end)

# Financial assistance
financial_assist_time <- financial_assist_account %>%
  mutate(SS_ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
  group_by(SS_ACCOUNT_NO) %>%
  arrange(financial_assist_start, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(financial_assist_start)) >
                            cummax(as.numeric(financial_assist_end)))[-n()])) %>%
  group_by(SS_ACCOUNT_NO, indx) %>%
  summarise(financial_assist_start=min(financial_assist_start), 
            financial_assist_end=max(financial_assist_end)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(financial_assist_start=ifelse(is.na(financial_assist_start),
                                       mdy("12/31/2099"),
                                       financial_assist_start),
         financial_assist_end=ifelse(is.na(financial_assist_end),
                                     mdy("12/31/2099"),
                                     financial_assist_end))

financial_assist_time_count <- financial_assist_time %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(count=n())

financial_assist_time_count_1 <- financial_assist_time_count %>%
  filter(count==1)

financial_assist_time_count_above_1 <- financial_assist_time_count %>%
  filter(count>1)

financial_assist_time_above_1 <- financial_assist_time %>%
  filter(SS_ACCOUNT_NO %in% financial_assist_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_none <- delinquency_status %>%
  filter(!(ACCOUNT_NO %in% c(financial_assist_time_count_above_1$SS_ACCOUNT_NO,
                             financial_assist_time_count_1$SS_ACCOUNT_NO))) %>%
  mutate(financial_assist=FALSE)

delinquency_status_sub <- delinquency_status %>%
  filter(ACCOUNT_NO %in% financial_assist_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_rest <- delinquency_status %>%
  filter(ACCOUNT_NO %in% financial_assist_time_count_1$SS_ACCOUNT_NO) %>%
  left_join(financial_assist_time %>%
              filter(SS_ACCOUNT_NO %in% financial_assist_time_count_1$SS_ACCOUNT_NO),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO")) %>%
  rowwise() %>%
  mutate(financial_assist=between(BILL_RUN_DT, financial_assist_start, financial_assist_end)) %>%
  ungroup() %>%
  mutate(financial_assist=ifelse(is.na(financial_assist), FALSE, financial_assist)) %>%
  select(-financial_assist_start, -financial_assist_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(financial_assist=any(BILL_RUN_DT %between%
                                list(subset(financial_assist_time_above_1,
                                            SS_ACCOUNT_NO==ACCOUNT_NO)$financial_assist_start,
                                     subset(financial_assist_time_above_1,
                                            SS_ACCOUNT_NO==ACCOUNT_NO)$financial_assist_end))) %>%
  ungroup()

delinquency_status <- rbind(delinquency_status_sub,
                            delinquency_status_rest,
                            delinquency_status_none)

# Cutoff/reconnect
cutoff_reconnect <-
  left_join(cutoff_info %>%
              mutate(CUTOFF_DATE=mdy(EFFECTIVE_DT)) %>%
              arrange(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE) %>%
              group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO) %>%
              mutate(id=row_number()) %>%
              ungroup() %>%
              select(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE, id),
            reconnect_info %>%
              mutate(RECONNECT_DATE=mdy(EFFECTIVE_DT)) %>%
              arrange(ACCOUNT_NO, PERSON_NO, LOCATION_NO, RECONNECT_DATE) %>%
              group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO) %>%
              mutate(id=row_number()) %>%
              ungroup() %>%
              select(ACCOUNT_NO, PERSON_NO, LOCATION_NO, RECONNECT_DATE, id),
            by=c("ACCOUNT_NO", "PERSON_NO", "LOCATION_NO", "id")) %>%
  select(-id) %>%
  arrange(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE)

cutoff_reconnect$CUTOFF_DATE[is.na(cutoff_reconnect$CUTOFF_DATE)] <-
  "2000-01-01"
cutoff_reconnect$RECONNECT_DATE[is.na(cutoff_reconnect$RECONNECT_DATE)] <-
  "2099-12-31"

cutoff_time <- cutoff_reconnect %>%
  mutate(SS_ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
  group_by(SS_ACCOUNT_NO) %>%
  arrange(CUTOFF_DATE, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(CUTOFF_DATE)) >
                            cummax(as.numeric(RECONNECT_DATE)))[-n()])) %>%
  group_by(SS_ACCOUNT_NO, indx) %>%
  summarise(cutoff_start=min(CUTOFF_DATE), 
            cutoff_end=max(RECONNECT_DATE)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(cutoff_start=ifelse(is.na(cutoff_start),
                             mdy("12/31/2099"),
                             cutoff_start),
         cutoff_end=ifelse(is.na(cutoff_end),
                           mdy("12/31/2099"),
                           cutoff_end))

cutoff_time_count <- cutoff_time %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(count=n())

cutoff_time_count_1 <- cutoff_time_count %>%
  filter(count==1)

cutoff_time_count_above_1 <- cutoff_time_count %>%
  filter(count>1)

cutoff_time_above_1 <- cutoff_time %>%
  filter(SS_ACCOUNT_NO %in% cutoff_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_none <- delinquency_status %>%
  filter(!(ACCOUNT_NO %in% c(cutoff_time_count_above_1$SS_ACCOUNT_NO,
                             cutoff_time_count_1$SS_ACCOUNT_NO))) %>%
  mutate(cutoff=FALSE)

delinquency_status_sub <- delinquency_status %>%
  filter(ACCOUNT_NO %in% cutoff_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_rest <- delinquency_status %>%
  filter(ACCOUNT_NO %in% cutoff_time_count_1$SS_ACCOUNT_NO) %>%
  left_join(cutoff_time %>%
              filter(SS_ACCOUNT_NO %in% cutoff_time_count_1$SS_ACCOUNT_NO),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO")) %>%
  rowwise() %>%
  mutate(cutoff=between(BILL_RUN_DT, cutoff_start, cutoff_end)) %>%
  ungroup() %>%
  mutate(cutoff=ifelse(is.na(cutoff), FALSE, cutoff)) %>%
  select(-cutoff_start, -cutoff_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(cutoff=any(BILL_RUN_DT %between%
                      list(subset(cutoff_time_above_1,
                                  SS_ACCOUNT_NO==ACCOUNT_NO)$cutoff_start,
                           subset(cutoff_time_above_1,
                                  SS_ACCOUNT_NO==ACCOUNT_NO)$cutoff_end))) %>%
  ungroup()

delinquency_status <- rbind(delinquency_status_sub,
                            delinquency_status_rest,
                            delinquency_status_none)

# Save the dataset
save(delinquency_status,
     file=paste0(working_data_dir, "/delinquency_status.RData"))
