# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))

# Only consider single family
account_info_subset <- account_info %>%
  mutate(occupancy_code=trimws(occupancy_code),
         cycle_num=as.numeric(trimws(cycle_code)),
         cycle_code=as.numeric(trimws(cycle_code)),
         cycle_code=case_when(cycle_code %in% 1:64 ~ "QUARTER",
                              cycle_code %in% 65:85 ~ "MONTH",
                              cycle_code %in% 90:92 ~ "BIMONTH",
                              .default="NOINFO")) %>%
  filter(occupancy_code %in% c("RESSF", "ASST")) %>%
  select(tu_id, cycle_num, cycle_code) %>%
  distinct(tu_id, cycle_code, .keep_all=TRUE) %>%
  mutate(account=TRUE)

# Average payment arrangement amount
bill_info <- bill_info %>%
  mutate(start_date=mdy(start_date),
         end_date=mdy(end_date),
         due_date=mdy(due_date),
         bill_date=mdy(bill_date))

bill_info_filtered <- bill_info %>% 
  filter(!is_canceled,
         !is.na(start_date), 
         !is.na(end_date),
         !is.na(due_date),
         !is_error,
         !is_voided,
         audit_or_live=="L",
         type_code %in% c("REGLR", "MSTMT", "FINAL"),
         !is_corrected)

# Those who are not on a payment plan
no_plan_bill <- bill_info_filtered %>%
  filter(is.na(source_code)) %>%
  mutate(previous_bill_amount=previous_bill_amount+non_bill_generated_changes,
         delinquent=previous_bill_amount+total_payments>0,
         delinquent_amount=ifelse(previous_bill_amount+total_payments>0,
                                  previous_bill_amount+total_payments,
                                  0),
         bill_year=year(bill_date)) %>%
  select(tu_id, due_date, bill_date,
         bill_year, delinquent, delinquent_amount,
         previous_bill_amount, total_payments, ar_due_before_bill, ar_due_after_bill,
         non_bill_generated_changes,
         source_code, type_code, is_off_cycle,
         start_date, end_date)

# Those on a payment plan
plan_bill <- bill_info_filtered %>%
  filter(!is.na(source_code)) %>%
  mutate(previous_bill_amount=previous_bill_amount+non_bill_generated_changes,
         delinquent=ar_due_before_bill>0,
         delinquent_amount=ifelse(ar_due_before_bill>0,
                                  ar_due_before_bill,
                                  0),
         bill_year=year(bill_date)) %>%
  select(tu_id, due_date, bill_date,
         bill_year, delinquent, delinquent_amount,
         previous_bill_amount, total_payments, ar_due_before_bill, ar_due_after_bill,
         non_bill_generated_changes,
         source_code, type_code, is_off_cycle,
         start_date, end_date)

delinquency_status <- bind_rows(no_plan_bill, plan_bill) %>%
  left_join(account_info_subset,
            by="tu_id") %>%
  filter(account, cycle_code!="NOINFO")

# Payment arrangement amount
payment_arrange_amount <- payment_arrangement_info %>%
  group_by(payment_plan_id) %>%
  summarise(amount_due=sum(amount, na.rm=TRUE),
            amount_outstanding=sum(outstanding_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(amount_paid=amount_due-amount_outstanding)

payment_arrange_amount <- payment_arrangement %>%
  filter(!grepl("^[0-9]", status_code)) %>%
  mutate(status_code=trimws(status_code),
         status_code=ifelse(status_code=="T", "T", "P"),
         payment_arrange_start=mdy(start_date),
         payment_arrange_end=mdy(end_date),
         payment_arrange_start_year=year(payment_arrange_start),
         payment_arrange_end_year=year(payment_arrange_end)) %>%
  right_join(payment_arrange_amount, by="payment_plan_id") %>%
  filter(payment_arrange_start_year>=2019 |
           payment_arrange_end_year>=2019)

# Payment arrangement
payment_arrange_time <- payment_arrange_amount %>%
  group_by(tu_id) %>%
  arrange(payment_arrange_start, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(payment_arrange_start)) >
                            cummax(as.numeric(payment_arrange_end)))[-n()])) %>%
  group_by(tu_id, indx) %>%
  summarise(terminated=any(status_code=="T"),
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
  group_by(tu_id) %>%
  summarise(count=n())

payment_arrange_time_count_1 <- payment_arrange_time_count %>%
  filter(count==1)

payment_arrange_time_count_above_1 <- payment_arrange_time_count %>%
  filter(count>1)

payment_arrange_time_above_1 <- payment_arrange_time %>%
  filter(tu_id %in% payment_arrange_time_count_above_1$tu_id)

delinquency_status_none <- delinquency_status %>%
  filter(!(tu_id %in% c(payment_arrange_time_count_above_1$tu_id,
                        payment_arrange_time_count_1$tu_id))) %>%
  mutate(payment_arrange=FALSE,
         payment_arrange_status=NA)

delinquency_status_sub <- delinquency_status %>%
  filter(tu_id %in% payment_arrange_time_count_above_1$tu_id)

delinquency_status_rest <- delinquency_status %>%
  filter(tu_id %in% payment_arrange_time_count_1$tu_id) %>%
  left_join(payment_arrange_time %>%
              filter(tu_id %in% payment_arrange_time_count_1$tu_id),
            by=c("tu_id"="tu_id")) %>%
  rowwise() %>%
  mutate(payment_arrange=between(bill_date, payment_arrange_start, payment_arrange_end),
         payment_arrange_status=ifelse(payment_arrange, terminated, NA)) %>%
  ungroup() %>%
  mutate(payment_arrange=ifelse(is.na(payment_arrange), FALSE, payment_arrange)) %>%
  select(-payment_arrange_start, -payment_arrange_end, -terminated)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(payment_arrange=any(bill_date %between%
                               list(subset(payment_arrange_time_above_1,
                                           tu_id==tu_id)$payment_arrange_start,
                                    subset(payment_arrange_time_above_1,
                                           tu_id==tu_id)$payment_arrange_end)),
         payment_arrange_status=
           ifelse(payment_arrange,
                  subset(payment_arrange_time_above_1,
                         tu_id==tu_id)$terminated[
                           which(bill_date %between%
                                   list(subset(payment_arrange_time_above_1,
                                               tu_id==tu_id)$payment_arrange_start,
                                        subset(payment_arrange_time_above_1,
                                               tu_id==tu_id)$payment_arrange_end))],
                  NA)) %>%
  ungroup()

delinquency_status <- bind_rows(delinquency_status_sub,
                                delinquency_status_rest,
                                delinquency_status_none)

# Attach financial info onto payment arrangement
financial_assist_account <- financial_assist %>%
  mutate(financial_assist_start=mdy(effective_date),
         financial_assist_end=mdy(expiration_date)) %>%
  filter(year(financial_assist_start)>=2019 |
           year(financial_assist_end)>=2019) %>%
  select(tu_id, financial_assist_start, financial_assist_end)

# Financial assistance
financial_assist_time <- financial_assist_account %>%
  group_by(tu_id) %>%
  arrange(financial_assist_start, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(financial_assist_start)) >
                            cummax(as.numeric(financial_assist_end)))[-n()])) %>%
  group_by(tu_id, indx) %>%
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
  group_by(tu_id) %>%
  summarise(count=n())

financial_assist_time_count_1 <- financial_assist_time_count %>%
  filter(count==1)

financial_assist_time_count_above_1 <- financial_assist_time_count %>%
  filter(count>1)

financial_assist_time_above_1 <- financial_assist_time %>%
  filter(tu_id %in% financial_assist_time_count_above_1$tu_id)

delinquency_status_none <- delinquency_status %>%
  filter(!(tu_id %in% c(financial_assist_time_count_above_1$tu_id,
                        financial_assist_time_count_1$tu_id))) %>%
  mutate(financial_assist=FALSE)

delinquency_status_sub <- delinquency_status %>%
  filter(tu_id %in% financial_assist_time_count_above_1$tu_id)

delinquency_status_rest <- delinquency_status %>%
  filter(tu_id %in% financial_assist_time_count_1$tu_id) %>%
  left_join(financial_assist_time %>%
              filter(tu_id %in% financial_assist_time_count_1$tu_id),
            by=c("tu_id"="tu_id")) %>%
  rowwise() %>%
  mutate(financial_assist=between(bill_date, financial_assist_start, financial_assist_end)) %>%
  ungroup() %>%
  mutate(financial_assist=ifelse(is.na(financial_assist), FALSE, financial_assist)) %>%
  select(-financial_assist_start, -financial_assist_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(financial_assist=any(bill_date %between%
                                list(subset(financial_assist_time_above_1,
                                            tu_id==tu_id)$financial_assist_start,
                                     subset(financial_assist_time_above_1,
                                            tu_id==tu_id)$financial_assist_end))) %>%
  ungroup()

delinquency_status <- bind_rows(delinquency_status_sub,
                                delinquency_status_rest,
                                delinquency_status_none)

# Cutoff/reconnect
cutoff_reconnect <- cutoff_info %>%
  mutate(effective_date=mdy(effective_date)) %>%
  arrange(tu_id, effective_date) %>%
  group_by(tu_id) %>%
  mutate(lead_cutoff=lead(request_type),
         lead_date=lead(effective_date)) %>%
  filter(request_type=="CUTOF") %>%
  ungroup()

cutoff_reconnect$lead_date[cutoff_reconnect$lead_cutoff=="CUTOF"] <- NA
cutoff_reconnect$lead_cutoff[cutoff_reconnect$lead_cutoff=="CUTOF"] <- NA

cutoff_reconnect <- cutoff_reconnect %>%
  select(-action_id, -action_code, 
         -request_number, -request_type,
         -received_date, -scheduled_timestamp,
         -resolution_code, -lead_cutoff) %>%
  rename(cutoff_date=effective_date,
         reconnect_date=lead_date)

cutoff_reconnect$cutoff_date[is.na(cutoff_reconnect$cutoff_date)] <-
  "2000-01-01"
cutoff_reconnect$reconnect_date[is.na(cutoff_reconnect$reconnect_date)] <-
  "2099-12-31"

cutoff_time <- cutoff_reconnect %>%
  group_by(tu_id) %>%
  arrange(cutoff_date, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(cutoff_date)) >
                            cummax(as.numeric(reconnect_date)))[-n()])) %>%
  group_by(tu_id, indx) %>%
  summarise(cutoff_start=min(cutoff_date), 
            cutoff_end=max(reconnect_date)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(cutoff_start=ifelse(is.na(cutoff_start),
                             mdy("12/31/2099"),
                             cutoff_start),
         cutoff_end=ifelse(is.na(cutoff_end),
                           mdy("12/31/2099"),
                           cutoff_end))

cutoff_time_count <- cutoff_time %>%
  group_by(tu_id) %>%
  summarise(count=n())

cutoff_time_count_1 <- cutoff_time_count %>%
  filter(count==1)

cutoff_time_count_above_1 <- cutoff_time_count %>%
  filter(count>1)

cutoff_time_above_1 <- cutoff_time %>%
  filter(tu_id %in% cutoff_time_count_above_1$tu_id)

delinquency_status_none <- delinquency_status %>%
  filter(!(tu_id %in% c(cutoff_time_count_above_1$tu_id,
                        cutoff_time_count_1$tu_id))) %>%
  mutate(cutoff=FALSE)

delinquency_status_sub <- delinquency_status %>%
  filter(tu_id %in% cutoff_time_count_above_1$tu_id)

delinquency_status_rest <- delinquency_status %>%
  filter(tu_id %in% cutoff_time_count_1$tu_id) %>%
  left_join(cutoff_time %>%
              filter(tu_id %in% cutoff_time_count_1$tu_id),
            by=c("tu_id"="tu_id")) %>%
  rowwise() %>%
  mutate(cutoff=between(bill_date, cutoff_start, cutoff_end)) %>%
  ungroup() %>%
  mutate(cutoff=ifelse(is.na(cutoff), FALSE, cutoff)) %>%
  select(-cutoff_start, -cutoff_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(cutoff=any(bill_date %between%
                      list(subset(cutoff_time_above_1,
                                  tu_id==tu_id)$cutoff_start,
                           subset(cutoff_time_above_1,
                                  tu_id==tu_id)$cutoff_end))) %>%
  ungroup()

delinquency_status <- bind_rows(delinquency_status_sub,
                                delinquency_status_rest,
                                delinquency_status_none) %>%
  distinct()

# Flag first bill and resumption
delinquency_status <- delinquency_status %>%
  arrange(tu_id, bill_date) %>%
  group_by(tu_id) %>%
  mutate(rnum=row_number(),
         lag_final=lag(type_code)) %>%
  ungroup() %>%
  mutate(lag_previous_bill_amount=lag(previous_bill_amount)) %>%
  filter(!(lag_previous_bill_amount==previous_bill_amount & lag_final=="FINAL" & type_code=="FINAL")) %>%
  mutate(type_code=
           case_when(type_code!="FINAL" & previous_bill_amount<=0 & total_payments>=0 & rnum==1 ~ "FIRST",
                     type_code!="FINAL" & lag_final=="FINAL" & !is.na(lag_final) ~ "RESUME",
                     type_code!="FINAL" & previous_bill_amount<=0 & total_payments<0 & rnum==1 ~ "RESUME",
                     .default=type_code)) %>%
  select(-lag_final, -lag_previous_bill_amount, -rnum) %>%
  distinct()

# Save the dataset
save(delinquency_status,
     file=paste0(working_data_dir, "/delinquency_status.RData"))
