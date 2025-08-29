# Measures of Delinquency/Cutoff

load(file=paste0(working_data_dir, "/analysis_info.RData"))

bill_info <- bill_info %>%
  mutate(start_date=mdy(start_date),
         end_date=mdy(end_date),
         due_date=mdy(due_date))

bill_info_filtered <- bill_info %>% 
  filter(!is_canceled,
         !is.na(start_date), 
         !is.na(end_date),
         !is.na(due_date),
         !is_error,
         !is_voided,
         audit_or_live=="L",
         type_code %in% c("REGLR", "MSTMT"),
         !is_corrected)

# Those who are not on a payment plan
no_plan_bill <- bill_info_filtered %>%
  filter(is.na(source_code)) %>%
  mutate(delinquent=previous_bill_amount+total_payments>0,
         delinquent_amount=ifelse(previous_bill_amount+total_payments>0,
                                  previous_bill_amount+total_payments,
                                  0),
         due_year=year(due_date)) %>%
  group_by(tu_id, due_year) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=sum(delinquent_amount, na.rm=TRUE),
            total_bill=sum(previous_bill_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  select(tu_id, due_year, n_bill, delinquent, delinquent_amount, total_bill)

# Those on a payment plan
plan_bill <- bill_info_filtered %>%
  filter(!is.na(source_code)) %>%
  mutate(delinquent=ar_due_before_bill>0,
         delinquent_amount=ifelse(ar_due_before_bill>0,
                                  ar_due_before_bill,
                                  0),
         due_year=year(due_date)) %>%
  group_by(tu_id, due_year) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=sum(delinquent_amount, na.rm=TRUE),
            total_bill=sum(previous_bill_amount[source_code=="QB2"], na.rm=TRUE)) %>%
  ungroup() %>%
  select(tu_id, due_year, n_bill, delinquent, delinquent_amount, total_bill)

delinquency_status <- bind_rows(no_plan_bill, plan_bill) %>%
  group_by(tu_id, due_year) %>%
  summarise(n_bill=sum(n_bill, na.rm=TRUE),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=sum(delinquent_amount, na.rm=TRUE),
            total_bill=sum(total_bill, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill,
         delinquency_amount_rate=delinquent_amount/total_bill,
         delinquency_amount_rate=ifelse(delinquency_amount_rate>1,
                                        1,
                                        delinquency_amount_rate),
         delinquency_amount_rate=ifelse(is.na(delinquency_amount_rate),
                                        0,
                                        delinquency_amount_rate)) %>%
  pivot_wider(id_cols=tu_id, 
              names_from=due_year, 
              values_from=c("n_bill",
                            "delinquent",
                            "delinquency_rate",
                            "delinquent_amount",
                            "delinquency_amount_rate",
                            "total_bill"),
              values_fill=0) %>%
  distinct()


# Cutoff by year ####
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

cutoff_reconnect$reconnect_date[is.na(cutoff_reconnect$reconnect_date)] <-
  "2099-12-31"

cutoff_reconnect <- cutoff_reconnect %>%
  rowwise() %>%
  mutate(cutoff_2019=
           between(2019, year(cutoff_date), year(reconnect_date)),
         cutoff_2020=
           between(2020, year(cutoff_date), year(reconnect_date)),
         cutoff_2021=
           between(2021, year(cutoff_date), year(reconnect_date)),
         cutoff_2022=
           between(2022, year(cutoff_date), year(reconnect_date)),
         cutoff_2023=
           between(2023, year(cutoff_date), year(reconnect_date)),
         cutoff_2024=
           between(2024, year(cutoff_date), year(reconnect_date))) %>%
  group_by(tu_id) %>%
  summarise(cutoff_2019=sum(cutoff_2019),
            cutoff_2020=sum(cutoff_2020),
            cutoff_2021=sum(cutoff_2021),
            cutoff_2022=sum(cutoff_2022),
            cutoff_2023=sum(cutoff_2023),
            cutoff_2024=sum(cutoff_2024)) %>%
  ungroup() %>%
  mutate(cutoff_2019=cutoff_2019>0,
         cutoff_2020=cutoff_2020>0,
         cutoff_2021=cutoff_2021>0,
         cutoff_2022=cutoff_2022>0,
         cutoff_2023=cutoff_2023>0,
         cutoff_2024=cutoff_2024>0) %>%
  distinct()

# Save data
save(delinquency_status,
     cutoff_reconnect,
     file=paste0(working_data_dir, "/delinquency_info.RData"))
