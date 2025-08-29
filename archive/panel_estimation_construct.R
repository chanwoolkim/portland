# LOAD DATA
load(file=paste0(working_data_dir, "/portland_panel.RData"))


# Precleaning ####
portland_panel <- portland_panel %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  replace_na(list(senior_disabilities=FALSE))


# Aggregate monthly payments ####
# First aggregate consumption and relevant bills and assistances
portland_panel_sub <- portland_panel %>%
  filter(source_code!="") %>%
  arrange(tu_id, bill_date) %>%
  mutate(bill_date=ifelse(source_code!="QB1", NA, bill_date)) %>%
  fill(bill_date, .direction="downup") %>%
  mutate(bill_date=as_date(bill_date)) %>%
  group_by(tu_id, bill_date) %>%
  summarise(across(c("payment_arrange", "payment_arrange_status", "financial_assist",
                     "cutoff",
                     "usage_bill_amount", "water_cons", "sewer_cons",
                     "bill_penalty", "bill_donate", "bill_bankrupt", "bill_leaf",
                     "net_after_assistance", "bill_before_assistance", "discount_assistance",
                     "crisis_voucher_amount"),
                   .fns=~sum(., na.rm=TRUE))) %>%
  ungroup() %>%
  mutate(across(c("payment_arrange", "payment_arrange_status",
                  "financial_assist",
                  "cutoff"),
                .fns=~.>0))

portland_panel_quarter <- portland_panel %>%
  filter(is.na(source_code))

portland_panel_month <- portland_panel %>%
  filter(!is.na(source_code)) %>%
  select(-payment_arrange, -payment_arrange_status, -financial_assist,
         -cutoff,
         -usage_bill_amount, -water_cons, -sewer_cons,
         -bill_penalty, -bill_donate, -bill_bankrupt, -bill_leaf,
         -net_after_assistance, -bill_before_assistance, -discount_assistance,
         -crisis_voucher_amount) %>%
  left_join(portland_panel_sub, by=c("tu_id", "bill_date"))

portland_panel_estimation <- bind_rows(portland_panel_quarter, 
                                       portland_panel_month) %>%
  arrange(tu_id, bill_date)

rm(portland_panel_quarter, portland_panel_month, portland_panel_sub)

# Replace values as needed
portland_panel_estimation <- portland_panel_estimation %>%
  group_by(tu_id) %>%
  mutate(source_num=substr(source_code, 3, 3) %>% as.numeric(),
         across(c("source_num", "type_code", "previous_bill", "total_payments"),
                .fns=list(lag=~lag(.))),
         across(c("source_num", "type_code", "previous_bill", "total_payments"),
                .fns=list(lag_lag=~lag(., n=2))),
         across(c("source_num", "type_code", "previous_bill"),
                .fns=list(lead=~lead(.))),
         across(c("source_num", "type_code", "previous_bill"),
                .fns=list(lead_lead=~lead(., n=2)))) %>%
  ungroup()

portland_panel_estimation <- portland_panel_estimation %>%
  mutate(previous_bill=
           case_when(type_code=="FINAL" & source_num_lag==2 ~
                       previous_bill_lag,
                     source_num_lag_lag==2 ~
                       previous_bill_lag_lag,
                     .default=previous_bill),
         total_payments=
           case_when(type_code=="FINAL" & source_num_lag==2 ~
                       total_payments+total_payments_lag,
                     source_num_lag_lag==2 ~
                       total_payments+total_payments_lag+total_payments_lag_lag,
                     .default=total_payments),
         leftover_debt=case_when(type_code=="FINAL" & source_num_lag==2 ~
                                   previous_bill-total_payments,
                                 .default=leftover_debt),
         current_bill=case_when(source_num==1 & source_num_lead==2 ~
                                  previous_bill_lead,
                                source_num==1 & type_code_lead=="FINAL" ~
                                  previous_bill_lead,
                                .default=current_bill))

portland_panel_estimation <- portland_panel_estimation %>%
  filter(type_code!="MSTMT" |
           (type_code=="MSTMT" & is.na(type_code_lead)) |
           (source_num==2 & source_num_lead==3 & is.na(type_code_lead_lead))) %>%
  mutate(agg=case_when((type_code=="FINAL" & source_num_lag==2) |
                         (source_num_lag==3 & source_num_lag_lag==2) ~ "AGG",
                       (type_code=="MSTMT" & is.na(type_code_lead)) |
                         (type_code=="MSTMT" & is.na(type_code_lead_lead)) ~ "CHOP",
                       .default="")) %>%
  select(-contains("lag"), -contains("lead"), -source_num) %>%
  mutate(delinquent=leftover_debt>0)

# Save the dataset
save(portland_panel_estimation,
     file=paste0(working_data_dir, "/portland_panel_estimation.RData"))
