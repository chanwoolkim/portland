# LOAD DATA
load(file=paste0(working_data_dir, "/portland_panel.RData"))


# Precleaning ####
portland_panel <- portland_panel %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  replace_na(list(senior_disabilities=FALSE))


# Aggregate monthly payments ####
# First aggregate consumption and relevant bills and assistances
portland_panel_sub <- portland_panel %>%
  filter(SOURCE_CD!="") %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT) %>%
  mutate(BILL_RUN_DT=ifelse(SOURCE_CD!="QB1", NA, BILL_RUN_DT)) %>%
  fill(BILL_RUN_DT, .direction="downup") %>%
  mutate(BILL_RUN_DT=as_date(BILL_RUN_DT)) %>%
  group_by(ACCOUNT_NO, BILL_RUN_DT) %>%
  summarise(across(c("payment_arrange", "payment_arrange_status", "financial_assist",
                     "cutoff",
                     "usage_bill_amount", "usage_bill_water_cons", "usage_bill_sewer_cons",
                     "water_cons", "sewer_cons",
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
  filter(SOURCE_CD=="")

portland_panel_month <- portland_panel %>%
  filter(SOURCE_CD!="") %>%
  select(-payment_arrange, -payment_arrange_status, -financial_assist,
         -cutoff,
         -usage_bill_amount, -usage_bill_water_cons, -usage_bill_sewer_cons,
         -water_cons, -sewer_cons,
         -bill_penalty, -bill_donate, -bill_bankrupt, -bill_leaf,
         -net_after_assistance, -bill_before_assistance, -discount_assistance,
         -crisis_voucher_amount) %>%
  left_join(portland_panel_sub, by=c("ACCOUNT_NO", "BILL_RUN_DT"))

portland_panel_estimation <- rbind(portland_panel_quarter, portland_panel_month) %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT)

rm(portland_panel_quarter, portland_panel_month, portland_panel_sub)

# Replace values as needed
portland_panel_estimation <- portland_panel_estimation %>%
  group_by(ACCOUNT_NO) %>%
  mutate(source_num=substr(SOURCE_CD, 3, 3) %>% as.numeric(),
         across(c("source_num", "BILL_TP", "previous_bill", "total_payments"),
                .fns=list(lag=~lag(.))),
         across(c("source_num", "BILL_TP", "previous_bill", "total_payments"),
                .fns=list(lag_lag=~lag(., n=2))),
         across(c("source_num", "BILL_TP", "previous_bill"),
                .fns=list(lead=~lead(.))),
         across(c("source_num", "BILL_TP", "previous_bill"),
                .fns=list(lead_lead=~lead(., n=2)))) %>%
  ungroup()

portland_panel_estimation <- portland_panel_estimation %>%
  mutate(previous_bill=
           case_when(BILL_TP=="FINAL" & source_num_lag==2 ~
                       previous_bill_lag,
                     source_num_lag_lag==2 ~
                       previous_bill_lag_lag,
                     .default=previous_bill),
         total_payments=
           case_when(BILL_TP=="FINAL" & source_num_lag==2 ~
                       total_payments+total_payments_lag,
                     source_num_lag_lag==2 ~
                       total_payments+total_payments_lag+total_payments_lag_lag,
                     .default=total_payments),
         leftover_debt=case_when(BILL_TP=="FINAL" & source_num_lag==2 ~
                                   previous_bill-total_payments,
                                 .default=leftover_debt),
         current_bill=case_when(source_num==1 & source_num_lead==2 ~
                                  previous_bill_lead,
                                source_num==1 & BILL_TP_lead=="FINAL" ~
                                  previous_bill_lead,
                                .default=current_bill))

portland_panel_estimation <- portland_panel_estimation %>%
  filter(BILL_TP!="MSTMT" |
           (BILL_TP=="MSTMT" & is.na(BILL_TP_lead)) |
           (source_num==2 & source_num_lead==3 & is.na(BILL_TP_lead_lead))) %>%
  mutate(agg=case_when((BILL_TP=="FINAL" & source_num_lag==2) |
                         (source_num_lag==3 & source_num_lag_lag==2) ~ "AGG",
                       (BILL_TP=="MSTMT" & is.na(BILL_TP_lead)) |
                         (BILL_TP=="MSTMT" & is.na(BILL_TP_lead_lead)) ~ "CHOP",
                       .default="")) %>%
  select(-contains("lag"), -contains("lead"), -source_num) %>%
  mutate(delinquent=leftover_debt>0)

# Save the dataset
save(portland_panel_estimation,
     file=paste0(working_data_dir, "/portland_panel_estimation.RData"))
