# LOAD DATA
load(file=paste0(working_data_dir, "/portland_panel.RData"))


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

rm(portland_panel, portland_panel_sub, portland_panel_quarter, portland_panel_month)

# Replace values as needed
portland_panel_estimation <- portland_panel_estimation %>%
  group_by(ACCOUNT_NO) %>%
  mutate(source_num=substr(SOURCE_CD, 3, 3) %>% as.numeric(),
         source_lag=lag(source_num),
         source_lead=lead(source_num),
         final_lag=lag(BILL_TP),
         final_lead=lead(BILL_TP),
         across(everything(),
                .fns=list(lag=~lag(.))),
         across(everything(),
                .fns=list(lead=~lead(.)))) %>%
  ungroup()

portland_patnel_estimation <- portland_panel_estimation %>%
  mutate(across(c("DUE_DT"),
                .fns=~case_when(SOURCE_CD=="QB2" & final_lead=="FINAL" ~
                                  get(paste0(deparse(substitute(.x)), "_lead")),
                                SOURCE_CD=="QB2" & source_lead==3 ~
                                  get(paste0(deparse(substitute(.x)), "_lead_lead")),
                                SOURCE_CD=="QB1" & final_lead=="FINAL" ~
                                  get(paste0(deparse(substitute(.x)), "_lead")),
                                .default=.x)),
         across(c("current_bill"),
                .fns=~case_when(SOURCE_CD=="QB1" & final_lead=="FINAL" ~
                                  get(paste0(deparse(substitute(.x)), "_lead")),
                                .default=.x)),
         across(c("current_bill"),
                .fns=~case_when(SOURCE_CD=="QB1" & final_lead=="FINAL" ~
                                  get(paste0(deparse(substitute(.x)), "_lead")),
                                .default=.x)))

portland_panel_estimation <- portland_panel_estimation %>%
  filter() %>%
  select(-contains("lead"))

# Save the dataset
save(portland_panel_estimation,
     file=paste0(working_data_dir, "/portland_panel_estimation.RData"))
