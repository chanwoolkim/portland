load(file=paste0(working_data_dir, "/analysis_info_large.RData.gz"))

usage_info <- usage_info %>%
  mutate(BILL_PRINT_CD=trimws(BILL_PRINT_CD),
         REPORT_CONTEXT=trimws(REPORT_CONTEXT)) %>%
  group_by(ACCOUNT_NO, BILL_RUN_DT) %>%
  summarise(usage_bill_amount=sum(BC_DETAIL_AMT, na.rm=TRUE),
            water_cons=sum(CONS_LEVEL_AMT[BILL_PRINT_CD=="WATER"], na.rm=TRUE),
            sewer_cons=sum(CONS_LEVEL_AMT[BILL_PRINT_CD=="SEWER"], na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(BILL_RUN_DT=mdy(BILL_RUN_DT))

financial_info <- financial_info %>%
  mutate(ACCOUNT_NO=as.character(SS_ACCOUNT_NO),
         SS_BILL_DT=as.character(SS_BILL_DT),
         ITEM_SUMMARY=trimws(as.character(ITEM_SUMMARY)),
         ITEM_AMT=as.numeric(as.character(ITEM_AMT)),
         ADJUSTED_ITEM_AMT=as.numeric(as.character(ADJUSTED_ITEM_AMT))) %>%
  group_by(ACCOUNT_NO, SS_BILL_DT, ITEM_SUMMARY) %>%
  summarise(BILL_AMT=sum(ITEM_AMT, na.rm=TRUE),
            ADJUSTED_BILL_AMT=sum(ADJUSTED_ITEM_AMT, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(BILL_RUN_DT=mdy(SS_BILL_DT))

financial_info <- financial_info %>%
  select(-SS_BILL_DT) %>%
  filter(!is.na(ACCOUNT_NO),
         !is.na(BILL_RUN_DT)) %>%
  pivot_wider(id_cols=c(ACCOUNT_NO, BILL_RUN_DT),
              values_from=BILL_AMT,
              names_from=ITEM_SUMMARY) %>%
  rename(bill_sewer_cons=SEWER,
         bill_water_cons=WATER,
         bill_payment=PYMNT,
         bill_penalty=PNLTY,
         bill_donate=DONAT,
         bill_bankrupt=BNKRP,
         bill_leaf=LRF)

save(usage_info, financial_info,
     file=paste0(working_data_dir, "/usage_financial.RData"))



# Diagnostic ####
bill_test <- bill_test %>%
  filter(SOURCE_CD=="", BILL_RUN_DT<=mdy("08/09/2021"), !is.na(BILL_AMT))

test <- bill_test %>%
  filter(round(BILL_AMT,2)!=round(AR_DUE_AFTER_BILL,2)) %>%
  filter(!(payment_arrange | financial_assist))

View(test)

bill_test <- bill_test %>%
  filter(!is.na(BILL_AMT)) %>%
  mutate(eq=round(BILL_AMT,2)==round(AR_DUE_AFTER_BILL,2)) %>%
  filter(!eq) %>%
  select(ACCOUNT_NO, BILL_RUN_DT, BILL_AMT, AR_DUE_AFTER_BILL)

bill_test <- delinquency_status %>%
  left_join(to_merge %>% select(ACCOUNT_NO, BILL_RUN_DT, BILL_AMT, ADJUSTED_BILL_AMT),
            by=c("ACCOUNT_NO", "BILL_RUN_DT"))


account_list <- c(2939882700, 2993715500, 2947378900, 2938578200, 2939033600)

usage_export <- data.frame()
bill_export <- data.frame()
financial_export <- data.frame()

for (i in 1:length(account_list)) {
  dates <- (bill_test %>% filter(ACCOUNT_NO==account_list[i]))$BILL_RUN_DT
  
  usage_s <- usage_info %>% filter(ACCOUNT_NO %in% account_list[i],
                                   mdy(BILL_RUN_DT) %in% dates)
  usage_export <- rbind(usage_export, usage_s)
  
  bill_s <- bill_info_filtered %>% filter(ACCOUNT_NO %in% account_list[i],
                                          mdy(BILL_RUN_DT) %in% dates)
  bill_export <- rbind(bill_export, bill_s)
  
  financial_s <- financial_info %>% filter(as.character(SS_ACCOUNT_NO) %in% account_list[i],
                                            mdy(as.character(SS_BILL_DT)) %in% dates)
  financial_export <- rbind(financial_export, financial_s)
}

write.csv(usage_export,
          file=paste0(working_data_dir, "/usage_export.csv"))
write.csv(bill_export,
          file=paste0(working_data_dir, "/bill_export.csv"))
write.csv(financial_export,
          file=paste0(working_data_dir, "/financial_export.csv"))
