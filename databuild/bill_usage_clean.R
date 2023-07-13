load(file=paste0(working_data_dir, "/analysis_info_large.RData.gz"))

usage_info <- usage_info %>%
  mutate(BILL_PRINT_CD=trimws(BILL_PRINT_CD),
         REPORT_CONTEXT=trimws(REPORT_CONTEXT)) %>%
  group_by(ACCOUNT_NO, BILL_RUN_DT) %>%
  summarise(usage_bill_amount=sum(BC_DETAIL_AMT, na.rm=TRUE),
            usage_bill_water_cons=sum(BC_DETAIL_AMT[BILL_PRINT_CD=="WATER"], na.rm=TRUE),
            usage_bill_sewer_cons=sum(BC_DETAIL_AMT[BILL_PRINT_CD=="SEWER"], na.rm=TRUE),
            water_cons=sum(CONS_LEVEL_AMT[REPORT_CONTEXT=="WCONS"], na.rm=TRUE),
            sewer_cons=sum(CONS_LEVEL_AMT[REPORT_CONTEXT=="SCONS"], na.rm=TRUE)) %>%
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
              values_from=ADJUSTED_BILL_AMT,
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
