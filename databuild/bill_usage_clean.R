load(file=paste0(working_data_dir, "/analysis_info_large.RData.gz"))

usage_info <- usage_info %>%
  mutate(BILL_PRINT_CD=trimws(BILL_PRINT_CD),
         REPORT_CONTEXT=trimws(REPORT_CONTEXT),
         WEIGHT=ifelse(BC_DETAIL_PRORATED_YN,
                       BC_ACTIVE_DAYS/BC_STANDARD_DAYS,
                       1)) %>%
  group_by(ACCOUNT_NO, BILL_RUN_DT) %>%
  summarise(usage_bill_amount=sum(BC_DETAIL_AMT, na.rm=TRUE),
            usage_bill_water_cons=sum(BC_DETAIL_AMT[BILL_PRINT_CD=="WATER"], na.rm=TRUE),
            usage_bill_sewer_cons=sum(BC_DETAIL_AMT[BILL_PRINT_CD=="SEWER"], na.rm=TRUE),
            water_cons=sum(CONS_LEVEL_AMT[REPORT_CONTEXT=="WCONS"], na.rm=TRUE),
            sewer_cons=sum(CONS_LEVEL_AMT[REPORT_CONTEXT=="SCONS"], na.rm=TRUE),
            
            water_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="WCONS"],
                                          WEIGHT[REPORT_CONTEXT=="WCONS"], na.rm=TRUE),
            sewer_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="SCONS"],
                                          WEIGHT[REPORT_CONTEXT=="SCONS"], na.rm=TRUE),
            water_fixed_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="WBASE"],
                                          WEIGHT[REPORT_CONTEXT=="WBASE"], na.rm=TRUE),
            sewer_fixed_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="SBASE"],
                                          WEIGHT[REPORT_CONTEXT=="SBASE"], na.rm=TRUE),
            storm_fixed_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="STORM"],
                                            WEIGHT[REPORT_CONTEXT=="STORM"], na.rm=TRUE),
            storm_phs_fixed_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="STORM_PHS"],
                                            WEIGHT[REPORT_CONTEXT=="STORM_PHS"], na.rm=TRUE),
            sewer_phs_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="SEWER_PHS"],
                                            WEIGHT[REPORT_CONTEXT=="SEWER_PHS"], na.rm=TRUE),
            bod_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="BOD"],
                                                WEIGHT[REPORT_CONTEXT=="BOD"], na.rm=TRUE),
            tss_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="TSS"],
                                        WEIGHT[REPORT_CONTEXT=="TSS"], na.rm=TRUE),
            cleanriver_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="CLEANRIVER"],
                                        WEIGHT[REPORT_CONTEXT=="CLEANRIVER"], na.rm=TRUE),
            clnrvrcrd_var_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="CLNRVRCRD"],
                                               WEIGHT[REPORT_CONTEXT=="CLNRVRCRD"], na.rm=TRUE),
            swr_donation_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="SWR DONAT"],
                                              WEIGHT[REPORT_CONTEXT=="SWR DONAT"], na.rm=TRUE),
            linc_sewer_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="LINCSEWER"],
                                             WEIGHT[REPORT_CONTEXT=="LINCSEWER"], na.rm=TRUE),
            linc_water_price=weighted.mean(BC_DETAIL_RATE[REPORT_CONTEXT=="LINC"],
                                             WEIGHT[REPORT_CONTEXT=="LINC"], na.rm=TRUE)) %>%
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
