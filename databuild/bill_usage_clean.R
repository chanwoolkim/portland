# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/analysis_info_large.RData.gz"))


# Clean usage info ####
usage_info <- usage_info %>%
  mutate(report_context=trimws(report_context),
         weight=ifelse(is_bc_detail_prorated,
                       bc_active_days/bc_standard_days,
                       1)) %>%
  group_by(account_number, bill_run_date) %>%
  summarise(usage_bill_amount=sum(bc_detail_amount, na.rm=TRUE),
            water_cons=sum(cons_level_amount[report_context=="WCONS"], na.rm=TRUE),
            sewer_cons=sum(cons_level_amount[report_context=="SCONS"], na.rm=TRUE),
            
            water_var_price=weighted.mean(bc_detail_rate[report_context=="WCONS"],
                                          weight[report_context=="WCONS"], na.rm=TRUE),
            sewer_var_price=weighted.mean(bc_detail_rate[report_context=="SCONS"],
                                          weight[report_context=="SCONS"], na.rm=TRUE),
            water_fixed_price=weighted.mean(bc_detail_rate[report_context=="WBASE"],
                                            weight[report_context=="WBASE"], na.rm=TRUE),
            sewer_fixed_price=weighted.mean(bc_detail_rate[report_context=="SBASE"],
                                            weight[report_context=="SBASE"], na.rm=TRUE),
            storm_fixed_price=weighted.mean(bc_detail_rate[report_context=="STORM"],
                                            weight[report_context=="STORM"], na.rm=TRUE),
            storm_phs_fixed_price=weighted.mean(bc_detail_rate[report_context=="STORM_PHS"],
                                                weight[report_context=="STORM_PHS"], na.rm=TRUE),
            sewer_phs_var_price=weighted.mean(bc_detail_rate[report_context=="SEWER_PHS"],
                                              weight[report_context=="SEWER_PHS"], na.rm=TRUE),
            bod_var_price=weighted.mean(bc_detail_rate[report_context=="BOD"],
                                        weight[report_context=="BOD"], na.rm=TRUE),
            tss_var_price=weighted.mean(bc_detail_rate[report_context=="TSS"],
                                        weight[report_context=="TSS"], na.rm=TRUE),
            cleanriver_var_price=weighted.mean(bc_detail_rate[report_context=="CLEANRIVER"],
                                               weight[report_context=="CLEANRIVER"], na.rm=TRUE),
            clnrvrcrd_var_price=weighted.mean(bc_detail_rate[report_context=="CLNRVRCRD"],
                                              weight[report_context=="CLNRVRCRD"], na.rm=TRUE),
            swr_donation_price=weighted.mean(bc_detail_rate[report_context=="SWR DONAT"],
                                             weight[report_context=="SWR DONAT"], na.rm=TRUE),
            linc_sewer_price=weighted.mean(bc_detail_rate[report_context=="LINCSEWER"],
                                           weight[report_context=="LINCSEWER"], na.rm=TRUE),
            linc_water_price=weighted.mean(bc_detail_rate[report_context=="LINC"],
                                           weight[report_context=="LINC"], na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(bill_run_date=mdy(bill_run_date))


# Clean financial info ####
financial_info <- financial_info %>%
  mutate(summary=trimws(as.character(summary)))

# Save bills not in bill_info (to identify payment to final bill)
bill_info <- bill_info %>%
  select(account_number, bill_date) %>%
  distinct() %>%
  mutate(match=TRUE)

financial_info_leftover <- financial_info %>%
  left_join(bill_info,
            by=c("account_number", "bill_date")) %>%
  filter(is.na(match),
         transaction_code=="PYMNT",
         !grepl("COMMIT", transaction_type)) %>%
  select(-match)

financial_info <- financial_info %>%
  group_by(account_number, bill_date, summary) %>%
  summarise(bill_amount=sum(amount, na.rm=TRUE),
            adjusted_bill_amount=sum(adjusted_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(bill_date=mdy(bill_date))

financial_info <- financial_info %>%
  filter(!is.na(account_number),
         !is.na(bill_date)) %>%
  pivot_wider(id_cols=c(account_number, bill_date),
              values_from=adjusted_bill_amount,
              names_from=summary) %>%
  rename(bill_sewer_cons=SEWER,
         bill_water_cons=WATER,
         bill_payment=PYMNT,
         bill_penalty=PNLTY,
         bill_donate=DONAT,
         bill_bankrupt=BNKRP,
         bill_leaf=LRF)

save(financial_info_leftover,
     file=paste0(working_data_dir, "/financial_info_leftover.RData"))

save(usage_info, financial_info,
     file=paste0(working_data_dir, "/usage_financial.RData"))
