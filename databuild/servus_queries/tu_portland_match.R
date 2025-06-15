load(paste0(working_data_dir, "/tu_data.RData"))
load(paste0(working_data_dir, "/analysis_info_large.RData.gz"))

exclusion_accounts_early <- 
  read_xlsx(paste0(auxiliary_data_dir, 
                   "/RCT1AcctsNotBilledOptOutTrialAnonymizedBooth_FINAL03122025.xlsx"),
            sheet="10-11-2024")

exclusion_accounts_late <-
  read_xlsx(paste0(auxiliary_data_dir, 
                   "/RCT1AcctsNotBilledOptOutTrialAnonymizedBooth_FINAL03122025.xlsx"),
            sheet="3-12-2025")

exclusion_accounts <- bind_rows(exclusion_accounts_early %>%
                                  transmute(tu_id=`Tu Id`,
                                            cycle=NA,
                                            reason=`Exclusion Reason`), 
                                exclusion_accounts_late %>%
                                  transmute(tu_id=`TU Number`,
                                            cycle=Cycle,
                                            reason=`Reason`))

rct_exclusion <- financial_info %>%
  filter(tu_id %in% exclusion_accounts$`Tu Id`) %>%
  select(transaction_number)

write_csv(rct_exclusion, 
          paste0(working_data_dir, "/servus_query/rct_exclusion.csv"))

rct_subject <- read_csv(paste0(working_data_dir, "/portland_rct_subject.csv"))
rct_additional <- read_csv(paste0(working_data_dir, "/portland_rct_additional.csv"))

rct_subject <- bind_rows(rct_subject, rct_additional)

rct_info <- financial_info %>%
  filter(tu_id %in% rct_subject$tu_id) %>%
  select(transaction_number, tu_id) %>%
  left_join(rct_subject, by="tu_id") %>%
  select(-tu_id)

write_csv(rct_info, 
          paste0(auxiliary_data_dir, "/rct_info.csv"))

tu_data <- tu_data %>%
  mutate(credit_score=ifelse(credit_score<300, NA, credit_score))

tu_income <- tu_data %>%
  mutate(year=year(credit_date),
         quarter=quarter(credit_date)) %>%
  select(year, quarter, tu_id, etie, credit_score) %>%
  distinct()

financial_income <- financial_info %>%
  mutate(transaction_date=mdy(transaction_date)) %>%
  mutate(year=year(transaction_date),
         quarter=quarter(transaction_date)) %>%
  select(tu_id, transaction_number, year, quarter)

financial_income_pre_2023 <- financial_income %>%
  filter(year<2023) %>%
  left_join(tu_income,
            by=c("tu_id", "year", "quarter"))

financial_income_post_2023 <- financial_income %>%
  filter(year>=2023) %>%
  left_join(tu_income %>%
              filter(year==2023) %>%
              select(-year, -quarter),
            by=c("tu_id"))

financial_income <- bind_rows(financial_income_pre_2023,
                              financial_income_post_2023)

financial_income <- financial_income %>%
  arrange(tu_id, year, quarter) %>%
  group_by(tu_id) %>%
  fill(etie, .direction="downup") %>%
  fill(credit_score, .direction="downup") %>%
  ungroup() %>%
  filter(!is.na(etie), !is.na(credit_score)) %>%
  select(transaction_number, etie, credit_score)

write_csv(financial_income, 
          paste0(auxiliary_data_dir, "/financial_income.csv"))

tu_data <- tu_data %>% 
  filter(credit_date>mdy("01/01/2023")) %>%
  group_by(tu_id) %>% 
  summarise(etie=mean(etie, na.rm=TRUE), 
            credit_score=mean(credit_score, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(income_quartile=ntile(etie, 4),
         credit_quartile=ntile(credit_score, 4))

financial_quartile <- financial_info %>%
  select(tu_id, transaction_number) %>%
  left_join(tu_data %>% 
              select(tu_id, income_quartile, credit_quartile), 
            by="tu_id") %>%
  select(transaction_number, income_quartile, credit_quartile)

write_csv(financial_quartile, 
          paste0(auxiliary_data_dir, "/financial_quartile.csv"))
