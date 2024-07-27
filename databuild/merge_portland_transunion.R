# LOAD DATA
load(file=paste0(working_data_dir, "/portland_panel_estimation.RData"))
load(file=paste0(working_data_dir, "/tu_data.RData"))


# Precleaning ####
tu_data <- tu_data %>%
  select(tu_id,
         credit_date, credit_score,
         hh_income_estimate, 
         ethnicity) %>%
  mutate(year_credit=year(credit_date),
         quarter_credit=quarter(credit_date))

portland_transunion_pre_2023 <- portland_panel_estimation %>%
  mutate(year_bill=year(bill_date),
         quarter_bill=quarter(bill_date)) %>%
  filter(year_bill<2023) %>%
  left_join(tu_data, by=c("tu_id",
                          "year_bill"="year_credit",
                          "quarter_bill"="quarter_credit"))

portland_transunion_post_2023 <- portland_panel_estimation %>%
  mutate(year_bill=year(bill_date),
         quarter_bill=quarter(bill_date)) %>%
  filter(year_bill>=2023) %>%
  left_join(tu_data %>% filter(year_credit==2023), by="tu_id")

portland_transunion <- bind_rows(portland_transunion_pre_2023,
                                 portland_transunion_post_2023) %>%
  select(-year_bill, -quarter_bill, 
         -year_credit, -quarter_credit, -credit_date) %>%
  mutate(credit_score=as.numeric(credit_score))

# Save the dataset
save(portland_transunion,
     file=paste0(working_data_dir, "/portland_transunion.RData"))
