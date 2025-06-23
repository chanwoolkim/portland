load(paste0(working_data_dir, "/estimation_dataset.RData"))
rct_subjects <- read_csv(paste0(working_data_dir, "/portland_rct_subject.csv"))
rct_additional <- read_csv(paste0(working_data_dir, "/portland_rct_additional.csv"))
exclusion_accounts <- read_xlsx(paste0(auxiliary_data_dir,
                                       "/SDP RCT 1 Exclusions Anonymized 10112024 .xlsx"))

# RCT related numbers ####
rct_accounts <- bind_rows(rct_subjects, rct_additional) %>%
  filter(!tu_id %in% exclusion_accounts$`Tu Id`) %>%
  distinct()

# Financial assistance
rct_fa_count <- rct_accounts %>% 
  group_by(linc_tier_type) %>% 
  summarise(n=n_distinct(tu_id)) %>%
  ungroup()

rct_tier_1_count <- rct_fa_count %>%
  filter(linc_tier_type=="Tier1") %>%
  pull(n)

rct_tier_2_count <- rct_fa_count %>%
  filter(linc_tier_type=="Tier2") %>%
  pull(n)

rct_fa_count <- rct_tier_1_count+rct_tier_2_count

export_tex(prettyNum(rct_fa_count, big.mark=",", scientific=FALSE),
           "rct_fa_count")

export_tex(prettyNum(rct_tier_1_count, big.mark=",", scientific=FALSE),
           "rct_tier_1_count")

export_tex(prettyNum(rct_tier_2_count, big.mark=",", scientific=FALSE),
           "rct_tier_2_count")

rct_bills <- estimation_dataset %>%
  filter(t==0)

rct_next_bill_count <- estimation_dataset %>%
  filter(t==0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

rct_next_bill_share <- round(rct_next_bill_count/nrow(rct_bills)*100, 0)

export_tex(paste0(prettyNum(rct_next_bill_count, big.mark=",", scientific=FALSE),
                  " (", rct_next_bill_share, "\\% of RCT sample)"),
           "rct_next_bill_num")

opt_out_count <- estimation_dataset %>%
  filter(t==0, !is.na(exit_reason) | account_status=="FINAL") %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

opt_out_share <- round(opt_out_count/nrow(rct_bills)*100, 0)

export_tex(paste0(prettyNum(opt_out_count, big.mark=",", scientific=FALSE),
                  " (", opt_out_share, "\\%)"),
           "rct_opt_out_num")

rct_eligible_bill <- estimation_dataset %>%
  filter(t==0, is.na(exit_reason), account_status!="FINAL")

irregular_count <- rct_eligible_bill %>%
  filter(is_rebill | first_bill | !(service_water & service_sewer & service_storm) |
           B_t<=0 | lag_w_t<=0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

irregular_share <- round(irregular_count/nrow(rct_bills)*100, 0)

export_tex(paste0(prettyNum(irregular_count, big.mark=",", scientific=FALSE),
                  " (", irregular_share, "\\%)"),
           "rct_irregular_num")

extension_count <- rct_eligible_bill %>%
  filter(payment_plan | monthly_payment) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

extension_share <- round(extension_count/nrow(rct_bills)*100, 0)

export_tex(paste0(prettyNum(extension_count, big.mark=",", scientific=FALSE),
                  " (", extension_share, "\\%)"),
           "rct_extension_num")

fa_tier_2_count <- rct_eligible_bill %>%
  filter(fa_type=="Tier2") %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

fa_tier_2_share <- round(fa_tier_2_count/nrow(rct_bills)*100, 0)
export_tex(paste0(prettyNum(fa_tier_2_count, big.mark=",", scientific=FALSE),
                  " (", fa_tier_2_share, "\\%)"),
           "rct_fa_tier_2_num")

threshold_data_rct <- estimation_dataset %>%
  group_by(id) %>%
  filter(any(t==1)) %>%
  ungroup() %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date)) %>%
  ungroup() %>%
  filter(t==0, is.na(exit_reason), account_status!="FINAL",
         (fa_type!="Tier2" | is.na(fa_type))) %>%
  mutate(bill=O_t-D_t)

threshold_data_count <- threshold_data_rct %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

threshold_data_share <- round(threshold_data_count/nrow(rct_bills)*100, 0)

export_tex(paste0(prettyNum(threshold_data_count, big.mark=",", scientific=FALSE),
                  " (", threshold_data_share, "\\%)"),
           "rct_threshold_data_num")

export_tex(paste0(threshold_data_share, "\\%"),
           "rct_threshold_data_share")


# RCT balance table ####
estimation_dataset <- estimation_dataset %>% 
  filter(bill_date >= "2024-04-01",
         bill_date <= "2024-06-30",
         !is_rebill,
         account_status!="FINAL") %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

estimation_dataset <- estimation_dataset %>%
  mutate(delinquent=O_t+E_t>0)

balance_summary <- function(remove_severe_fa=TRUE) {
  if (remove_severe_fa) {
    balance_summary_df <- estimation_dataset %>%
      filter(fa_type!="Tier2" | is.na(fa_type))
  } else {
    balance_summary_df <- estimation_dataset
  }
  
  balance_summary_df <- full_join(
    balance_summary_df %>%
      group_by(discount_grid) %>%
      summarise(across(c("B_t", "D_t", "F_t", "O_t", "E_t",
                         "lag_w_t", "delinquent",
                         "tu_income", "credit_score",
                         "census_unemployment_rate_in_labor_force",
                         "census_percent_of_house_holds_in_poverty",
                         "census_average_house_hold_size",
                         "census_percent_of_population_includes_black",
                         "census_percent_of_population_includes_hispanic"), 
                       ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
                n=n_distinct(id)) %>%
      ungroup(),
    balance_summary_df %>%
      group_by(discount_grid) %>%
      summarise(across(c("B_t", "D_t", "F_t", "O_t", "E_t",
                         "lag_w_t", "delinquent",
                         "tu_income", "credit_score",
                         "census_unemployment_rate_in_labor_force",
                         "census_percent_of_house_holds_in_poverty",
                         "census_average_house_hold_size",
                         "census_percent_of_population_includes_black",
                         "census_percent_of_population_includes_hispanic"), 
                       ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}")) %>%
      ungroup(),
    by="discount_grid")
  
  tab <- TexRow("Discount Grid") /
    TexRow(seq(0, 80, 10), cspan=rep(2, 9), percent=TRUE, dec=0) +
    TexMidrule(list(c(1, 1), c(2, 3), c(4, 5), c(6, 7), c(8, 9), c(10, 11), c(12, 13), c(14, 15), c(16, 17), c(18, 19))) +
    TexRow(c("Category", rep(c("Mean", "SD"), 9))) +
    TexMidrule() +
    TexRow("\\textbf{Pre-Experiment Behavior}") +
    TexRow("\\quad Bill from Usage") / TexRow(c(rbind(balance_summary_df$mean_B_t,
                                                      balance_summary_df$sd_B_t)), dec=0, dollar=TRUE) +
    TexRow("\\quad Previous Unpaid Debt") / TexRow(c(rbind(balance_summary_df$mean_D_t,
                                                           balance_summary_df$sd_D_t)), dec=0, dollar=TRUE) +
    TexRow("\\quad Total Bill") / TexRow(c(rbind(balance_summary_df$mean_O_t,
                                                 balance_summary_df$sd_O_t)), dec=0, dollar=TRUE) +
    TexRow("\\quad Total Payment") / TexRow(c(rbind(-balance_summary_df$mean_E_t,
                                                    balance_summary_df$sd_E_t)), dec=0, dollar=TRUE) +
    TexRow("\\quad \\% Delinquent") / TexRow(c(rbind(balance_summary_df$mean_delinquent*100,
                                                     balance_summary_df$sd_delinquent*100)), dec=0, percent=TRUE) +
    TexRow("\\quad Water Usage (in ccf)") / TexRow(c(rbind(balance_summary_df$mean_lag_w_t,
                                                           balance_summary_df$sd_lag_w_t)), dec=0) +
    TexMidrule() +
    TexRow("\\textbf{Socioeconomic Status (TransUnion)}") +
    TexRow("\\quad Income") / TexRow(c(rbind(balance_summary_df$mean_tu_income,
                                             balance_summary_df$sd_tu_income)), dec=0, dollar=TRUE) +
    TexRow("\\quad Credit Score") / TexRow(c(rbind(balance_summary_df$mean_credit_score,
                                                   balance_summary_df$sd_credit_score)), dec=0) +
    TexMidrule() +
    TexRow("\\textbf{Demographics (Census)}") +
    TexRow("\\quad Unemployment Rate") / TexRow(c(rbind(balance_summary_df$mean_census_unemployment_rate_in_labor_force,
                                                        balance_summary_df$sd_census_unemployment_rate_in_labor_force)), dec=1, percent=TRUE) +
    TexRow("\\quad \\% Households in Poverty") / TexRow(c(rbind(balance_summary_df$mean_census_percent_of_house_holds_in_poverty,
                                                                balance_summary_df$sd_census_percent_of_house_holds_in_poverty)), dec=1, percent=TRUE) +
    TexRow("\\quad Average Household Size") / TexRow(c(rbind(balance_summary_df$mean_census_average_house_hold_size,
                                                             balance_summary_df$sd_census_average_house_hold_size)), dec=1) +
    TexRow("\\quad \\% Black") / TexRow(c(rbind(balance_summary_df$mean_census_percent_of_population_includes_black,
                                                balance_summary_df$sd_census_percent_of_population_includes_black)), dec=1, percent=TRUE) +
    TexRow("\\quad \\% Hispanic") / TexRow(c(rbind(balance_summary_df$mean_census_percent_of_population_includes_hispanic,
                                                   balance_summary_df$sd_census_percent_of_population_includes_hispanic)), dec=1, percent=TRUE) +
    TexMidrule() +
    TexRow("\\textbf{Number of Accounts}") / 
    TexRow(balance_summary_df$n, cspan=rep(2, 9), dec=0)
  
  return(tab)
}

balance_summary_without_severe_fa <- balance_summary(remove_severe_fa=TRUE)
balance_summary_with_severe_fa <- balance_summary(remove_severe_fa=FALSE)

TexSave(balance_summary_without_severe_fa, 
        paste0(output_dir, "/tables/balance_summary_without_severe_fa"),
        positions=c("l", rep("c", 18)))

TexSave(balance_summary_with_severe_fa,
        paste0(output_dir, "/tables/balance_summary_with_severe_fa"),
        positions=c("l", rep("c", 18)))
