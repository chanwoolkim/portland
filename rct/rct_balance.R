load(paste0(working_data_dir, "/estimation_dataset.RData"))

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
                         "income", "credit_score",
                         "unemployment_rate_in_labor_force",
                         "percent_of_house_holds_in_poverty",
                         "average_house_hold_size",
                         "percent_of_population_includes_black",
                         "percent_of_population_includes_hispanic"), 
                       ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
                n=n_distinct(id)) %>%
      ungroup(),
    balance_summary_df %>%
      group_by(discount_grid) %>%
      summarise(across(c("B_t", "D_t", "F_t", "O_t", "E_t",
                         "lag_w_t", "delinquent",
                         "income", "credit_score",
                         "unemployment_rate_in_labor_force",
                         "percent_of_house_holds_in_poverty",
                         "average_house_hold_size",
                         "percent_of_population_includes_black",
                         "percent_of_population_includes_hispanic"), 
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
    TexRow("\\quad Income") / TexRow(c(rbind(balance_summary_df$mean_income,
                                             balance_summary_df$sd_income)), dec=0, dollar=TRUE) +
    TexRow("\\quad Credit Score") / TexRow(c(rbind(balance_summary_df$mean_credit_score,
                                                   balance_summary_df$sd_credit_score)), dec=0) +
    TexMidrule() +
    TexRow("\\textbf{Demographics (Census)}") +
    TexRow("\\quad Unemployment Rate") / TexRow(c(rbind(balance_summary_df$mean_unemployment_rate_in_labor_force,
                                                        balance_summary_df$sd_unemployment_rate_in_labor_force)), dec=1, percent=TRUE) +
    TexRow("\\quad \\% Households in Poverty") / TexRow(c(rbind(balance_summary_df$mean_percent_of_house_holds_in_poverty,
                                                                balance_summary_df$sd_percent_of_house_holds_in_poverty)), dec=1, percent=TRUE) +
    TexRow("\\quad Average Household Size") / TexRow(c(rbind(balance_summary_df$mean_average_house_hold_size,
                                                             balance_summary_df$sd_average_house_hold_size)), dec=1) +
    TexRow("\\quad \\% Black") / TexRow(c(rbind(balance_summary_df$mean_percent_of_population_includes_black,
                                                balance_summary_df$sd_percent_of_population_includes_black)), dec=1, percent=TRUE) +
    TexRow("\\quad \\% Hispanic") / TexRow(c(rbind(balance_summary_df$mean_percent_of_population_includes_hispanic,
                                                   balance_summary_df$sd_percent_of_population_includes_hispanic)), dec=1, percent=TRUE) +
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
