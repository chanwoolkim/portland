# Preliminary ####
cpi <- read_csv(paste0(auxiliary_data_dir, "/CPIAUCSL.csv")) %>%
  transmute(year=year(observation_date),
            quarter=quarter(observation_date),
            cpi=CPIAUCSL/CPIAUCSL[1])

cpi_year <- cpi %>%
  group_by(year) %>%
  summarise(cpi=mean(cpi)) %>%
  ungroup()

# Revenue decomposition ####
generate_revenue_graphs <- function(covid_location,
                                    rate_change_location,
                                    breakdown_location,
                                    real_terms=TRUE,
                                    filename) {
  if (!("liens" %in% names(final_aggregate))) {
    final_aggregate$liens <- 0
  }
  
  transaction_aggregate_summary <- transaction_aggregate %>% 
    left_join(code_info %>% select(transaction_type, category),
              by="transaction_type") %>%
    filter(!grepl("COMMIT", transaction_type)) %>%
    group_by(year, quarter, category) %>%
    summarise(total_amount=sum(total_amount, na.rm=TRUE)) %>%
    ungroup() %>%
    spread(category, total_amount) %>%
    left_join(payment_plan_aggregate %>%
                mutate_all(as.numeric) %>%
                select(year, quarter, 
                       total_payment_arrange=total_amount_sum,
                       total_outstanding=total_outstanding_amount_sum,
                       total_payment_made=difference_sum,
                       total_delayed=remaining_amount_sum),
              by=c("year", "quarter")) %>%
    left_join(final_aggregate %>%
                mutate_all(as.numeric) %>%
                select(year, quarter,
                       final_discount=discount_amount,
                       final_payment=payment_amount,
                       final_bill=bill_amount,
                       final_liens=liens_amount,
                       final_leftover=remaining_amount),
              by=c("year", "quarter")) %>%
    mutate(final_discount=ifelse(is.na(final_discount), 0, final_discount),
           final_payment=ifelse(is.na(final_payment), 0, final_payment),
           final_bill=ifelse(is.na(final_bill), 0, final_bill),
           final_liens=ifelse(is.na(final_liens), 0, final_liens),
           final_leftover=ifelse(is.na(final_leftover), 0, final_leftover))
  
  if (!("liens" %in% names(transaction_aggregate_summary))) {
    transaction_aggregate_summary$liens <- 0
  }
  
  transaction_aggregate_summary <- transaction_aggregate_summary %>%
    mutate(liens=ifelse(is.na(liens), 0, liens),
           liens=-(liens+final_liens)) %>% 
    left_join(collection_aggregate %>%
                mutate_all(as.numeric) %>%
                rename(collection_amount=total_amount,
                       collection_paid=total_payment),
              by=c("year", "quarter")) %>%
    mutate(collection_amount=ifelse(is.na(collection_amount), 0, collection_amount),
           collection_paid=ifelse(is.na(collection_paid), 0, collection_paid))
  
  transaction_aggregate_summary <- na.fill(transaction_aggregate_summary, 0) %>%
    as.data.frame()
  
  transaction_aggregate_summary <- transaction_aggregate_summary %>%
    mutate(bill=bill+fees+transfer+final_bill,
           discount=-discount-linc_discount-rct_discount,
           cleanriver_discount=-cleanriver_discount,
           final_discount=-writeoff-final_discount,
           payment=-payment-final_payment-refund) %>%
    arrange(year, quarter)
  
  for (i in 1:nrow(transaction_aggregate_summary)) {
    carryover_debt <- ifelse(i==1, 
                             0, 
                             transaction_aggregate_summary$leftover_debt[i-1]-
                               transaction_aggregate_summary$final_leftover[i-1]-
                               transaction_aggregate_summary$liens[i-1])
    transaction_aggregate_summary$leftover_debt[i] <- 
      transaction_aggregate_summary$bill[i]+
      carryover_debt-
      transaction_aggregate_summary$discount[i]-
      transaction_aggregate_summary$cleanriver_discount[i]-
      transaction_aggregate_summary$final_discount[i]-
      transaction_aggregate_summary$payment[i]+
      transaction_aggregate_summary$total_outstanding[i]
  }
  
  transaction_aggregate_summary <- transaction_aggregate_summary %>% 
    mutate(payment=payment+collection_paid,
           total_delayed=total_delayed+collection_amount-collection_paid+cumsum(liens))
  
  transaction_aggregate_summary_plot <- transaction_aggregate_summary %>% 
    mutate(leftover_debt=leftover_debt/1000000,
           payment=payment/1000000,
           discount=discount/1000000,
           cleanriver_discount=cleanriver_discount/1000000,
           final_discount=final_discount/1000000,
           delayed=total_delayed/1000000,
           revenue=leftover_debt+payment+discount+cleanriver_discount+final_discount+delayed,
           accum_discount=payment+discount,
           accum_cleanriver_discount=accum_discount+cleanriver_discount,
           accum_final_discount=accum_cleanriver_discount+final_discount,
           accum_leftover_debt=accum_final_discount+leftover_debt,
           accum_delayed=accum_leftover_debt+delayed,
           date=as.Date(paste0(year, "-", quarter*3-2, "-01"))) %>%
    filter(year<2025)
  
  # Re-aggregate over years
  transaction_aggregate_summary_plot_year <- transaction_aggregate_summary_plot %>%
    select(-date) %>%
    group_by(year) %>%
    summarise_all(sum) %>%
    ungroup()
  
  # If in real terms, make everything in 2019 dollars
  if (real_terms) {
    transaction_aggregate_summary_plot <- transaction_aggregate_summary_plot %>%
      left_join(cpi, by=c("year", "quarter")) %>%
      mutate(leftover_debt=leftover_debt/cpi,
             payment=payment/cpi,
             discount=discount/cpi,
             cleanriver_discount=cleanriver_discount/cpi,
             final_discount=final_discount/cpi,
             delayed=delayed/cpi,
             revenue=revenue/cpi,
             accum_discount=accum_discount/cpi,
             accum_cleanriver_discount=accum_cleanriver_discount/cpi,
             accum_final_discount=accum_final_discount/cpi,
             accum_leftover_debt=accum_leftover_debt/cpi,
             accum_delayed=accum_delayed/cpi)
    
    transaction_aggregate_summary_plot_year <- transaction_aggregate_summary_plot_year %>%
      left_join(cpi_year, by="year") %>%
      mutate_at(vars(-year), ~.x/cpi)
  }
  
  gg <- ggplot(data=transaction_aggregate_summary_plot,
               aes(x=date, y=revenue)) +
    geom_path(colour="black", size=1) + 
    geom_path(aes(x=date, y=payment), colour=colours_set[1], size=1) +
    geom_path(aes(x=date, y=accum_discount), colour=colours_set[2], size=1) +
    geom_path(aes(x=date, y=accum_cleanriver_discount), colour=colours_set[3], size=1) +
    geom_path(aes(x=date, y=accum_final_discount), colour=colours_set[4], size=1) +
    geom_path(aes(x=date, y=accum_leftover_debt), colour=colours_set[5], size=1) +
    geom_path(aes(x=date, y=accum_delayed), colour=colours_set[6], size=1) +
    geom_ribbon(aes(ymin=0, ymax=payment, fill="Payments"), alpha=0.8) +
    geom_ribbon(aes(ymin=payment, ymax=accum_discount, fill="Discounts"), alpha=0.8) +
    geom_ribbon(aes(ymin=accum_discount, ymax=accum_cleanriver_discount, fill="Environment Discounts"), alpha=0.8) +
    geom_ribbon(aes(ymin=accum_cleanriver_discount, ymax=accum_final_discount, fill="Write-Offs"), alpha=0.8) +
    geom_ribbon(aes(ymin=accum_final_discount, ymax=accum_leftover_debt, fill="Unpaid Debt"), alpha=0.8) +
    geom_ribbon(aes(ymin=accum_leftover_debt, ymax=accum_delayed, fill="Delayed"), alpha=0.8) +
    scale_x_continuous(breaks=seq(as.Date("2019-01-01"), as.Date("2024-12-31"), by="1 year"),
                       minor_breaks=seq(as.Date("2019-01-01"), as.Date("2024-12-31"), by="3 month"),
                       labels=date_format("%YQ1")) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=5),
                       minor_breaks=seq(0, rate_change_location-5, 5)) +
    # COVID Timing
    geom_vline(xintercept=as.Date("2020-03-11"), 
               colour="darkred", linetype="dashed") +
    annotate("label", x=as.Date("2020-03-11"), y=covid_location, label="COVID",
               colour="darkred", size=4, family="serif") +
    geom_vline(xintercept=as.Date("2022-09-13"), 
               colour="darkred", linetype="dashed") +
    annotate("label", x=as.Date("2022-09-13"), y=covid_location, label="Shutoff Reinstated", 
               colour="darkred", size=4, family="serif") +
    # Rate Change
    geom_vline(xintercept=as.Date("2019-07-01"),
               colour="darkgray", linetype="dashed") +
    annotate("label", x=as.Date("2019-07-01"), y=rate_change_location, label="Rate Change\n(7.4%)", 
               colour="darkgray", size=4, family="serif") +
    geom_vline(xintercept=as.Date("2020-07-01"), 
               colour="darkgray", linetype="dashed") +
    annotate("label", x=as.Date("2020-07-01"), y=rate_change_location, label="Rate Change\n(6.5%)", 
               colour="darkgray", size=4, family="serif") +
    geom_vline(xintercept=as.Date("2021-07-01"), 
               colour="darkgray", linetype="dashed") +
    annotate("label", x=as.Date("2021-07-01"), y=rate_change_location, label="Rate Change\n(7.8%)", 
               colour="darkgray", size=4, family="serif") +
    geom_vline(xintercept=as.Date("2022-07-01"), 
               colour="darkgray", linetype="dashed") +
    annotate("label", x=as.Date("2022-07-01"), y=rate_change_location, label="Rate Change\n(7.7%)", 
               colour="darkgray", size=4, family="serif") +
    geom_vline(xintercept=as.Date("2023-07-01"), 
               colour="darkgray", linetype="dashed") +
    annotate("label", x=as.Date("2023-07-01"), y=rate_change_location, label="Rate Change\n(7.9%)", 
               colour="darkgray", size=4, family="serif") +
    geom_vline(xintercept=as.Date("2024-07-01"), 
               colour="darkgray", linetype="dashed") +
    annotate("label", x=as.Date("2024-07-01"), y=rate_change_location, label="Rate Change\n(5.9%)", 
               colour="darkgray", size=4, family="serif") +
    # Breakdown in 2019
    geom_label(data=transaction_aggregate_summary_plot_year[1, ],
               aes(x=as.Date("2019-10-15"), y=breakdown_location,
                   label=paste0("2019 Quarterly Average\nDelayed: ", round(delayed/revenue*100, 1), "%",
                                " ($", round(delayed/4, 1), "M)",
                                "\nUnpaid Debt: ", round(leftover_debt/revenue*100, 1), "%",
                                " ($", round(leftover_debt/4, 1), "M)",
                                "\nDiscounts: ", round(discount/revenue*100, 1), "%",
                                " ($", round(discount/4, 1), "M)",
                                "\nEnvironment Discounts: ", round(cleanriver_discount/revenue*100, 1), "%",
                                " ($", round(cleanriver_discount/4, 1), "M)",
                                "\nWrite-Offs: ", round(final_discount/revenue*100, 1), "%",
                                " ($", round(final_discount/4, 1), "M)",
                                "\nPayments: ", round(payment/revenue*100, 1), "%",
                                " ($", round(payment/4, 1), "M)")),
               colour="black", size=4, lineheight=1, hjust=0.5, family="serif") +
    # Breakdown in 2021
    geom_label(data=transaction_aggregate_summary_plot_year[3,],
               aes(x=as.Date("2021-06-01"), y=breakdown_location,
                   label=paste0("2021 Quarterly Average\nDelayed: ", round(delayed/revenue*100, 1), "%",
                                " ($", round(delayed/4, 1), "M)",
                                "\nUnpaid Debt: ", round(leftover_debt/revenue*100, 1), "%",
                                " ($", round(leftover_debt/4, 1), "M)",
                                "\nDiscounts: ", round(discount/revenue*100, 1), "%",
                                " ($", round(discount/4, 1), "M)",
                                "\nEnvironment Discounts: ", round(cleanriver_discount/revenue*100, 1), "%",
                                " ($", round(cleanriver_discount/4, 1), "M)",
                                "\nWrite-Offs: ", round(final_discount/revenue*100, 1), "%",
                                " ($", round(final_discount/4, 1), "M)",
                                "\nPayments: ", round(payment/revenue*100, 1), "%",
                                " ($", round(payment/4, 1), "M)")),
               colour="black", size=4, lineheight=1, hjust=0.5, family="serif") +
    # Breakdown in 2024
    geom_label(data=transaction_aggregate_summary_plot_year[6,],
               aes(x=as.Date("2024-01-01"), y=breakdown_location,
                   label=paste0("2024 Quarterly Average\nDelayed: ", round(delayed/revenue*100, 1), "%",
                                " ($", round(delayed/4, 1), "M)",
                                "\nUnpaid Debt: ", round(leftover_debt/revenue*100, 1), "%",
                                " ($", round(leftover_debt/4, 1), "M)",
                                "\nDiscounts: ", round(discount/revenue*100, 1), "%",
                                " ($", round(discount/4, 1), "M)",
                                "\nEnvironment Discounts: ", round(cleanriver_discount/revenue*100, 1), "%",
                                " ($", round(cleanriver_discount/4, 1), "M)",
                                "\nWrite-Offs: ", round(final_discount/revenue*100, 1), "%",
                                " ($", round(final_discount/4, 1), "M)",
                                "\nPayments: ", round(payment/revenue*100, 1), "%",
                                " ($", round(payment/4, 1), "M)")),
               colour="black", size=4, lineheight=1, hjust=0.5, family="serif") +
    labs(x="Date (Year/Quarter)",
         y="Revnue ($ millions)",
         title="Breakdown of Total Billed Amount") +
    fte_theme() +
    theme(plot.title=element_text(size=24, hjust=0.5),
          plot.subtitle=element_text(size=18, hjust=0.5),
          text=element_text(size=18),
          axis.text=element_text(size=18),
          axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          legend.title=element_text(size=18),
          legend.text=element_text(size=18),
          legend.position="bottom") +
    scale_fill_manual(values=c("Payments"=colours_set[1],
                               "Discounts"=colours_set[2],
                               "Environment Discounts"=colours_set[3],
                               "Write-Offs"=colours_set[4],
                               "Unpaid Debt"=colours_set[5],
                               "Delayed"=colours_set[6]),
                      breaks=c("Payments", "Discounts", "Environment Discounts", "Write-Offs", "Unpaid Debt", "Delayed"),
                      labels=c("Payments", "Discounts", "Environment Discounts", "Write-Offs", "Unpaid Debt", "Delayed"),
                      name="Category")
  
  gg
  ggsave(gg,
         file=paste0(output_dir, "/figures/", filename, ".png"),
         width=12, height=8)
  
  return(transaction_aggregate_summary)
}

code_info <- read_csv(paste0(working_data_dir, "/servus_query/code_info.csv"))
account_count <- read_csv(paste0(working_data_dir, "/servus_query/account_count.csv"))
transaction_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/transaction_aggregate.csv"))
payment_plan_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/payment_plan_aggregate.csv"))
collection_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/collection_aggregate.csv"))
final_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/final_aggregate.csv"))

all_summary <- generate_revenue_graphs(-5, 90, 12.5, real_terms=FALSE, "bill_breakdown_all")
all_real_summary <- generate_revenue_graphs(-5, 80, 12.5, real_terms=TRUE, "bill_breakdown_real_all")

account_count <- read_csv(paste0(working_data_dir, "/servus_query/smart_discount/account_count.csv"))
transaction_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/smart_discount/transaction_aggregate.csv"))
payment_plan_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/smart_discount/payment_plan_aggregate.csv"))
collection_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/smart_discount/collection_aggregate.csv"))
final_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/smart_discount/final_aggregate.csv"))

sdp_summary <- generate_revenue_graphs(-1.5, 15, 2.15, real_terms=FALSE, "bill_breakdown_sdp")

for (type in c("credit_quartile", "income_quartile")) {
  for (quartile in c(paste0("_", 1:4))) {
    account_count <- read_csv(paste0(working_data_dir, "/servus_query/", 
                                     type, "/account_count", quartile, ".csv"))
    transaction_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/", 
                                             type, "/transaction_aggregate", quartile, ".csv"))
    payment_plan_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/", 
                                              type, "/payment_plan_aggregate", quartile, ".csv"))
    collection_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/", 
                                            type, "/collection_aggregate", quartile, ".csv"))
    final_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/", 
                                       type, "/final_aggregate", quartile, ".csv"))
    
    filename <- paste0("bill_breakdown_", type, quartile)
    assign(paste0(type, quartile, "_summary"),
           generate_revenue_graphs(-1.5, 27.5, 3.75, real_terms=FALSE, filename))
  }
}
