#=========================================================================#
# survival_analysis.R
# 
# Follow the time-series of payments for delinquents after their due date
#
# May 19, 2025
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(wd, "/../../Dropbox/Apps/Overleaf/Water Pricing/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
# Transaction records for 2024Q4 bills
transaction <- read_csv(paste0(working_data_dir, "/servus_query/transaction.csv"))

# Full sample of everyone
load(paste0(working_data_dir, "/estimation_dataset_all.RData"))

# RCT participants only
load(paste0(working_data_dir, "/estimation_dataset.RData"))


#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
# Only choose regular bills (not final, rebill, or first)
estimation_dataset_all <- estimation_dataset_all %>%
  filter(!is_rebill,
         !first_bill) %>%
  mutate(fa=linc_discount<0) %>%
  select(-income_quartile) %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(lead_account_status=lead(account_status)) %>%
  ungroup() %>%
  filter(account_status!="FINAL",
         (lead_account_status!="FINAL" | is.na(lead_account_status))) %>%
  select(-lead_account_status)

estimation_dataset <- estimation_dataset %>%
  filter(is.na(exit_reason),
         account_status!="FINAL",
         !is_rebill,
         !first_bill) %>%
  select(-income_quartile)

# Only choose those who received the "next bill"
# Only choose those who got "full service" (water, sewer, and stormwater)
# Only choose those who are not on a monthly installments
threshold_data_all <- estimation_dataset_all %>%
  group_by(id) %>%
  filter(any(t==1)) %>%
  ungroup() %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date)) %>%
  ungroup() %>%
  filter(service_water, service_sewer, service_storm,
         bill_date>="2024-12-12", bill_date<="2025-03-14",
         B_t>0, lag_w_t>0, D_t==0,
         !id %in% estimation_dataset$id,
         !monthly_payment) %>%
  mutate(bill=O_t)

threshold_data_rct <- estimation_dataset %>%
  group_by(id) %>%
  filter(any(t==1)) %>%
  ungroup() %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date)) %>%
  ungroup() %>%
  filter(service_water, service_sewer, service_storm,
         t==0, B_t>0, lag_w_t>0,
         D_t==0, !monthly_payment, is.na(payment_plan)) %>%
  mutate(bill=O_t-D_t)


#---------+---------+---------+---------+---------+---------+
# Survival Analysis - No Groups
#---------+---------+---------+---------+---------+---------+
survival_analysis <- function(df, type) {
  # Choose those who have not paid by the due date
  transaction_survival <- df %>%
    group_by(account_number) %>%
    filter(any(grepl("LATE", transaction_type))) %>%
    ungroup()
  
  # Find when they paid off their bill
  hazard_payment <- transaction_survival %>%
    mutate(transaction_amount=round(as.numeric(amount), 2)) %>%
    filter(transaction_category!="fees") %>%
    group_by(account_number, transaction_date) %>%
    summarise(transaction_amount=sum(transaction_amount, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(account_number, transaction_date, transaction_amount) %>%
    group_by(account_number) %>%
    mutate(cumulative_paid=cumsum(transaction_amount),
           cumulative_paid=round(cumulative_paid, 2)) %>%
    ungroup() %>%
    left_join(transaction_survival %>% 
                select(account_number, initial_amount=bill, bill_date) %>%
                distinct(),
              by="account_number")
  
  hazard_payment <- hazard_payment %>%
    filter(initial_amount>0) %>%
    mutate(initial_unpaid=initial_amount+cumulative_paid,
           initial_unpaid=round(initial_unpaid, 2)) %>%
    group_by(account_number) %>%
    summarise(bill_date=first(bill_date),
              first_payment_date=min(transaction_date[initial_unpaid<=0])) %>%
    ungroup()
  
  # Subtract 20 days from the days it took them to pay off
  hazard_payment <- hazard_payment %>%
    mutate(first_payment_date=if_else(is.infinite(as.numeric(first_payment_date)), 
                                      as.Date("2028-05-15"), 
                                      first_payment_date),
           first_payment_days=as.numeric(difftime(first_payment_date, 
                                                  bill_date, 
                                                  units="days"))) %>%
    filter(first_payment_days>20) %>%
    mutate(first_payment_days=round(first_payment_days-20, 0))
  
  n_risk <- nrow(hazard_payment)
  
  # Find (cumulative) proportion of those who paid each day
  hazard_payment_summary <- hazard_payment %>%
    group_by(first_payment_days) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(first_payment_days) %>%
    mutate(cumulative_n=cumsum(n),
           proportion=n/(n_risk-cumulative_n),
           cumulative_proportion=cumulative_n/n_risk) %>%
    filter(first_payment_days>0, first_payment_days<60) %>%
    rowwise() %>%
    mutate(y_lower=binom_ci(proportion, (n_risk-cumulative_n))[1],
           y_upper=binom_ci(proportion, (n_risk-cumulative_n))[2],
           y_lower_cumulative=binom_ci(cumulative_proportion, n_risk)[1],
           y_upper_cumulative=binom_ci(cumulative_proportion, n_risk)[2]) %>%
    ungroup() %>%
    bind_rows(data.frame(first_payment_days=0, 
                         n=0, 
                         cumulative_n=0, 
                         proportion=0,
                         cumulative_proportion=0))
  
  gg <- ggplot(hazard_payment_summary, 
               aes(x=first_payment_days, y=cumulative_proportion)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=y_lower_cumulative, ymax=y_upper_cumulative),
                  width=0.2, size=0.75, alpha=0.7) +
    labs(x="Days Unpaid After Due Date",
         y="Cumulative Proportion of Accounts") +
    fte_theme() +
    scale_x_continuous(breaks=seq(0, 90, 5),
                       minor_breaks=seq(0, 90, 1)) +
    scale_y_continuous(limits=c(0, 1),
                       breaks=seq(0, 1, 0.1)) +
    geom_vline(xintercept=7, colour="darkred") +
    annotate("label", x=7, y=0.2, label="Late Fee (1%)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=14, colour="darkred") +
    annotate("label", x=14, y=0.3, label="Late Fee ($5)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=21, colour="darkred") +
    annotate("label", x=21, y=0.5, label="Late Fee ($14)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=28, colour="darkred") +
    annotate("label", x=28, y=0.75, label="Phone Call",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=35, colour="darkred") +
    annotate("label", x=35, y=0.9, label="Late Fee ($120)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=42, colour="darkred") +
    annotate("label", x=42, y=0.95, label="Shut-Off",
             colour="darkred", size=5, family="serif") +
    annotate("label", x=57, y=0.05, label=paste0("n=", n_risk),
             colour="black", size=5, family="serif")
  
  if (type=="all") {
    gg <- gg + labs(title="Hazard Rate of Paying Off Bill")
  } else {
    gg <- gg + labs(title="Hazard Rate of Paying Off Bill (RCT)")
  }
  
  ggsave(gg,
         filename=paste0(output_dir, "/figures/rd_", type, "_hazard.png"),
         width=12, height=8)
  
  gg <- ggplot(hazard_payment_summary, 
               aes(x=first_payment_days, y=proportion)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=y_lower, ymax=y_upper),
                  width=0.2, size=0.75, alpha=0.7) +
    labs(x="Days Unpaid After Due Date",
         y="Proportion of Accounts") +
    fte_theme() +
    scale_x_continuous(breaks=seq(0, 90, 5),
                       minor_breaks=seq(0, 90, 1)) +
    scale_y_continuous(limits=c(0, 0.45),
                       breaks=seq(0, 0.45, 0.05)) +
    geom_vline(xintercept=7, colour="darkred") +
    annotate("label", x=7, y=0.05, label="Late Fee (1%)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=14, colour="darkred") +
    annotate("label", x=14, y=0.075, label="Late Fee ($5)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=21, colour="darkred") +
    annotate("label", x=21, y=0.15, label="Late Fee ($14)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=28, colour="darkred") +
    annotate("label", x=28, y=0.3, label="Phone Call",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=35, colour="darkred") +
    annotate("label", x=35, y=0.45, label="Late Fee ($120)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=42, colour="darkred") +
    annotate("label", x=42, y=0.15, label="Shut-Off",
             colour="darkred", size=5,family="serif") +
    annotate("label", x=57, y=0.25, label=paste0("n=", n_risk),
             colour="black", size=5, family="serif")
  
  if (type=="all") {
    gg <- gg + labs(title="Hazard Rate of Paying Off Bill")
  } else {
    gg <- gg + labs(title="Hazard Rate of Paying Off Bill (RCT)")
  }
  
  ggsave(gg,
         filename=paste0(output_dir, "/figures/rd_", type, "_hazard_day.png"),
         width=12, height=8)
  
  return(gg)
}

transaction_all <- transaction %>%
  left_join(threshold_data_all %>%
              select(id, bill, bill_date, next_bill_date),
            by=c("account_number"="id")) %>%
  filter(account_number %in% threshold_data_all$id,
         transaction_date>=bill_date,
         transaction_date<next_bill_date,
         !transaction_category %in% c("bill",
                                      "cleanriver_discount", 
                                      "linc_discount", 
                                      "rct_discount"),
         bill>=292.08)

gg <- survival_analysis(transaction_all, "all")
gg

transaction_rct <- transaction %>%
  left_join(threshold_data_rct %>%
              select(id, bill, bill_date, next_bill_date),
            by=c("account_number"="id")) %>%
  filter(account_number %in% threshold_data_rct$id,
         transaction_date>=bill_date,
         transaction_date<next_bill_date,
         !transaction_category %in% c("bill",
                                      "cleanriver_discount", 
                                      "linc_discount", 
                                      "rct_discount"),
         bill>=278.22)

gg <- survival_analysis(transaction_rct, "rct")
gg


#---------+---------+---------+---------+---------+---------+
# Survival Analysis - Group by Threshold
#---------+---------+---------+---------+---------+---------+
survival_analysis_group <- function(df, type, threshold) {
  # Choose those who have not paid by the due date
  transaction_survival <- df %>%
    group_by(account_number) %>%
    filter(any(grepl("LATE", transaction_type))) %>%
    ungroup()
  
  # Find when they paid off their bill
  hazard_payment <- transaction_survival %>%
    mutate(transaction_amount=round(as.numeric(amount), 2),
           above_threshold=bill>=threshold) %>%
    filter(transaction_category!="fees") %>%
    group_by(account_number, transaction_date, above_threshold) %>%
    summarise(transaction_amount=sum(transaction_amount, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(above_threshold, account_number, transaction_date, transaction_amount) %>%
    group_by(account_number, above_threshold) %>%
    mutate(cumulative_paid=cumsum(transaction_amount),
           cumulative_paid=round(cumulative_paid, 2)) %>%
    ungroup() %>%
    left_join(transaction_survival %>% 
                select(account_number, initial_amount=bill, bill_date) %>%
                distinct(),
              by="account_number")
  
  hazard_payment <- hazard_payment %>%
    filter(initial_amount>0) %>%
    mutate(initial_unpaid=initial_amount+cumulative_paid,
           initial_unpaid=round(initial_unpaid, 2)) %>%
    group_by(account_number, above_threshold) %>%
    summarise(bill_date=first(bill_date),
              first_payment_date=min(transaction_date[initial_unpaid<=0])) %>%
    ungroup()
  
  # Subtract 20 days from the days it took them to pay off
  hazard_payment <- hazard_payment %>%
    mutate(first_payment_date=if_else(is.infinite(as.numeric(first_payment_date)), 
                                      as.Date("2028-05-15"), 
                                      first_payment_date),
           first_payment_days=as.numeric(difftime(first_payment_date, 
                                                  bill_date, 
                                                  units="days"))) %>%
    filter(first_payment_days>20) %>%
    mutate(first_payment_days=round(first_payment_days-20, 0))
  
  n_all <- nrow(hazard_payment)
  
  # Find (cumulative) proportion of those who paid each day
  hazard_payment_summary <- hazard_payment %>%
    group_by(above_threshold) %>%
    mutate(n_risk=n()) %>%
    ungroup() %>%
    group_by(first_payment_days, above_threshold) %>%
    summarise(n=n(), n_risk=mean(n_risk, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(above_threshold, first_payment_days) %>%
    group_by(above_threshold) %>%
    mutate(cumulative_n=cumsum(n),
           proportion=n/(n_risk-cumulative_n+n),
           cumulative_proportion=cumulative_n/n_risk) %>%
    ungroup() %>%
    filter(first_payment_days>0, first_payment_days<60) %>%
    rowwise() %>%
    mutate(y_lower=binom_ci(proportion, (n_risk-cumulative_n))[1],
           y_upper=binom_ci(proportion, (n_risk-cumulative_n))[2],
           y_lower_cumulative=binom_ci(cumulative_proportion, n_risk)[1],
           y_upper_cumulative=binom_ci(cumulative_proportion, n_risk)[2]) %>%
    ungroup() %>%
    bind_rows(data.frame(first_payment_days=rep(0, 2), 
                         n=rep(0, 2), 
                         cumulative_n=rep(0, 2), 
                         proportion=rep(0, 2),
                         cumulative_proportion=rep(0, 2),
                         above_threshold=c(TRUE, FALSE))) %>%
    mutate(above_threshold=ifelse(above_threshold, "Above Threshold", "Below Threshold"))
  
  gg <- ggplot(hazard_payment_summary, 
               aes(x=first_payment_days, y=cumulative_proportion,
                   group=above_threshold, colour=above_threshold)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=y_lower_cumulative, ymax=y_upper_cumulative),
                  width=0.2, size=0.75, alpha=0.7) +
    labs(colour="",
         x="Days Unpaid After Due Date",
         y="Cumulative Proportion of Accounts") +
    fte_theme() +
    scale_x_continuous(breaks=seq(0, 90, 5),
                       minor_breaks=seq(0, 90, 1)) +
    scale_y_continuous(limits=c(0, 1),
                       breaks=seq(0, 1, 0.1)) +
    scale_colour_manual(values=colours_set_contrast) +
    theme(plot.title=element_text(size=24, hjust=0.5),
          plot.subtitle=element_text(size=18, hjust=0.5),
          text=element_text(size=18),
          axis.text=element_text(size=18),
          axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          legend.title=element_text(size=18),
          legend.text=element_text(size=18),
          legend.position="bottom") +
    geom_vline(xintercept=7, colour="darkred") +
    annotate("label", x=7, y=0.2, label="Late Fee (1%)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=14, colour="darkred") +
    annotate("label", x=14, y=0.35, label="Late Fee ($5)",
             colour="darkred", size=5, family="serif")
  
  if (type=="all") {
    gg <- gg +
      geom_vline(xintercept=21, colour=colours_set_contrast[1]) +
      annotate("label", x=21, y=0.6, label="Late Fee ($14)",
               colour=colours_set_contrast[1], size=5, family="serif") +
      geom_vline(xintercept=28, colour=colours_set_contrast[1]) +
      annotate("label", x=28, y=0.8, label="Phone Call",
               colour=colours_set_contrast[1], size=5, family="serif") +
      labs(title="Hazard Rate of Paying Off Bill")
  } else {
    gg <- gg +
      geom_vline(xintercept=21, colour="darkred") +
      annotate("label", x=21, y=0.6, label="Late Fee ($14)",
               colour="darkred", size=5, family="serif") +
      geom_vline(xintercept=28, colour="darkred") +
      annotate("label", x=28, y=0.8, label="Phone Call",
               colour="darkred", size=5, family="serif") +
      labs(title="Hazard Rate of Paying Off Bill (RCT)")
  }
  
  gg <- gg +
    geom_vline(xintercept=35, colour=colours_set_contrast[1]) +
    annotate("label", x=35, y=0.95, label="Late Fee ($120)",
             colour=colours_set_contrast[1], size=5, family="serif") +
    geom_vline(xintercept=42, colour=colours_set_contrast[1]) +
    annotate("label", x=42, y=1, label="Shut-Off",
             colour=colours_set_contrast[1], size=5, family="serif") +
    annotate("label", x=57, y=0.05, label=paste0("n=", n_all),
             colour="black", size=5, family="serif")
  
  ggsave(gg,
         filename=paste0(output_dir, "/figures/rd_", type, "_hazard_group.png"),
         width=12, height=8)
  
  gg <- ggplot(hazard_payment_summary, 
               aes(x=first_payment_days, y=proportion, 
                   group=above_threshold, colour=above_threshold)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=y_lower, ymax=y_upper),
                  width=0.2, size=0.75, alpha=0.5) +
    labs(colour="",
         x="Days Unpaid After Due Date",
         y="Proportion of Accounts") +
    fte_theme() +
    scale_x_continuous(breaks=seq(0, 90, 5),
                       minor_breaks=seq(0, 90, 1)) +
    scale_y_continuous(limits=c(0, 0.4),
                       breaks=seq(0, 0.4, 0.05),
                       minor_breaks=seq(0, 0.4, 0.01)) +
    scale_colour_manual(values=colours_set_contrast) +
    theme(plot.title=element_text(size=24, hjust=0.5),
          plot.subtitle=element_text(size=18, hjust=0.5),
          text=element_text(size=18),
          axis.text=element_text(size=18),
          axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          legend.title=element_text(size=18),
          legend.text=element_text(size=18),
          legend.position="bottom") +
    geom_vline(xintercept=7, colour="darkred") +
    annotate("label", x=7, y=0.1, label="Late Fee (1%)",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=14, colour="darkred") +
    annotate("label", x=14, y=0.125, label="Late Fee ($5)",
             colour="darkred", size=5, family="serif")
  
  if (type=="all") {
    gg <- gg +
      geom_vline(xintercept=21, colour=colours_set_contrast[1]) +
      annotate("label", x=21, y=0.175, label="Late Fee ($14)",
               colour=colours_set_contrast[1], size=5, family="serif") +
      geom_vline(xintercept=28, colour=colours_set_contrast[1]) +
      annotate("label", x=28, y=0.3, label="Phone Call",
               colour=colours_set_contrast[1], size=5, family="serif") +
      labs(title="Hazard Rate of Paying Off Bill")
  } else {
    gg <- gg +
      geom_vline(xintercept=21, colour="darkred") +
      annotate("label", x=21, y=0.175, label="Late Fee ($14)",
               colour="darkred", size=5, family="serif") +
      geom_vline(xintercept=28, colour="darkred") +
      annotate("label", x=28, y=0.3, label="Phone Call",
               colour="darkred", size=5, family="serif") +
      labs(title="Hazard Rate of Paying Off Bill (RCT)")
  }
  
  gg <- gg + geom_vline(xintercept=35, colour=colours_set_contrast[1]) +
    annotate("label", x=35, y=0.25, label="Late Fee ($120)",
             colour=colours_set_contrast[1], size=5, family="serif") +
    geom_vline(xintercept=42, colour=colours_set_contrast[1]) +
    annotate("label", x=42, y=0.2, label="Shut-Off",
             colour=colours_set_contrast[1], size=5, family="serif") +
    annotate("label", x=57, y=0.25, label=paste0("n=", n_all),
             colour="black", size=5, family="serif")
  
  ggsave(gg,
         filename=paste0(output_dir, "/figures/rd_", type, "_hazard_group_day.png"),
         width=12, height=8)
  
  return(gg)
}

# Only look at those who were in the RD sample
transaction_all_control <- transaction %>%
  left_join(threshold_data_all %>%
              select(id, bill, bill_date, next_bill_date),
            by=c("account_number"="id")) %>%
  filter(account_number %in% threshold_data_all$id,
         transaction_date>=bill_date,
         transaction_date<next_bill_date,
         bill>292.08-20, bill<292.08+20)

transaction_all_control <- transaction_all_control %>%
  filter(!transaction_category %in% c("bill",
                                      "cleanriver_discount", 
                                      "linc_discount", 
                                      "rct_discount"))

# If you did not make any payment, you do not show up in the data
no_payment_id <- threshold_data_all %>%
  filter(bill>292.08-20, bill<292.08+20) %>%
  filter(!id %in% (transaction_all_control %>%
                     filter(transaction_category!="fees"))$account_number) %>%
  select(id, bill, bill_date, next_bill_date) %>%
  distinct()

transaction_all_control <- transaction_all_control %>%
  bind_rows(no_payment_id %>%
              mutate(transaction_date=next_bill_date,
                     transaction_type="LATE NO PAYMENT",
                     transaction_category="payment",
                     amount=0,
                     transaction_amount=0,
                     transaction_amount=round(as.numeric(amount), 2)) %>%
              select(account_number=id, bill, transaction_date, transaction_type, 
                     transaction_category, amount, transaction_amount))

gg <- survival_analysis_group(transaction_all_control, "all", 292.08)
gg

# Only look at those who were in the RCT sample
transaction_rct_control <- transaction %>%
  left_join(threshold_data_rct %>%
              select(id, bill, bill_date, next_bill_date),
            by=c("account_number"="id")) %>%
  filter(account_number %in% threshold_data_rct$id,
         transaction_date>=bill_date,
         transaction_date<next_bill_date,
         bill>278.22-20, bill<278.22+20)

transaction_rct_control <- transaction_rct_control %>%
  filter(!transaction_category %in% c("bill",
                                      "cleanriver_discount", 
                                      "linc_discount", 
                                      "rct_discount"))

# If you did not make any payment, you do not show up in the data
no_payment_id <- threshold_data_rct %>%
  filter(bill>278.22-20, bill<278.22+20) %>%
  filter(!id %in% (transaction_rct_control %>%
                     filter(transaction_category!="fees"))$account_number) %>%
  select(id, bill, bill_date, next_bill_date) %>%
  distinct()

transaction_rct_control <- transaction_rct_control %>%
  bind_rows(no_payment_id %>%
              mutate(transaction_date=next_bill_date,
                     transaction_type="LATE NO PAYMENT",
                     transaction_category="payment",
                     amount=0,
                     transaction_amount=0,
                     transaction_amount=round(as.numeric(amount), 2)) %>%
              select(account_number=id, bill, transaction_date, transaction_type, 
                     transaction_category, amount, transaction_amount))

gg <- survival_analysis_group(transaction_rct_control, "rct", 278.22)
gg
