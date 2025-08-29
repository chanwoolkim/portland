#=========================================================================#
# threshold_effect.R
# 
# Analysis on penalties (late fees and shut-offs)
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
# Full sample of everyone
load(paste0(working_data_dir, "/servus/analysis/estimation_dataset_all.RData"))

# RCT participants only
load(paste0(working_data_dir, "/servus/analysis/estimation_dataset.RData"))


#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
# Extract income quartile from the full sample
estimation_dataset_all <- estimation_dataset_all %>%
  mutate(income=aspire_income) %>%
  select(-contains("aspire_")) %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  fill(income, .direction="downup") %>%
  ungroup()

estimation_dataset <- estimation_dataset %>%
  mutate(income=aspire_income) %>%
  select(-contains("aspire_")) %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  fill(income, .direction="downup") %>%
  ungroup()

income_quartile <- estimation_dataset_all %>%
  filter(t==0) %>%
  select(id, income, bill_date) %>%
  arrange(id, bill_date) %>%
  distinct(id, income) %>%
  mutate(income_quartile=ntile(income, 4)) %>%
  select(id, income_quartile)

# Only choose regular bills (not final, rebill, or first)
estimation_dataset_all <- estimation_dataset_all %>%
  filter(!is_rebill,
         !first_bill) %>%
  mutate(fa=linc_discount<0) %>%
  left_join(income_quartile, by=c("id")) %>%
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
  left_join(income_quartile, by=c("id"))

# Only choose those who received the "next bill"
# Only choose those who got "full service" (water, sewer, and stormwater)
# Only choose those who are not on a monthly installments
threshold_data_all <- estimation_dataset_all %>%
  group_by(id) %>%
  filter(any(t==1)) %>%
  ungroup() %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t),
         next_bill_date=lead(bill_date)) %>%
  ungroup() %>%
  filter(bill_date>="2024-12-12", bill_date<="2025-03-14",
         !id %in% estimation_dataset$id)

threshold_data_count <- threshold_data_all %>%
  filter(D_t==0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

threshold_data_payment_plan_count <- threshold_data_all %>%
  filter(payment_plan, D_t==0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

threshold_data_monthly_payment_count <- threshold_data_all %>%
  filter(monthly_payment, D_t==0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

export_tex(paste0(prettyNum(threshold_data_payment_plan_count, 
                            big.mark=",", scientific=FALSE),
                  " (", round(threshold_data_payment_plan_count/
                                threshold_data_count*100, 0), "\\%)"),
           "threshold_data_payment_plan_num")

export_tex(paste0(prettyNum(threshold_data_monthly_payment_count, 
                            big.mark=",", scientific=FALSE),
                  " (", round(threshold_data_monthly_payment_count/
                                threshold_data_count*100, 0), "\\%)"),
           "threshold_data_monthly_payment_num")

threshold_data_all <- threshold_data_all %>%
  filter(is.na(payment_plan), !monthly_payment) %>%
  mutate(bill=O_t)

threshold_data_near_115 <- threshold_data_all %>%
  filter(D_t==0, bill>113.87-20, bill<=113.87+20)

threshold_data_near_115_count <- threshold_data_near_115 %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

threshold_data_near_115_fa_count <- threshold_data_near_115 %>%
  filter(linc_discount<0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

export_tex(paste0("(", round(threshold_data_near_115_fa_count/
                               threshold_data_near_115_count*100, 0), "\\%)"),
           "threshold_data_near_115_fa_num")

threshold_data_fa_count <- threshold_data_all %>%
  filter(D_t==0, linc_discount<0) %>%
  summarise(n=n_distinct(id)) %>%
  pull(n)

export_tex(paste0("(", round(threshold_data_fa_count/
                               threshold_data_count*100, 1), "\\%)"),
           "threshold_data_fa_num")

threshold_data_all_debt <- threshold_data_all %>%
  filter(D_t>0)

threshold_data_all <- threshold_data_all %>%
  filter(D_t==0)

# Pre-Period
threshold_data_pre <- estimation_dataset_all %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date),
         B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t)) %>%
  ungroup() %>%
  filter(bill_date>="2024-01-01", bill_date<="2024-12-31",
         !monthly_payment) %>%
  mutate(bill=O_t)

# COVID-Period
threshold_data_covid <- estimation_dataset_all %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date),
         B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t)) %>%
  ungroup() %>%
  filter(bill_date>="2021-01-01", bill_date<="2021-12-31",
         !monthly_payment) %>%
  mutate(bill=O_t)

# Pre-COVID-Period
threshold_data_precovid <- estimation_dataset_all %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date),
         B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t)) %>%
  ungroup() %>%
  filter(bill_date>="2019-01-01", bill_date<="2019-12-31",
         !monthly_payment) %>%
  mutate(bill=O_t)

# RCT
threshold_data_rct <- estimation_dataset %>%
  group_by(id) %>%
  filter(any(t==1)) %>%
  ungroup() %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date),
         B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t)) %>%
  ungroup() %>%
  filter(t==0, 
         D_t>=0, !monthly_payment, is.na(payment_plan)) %>%
  mutate(bill=O_t-D_t)

threshold_data_rct_debt <- threshold_data_rct %>%
  filter(D_t>0)

threshold_data_rct <- threshold_data_rct %>%
  filter(D_t==0)

# Create variables for analysis
for (df in c("threshold_data_all",
             "threshold_data_all_debt",
             "threshold_data_pre",
             "threshold_data_covid",
             "threshold_data_precovid",
             "threshold_data_rct",
             "threshold_data_rct_debt")) {
  assign(df,
         get(df) %>%
           mutate(payment_plan=ifelse(is.na(payment_plan), FALSE, payment_plan),
                  crisis_voucher=crisis_voucher<0,
                  fa=linc_discount<0,
                  payshare=-E_t/bill,
                  payshare=ifelse(bill<=0, NA, payshare),
                  payshare=ifelse(payshare<0, 0, payshare),
                  payshare=ifelse(payshare>1, 1, payshare),
                  payshare=round(payshare, 2),
                  payment=-E_t,
                  delinquent=payshare<1,
                  paid_something=payshare>0,
                  paid_nothing=payshare<0.01,
                  shutoff=!is.na(shutoff_date)))
}

# Filter on income quartiles
threshold_data_all_q1 <- threshold_data_all %>%
  filter(income_quartile==1)

threshold_data_all_q4 <- threshold_data_all %>%
  filter(income_quartile==4)

threshold_data_rct_q1 <- threshold_data_rct %>%
  filter(income_quartile==1)

threshold_data_rct_q4 <- threshold_data_rct %>%
  filter(income_quartile==4)

# Filter on the RCT control group
threshold_data_rct_control <- threshold_data_rct %>%
  filter(discount_grid==0)

#---------+---------+---------+---------+---------+---------+
# RD Graphs
#---------+---------+---------+---------+---------+---------+
outcome_variables <- c("payment", "delinquent", "payshare",
                       "paid_something", "paid_nothing",
                       "shutoff",
                       "lag_w_t", "income")

outcome_labels <- c("Revenue", 
                    "Probability of Delinquency",
                    "Payment Share",
                    "Probability of Paying Something",
                    "Probability of Paying Nothing",
                    "Probability of Shut-Off",
                    "Water Use (in ccf)",
                    "Income ($ thousands)")

title_labels <- c("Revenue", 
                  "Delinquency",
                  "Payment Share",
                  "Paying Something",
                  "Paying Nothing",
                  "Shut-Off",
                  "Water Use",
                  "Income")

outcome_bw <- c(5, 10, 5, 5, 30, 10, 20, 5)

rd_analysis <- function(df, threshold, bw=20, type, subtitle) {
  threshold_round <- case_when(threshold==292.08 ~ 300,
                               threshold==278.22 ~ 300,
                               threshold==113.87 ~ 115,
                               .default=NA)
  
  x_interval <- ifelse(bw>=50, 10, 5)
  plot_width <- case_when(bw>=100 ~ 24,
                          bw>=50 ~ 18,
                          .default=12)
  x_threshold <- floor(threshold/5)*5
  
  df_analysis <- df %>%
    filter(bill>threshold-bw, bill<threshold+bw) %>%
    mutate(above_threshold=bill>=threshold)
  
  window_analysis <- data.frame()
  
  for (i in 1:length(outcome_variables)) {
    # Select the number of bins (if more than 5, just choose 5)
    n_cut <- bin_select(df_analysis, outcome_variables[i], "bill", bw, 10, threshold)
    n_cut <- case_when(bw<50 & n_cut>5 ~ 5,
                       bw>=50 & bw<100 ~ 20,
                       bw>=100 ~ 40,
                       .default=n_cut)
    
    print(paste0("Running RD analysis for type: ", type, 
                 ", Threshold: ", threshold, 
                 ", Bandwidth: ", bw, 
                 ", Outcome: ", outcome_variables[i], 
                 ", Number of Bins: ", n_cut))
    
    # Binscatter and local linear lines, then create the RD graph
    bins <- binscatter(df_analysis, 
                       y=outcome_variables[i], x="bill",
                       n.cut=n_cut, threshold=threshold)
    
    if (outcome_variables[i] %in% c("delinquent", "payshare", 
                                    "paid_something", "paid_nothing",
                                    "shutoff")) {
      bins$df_bin <- bins$df_bin %>%
        rowwise() %>%
        mutate(y_lower=binom_ci(y, n)[1],
               y_upper=binom_ci(y, n)[2]) %>%
        ungroup()
    }
    
    linf <- loc_lin(df_analysis %>% mutate(x=bill),
                    y=outcome_variables[i], 
                    gran=15, bw=outcome_bw[i],
                    range=c(threshold-bw, threshold+bw), 
                    cutoff=threshold)
    
    n <- nrow(df_analysis)
    
    assign(paste0("gg_", outcome_variables[i]),
           ggplot(bins$df_bin, aes(x=x, y=y)) +
             geom_point(color="navy", size=3) +
             geom_errorbar(aes(ymin=y_lower, ymax=y_upper),
                           width=0.2, size=1, alpha=0.7) +
             geom_line(data=linf %>% 
                         bind_rows(.id="side"),
                       aes(x=x, y=y, group=side),
                       color="red") +
             scale_x_continuous(breaks=seq(x_threshold-bw, x_threshold+bw+5, x_interval),
                                minor_breaks=seq(x_threshold-bw, x_threshold+bw+5, 1),
                                label=scales::dollar) +
             labs(x="Bill",
                  y=outcome_labels[i],
                  title=paste0("Threshold Effect on ", title_labels[i]),
                  subtitle=subtitle) +
             fte_theme() +
             geom_vline(xintercept=threshold, colour="darkred", linetype="dashed") +
             annotate("label", x=x_threshold+bw+5, y=min(bins$df_bin$y_lower), label=paste0("n=", n),
                      colour="black", size=5, family="serif"))
    
    if (outcome_variables[i] %in% c("payment", "income")) {
      assign(paste0("gg_", outcome_variables[i]),
             get(paste0("gg_", outcome_variables[i])) + 
               scale_y_continuous(label=scales::dollar))
    }
    
    ggsave(get(paste0("gg_", outcome_variables[i])),
           filename=paste0(output_dir, "/figures/rd_", 
                           type, "_", threshold_round, "_",
                           outcome_variables[i], ".png"),
           width=plot_width, height=8)
    
    # Window analysis (only compare means above and below the threshold)
    fit <- lm(as.formula(paste0(outcome_variables[i], "~above_threshold")), 
              data=df_analysis)
    fit_summary <- broom::tidy(fit) %>%
      filter(term=="above_thresholdTRUE") %>%
      mutate(outcome=outcome_variables[i],
             estimate=ifelse(is.na(estimate), 0, estimate),
             std_error=ifelse(is.na(std.error), 0, std.error),
             statistic=ifelse(is.na(statistic), 0, statistic),
             p_value=ifelse(is.na(p.value), 1, p.value)) %>%
      select(outcome, estimate, std_error, statistic, p_value)
    
    window_analysis <- bind_rows(window_analysis,
                                 fit_summary %>%
                                   mutate(threshold=threshold,
                                          type=type,
                                          n=n))
  }
  return(window_analysis)
}

# Execute RD graphs
rd_list <- data.frame(type=c(rep(c("all", "all_q1", "all_q4", "all_debt",
                                   "pre", "covid", "precovid",
                                   "rct", "rct_q1", "rct_q4", "rct_debt"), each=2), "rct_control"),
                      subtitle=c(rep(c("(Non-RCT Sample for 2025Q1)", 
                                       "(Non-RCT Sample in Lowest Income Quartile for 2025Q1)",
                                       "(Non-RCT Sample in Highest Income Quartile for 2025Q1)", 
                                       "(Non-RCT Sample with Unpaid Past Debt for 2025Q1)",
                                       "(All Sample for 2024Q1-2024Q4)",
                                       "(All Sample for 2021Q1-2021Q4 (COVID))",
                                       "(All Sample for 2019Q1-2019Q4 (Pre-COVID))",
                                       "(RCT Sample for 2025Q1)", 
                                       "(RCT Sample in Lowest Income Quartile for 2025Q1)",
                                       "(RCT Sample in Highest Income Quartile for 2025Q1)", 
                                       "(RCT Sample with Unpaid Past Debt for 2025Q1)"), each=2),
                                 "RCT Control Group"),
                      threshold=c(rep(c(292.08, 113.87), 4),
                                  rep(c(292.08, 113.87), 3),
                                  rep(c(278.22, 113.87), 4), 278.22))

for (row in 1:nrow(rd_list)) {
  threshold_round <- case_when(rd_list$threshold[row]==292.08 ~ 300,
                               rd_list$threshold[row]==278.22 ~ 300,
                               rd_list$threshold[row]==113.87 ~ 115,
                               .default=NA)
  
  assign(paste0("rd_analysis_", rd_list$type[row], "_", threshold_round),
         rd_analysis(get(paste0("threshold_data_", rd_list$type[row])),
                     threshold=rd_list$threshold[row], 
                     type=rd_list$type[row],
                     subtitle=rd_list$subtitle[row]))
}

rd_list_large_bw <- rd_list %>%
  filter(!grepl("_", type)) %>%
  mutate(type_bw=paste0(type, "_large_bw"))

for (row in 1:nrow(rd_list_large_bw)) {
  threshold_round <- case_when(rd_list_large_bw$threshold[row]==292.08 ~ 300,
                               rd_list_large_bw$threshold[row]==278.22 ~ 300,
                               rd_list_large_bw$threshold[row]==113.87 ~ 115,
                               .default=NA)
  
  bw <- case_when(threshold_round==115 ~ 50,
                  threshold_round==300 ~ 100,
                  .default=20)
  
  assign(paste0("rd_analysis_", rd_list_large_bw$type_bw[row], "_", threshold_round),
         rd_analysis(get(paste0("threshold_data_", rd_list_large_bw$type[row])),
                     threshold=rd_list_large_bw$threshold[row], 
                     type=rd_list_large_bw$type_bw[row],
                     bw=bw,
                     subtitle=rd_list_large_bw$subtitle[row]))
}

# Export window analysis results in a tex table
tab_row <- function(row, se=FALSE, dec=0) {
  if (se) {
    TexRow(c(rd_analysis_all_300$std_error[row],
             rd_analysis_rct_control_300$std_error[row],
             rd_analysis_all_115$std_error[row]),
           se=TRUE, dec=rep(dec, 3))
  } else {
    TexRow(c(rd_analysis_all_300$estimate[row],
             rd_analysis_rct_control_300$estimate[row],
             rd_analysis_all_115$estimate[row]),
           pvalues=c(rd_analysis_all_300$p_value[row],
                     rd_analysis_rct_control_300$p_value[row],
                     rd_analysis_all_115$p_value[row]),
           dec=rep(dec, 3))
  }
}

tab <- TexRow(c("Threshold", "\\$300 (Shut-Offs)", "\\$115 (Late Notice)"), cspan=c(1, 2, 1)) +
  TexMidrule(list(c(1, 1), c(2, 3), c(4, 4))) +
  TexRow(c("Outcome", "Classic RD", "RCT", "Classic RD")) +
  TexMidrule() +
  TexRow("Delinquency") / tab_row(2, dec=3) +
  TexRow("") / tab_row(2, se=TRUE, dec=3) +
  TexRow("Payment Share") / tab_row(3, dec=3) +
  TexRow("") / tab_row(3, se=TRUE, dec=3) +
  TexRow("Paying Something") / tab_row(4, dec=3) +
  TexRow("") / tab_row(4, se=TRUE, dec=3) +
  TexRow("Shut-Offs") / tab_row(6, dec=3) +
  TexRow("") / tab_row(6, se=TRUE, dec=3) +
  TexMidrule() +
  TexRow("Observations") / 
  TexRow(c(rd_analysis_all_300$n[1],
           rd_analysis_rct_300$n[1],
           rd_analysis_all_115$n[1]) %>% as.numeric(), dec=0)
tab
TexSave(tab, filename=paste0(output_dir, "/tables/rd_analysis"),
        positions=c('l', rep('c', 4)))

delinquency_change_threshold <- round(-rd_analysis_all_300$estimate[2]*100, 1)
export_tex(paste0(delinquency_change_threshold, "p.p."),
           "delinquency_change_threshold")

payshare_change_threshold <- round(rd_analysis_all_300$estimate[3]*100, 1)
export_tex(paste0(payshare_change_threshold, "p.p."),
           "payshare_change_threshold")

payshare_change_300_bill <- payshare_change_threshold*300/100
export_tex(gsub("\\$", "\\\\$", scales::dollar(payshare_change_300_bill)),
           "payshare_change_300_bill")

delinquency_change_threshold_rct <- round(-rd_analysis_rct_control_300$estimate[2]*100, 1)
export_tex(paste0(delinquency_change_threshold_rct, "p.p."),
           "delinquency_change_threshold_rct")

payshare_change_threshold_rct <- round(rd_analysis_rct_control_300$estimate[3]*100, 1)
export_tex(paste0(payshare_change_threshold_rct, "p.p."),
           "payshare_change_threshold_rct")

delinquency_change_threshold_precovid <- round(-rd_analysis_precovid_115$estimate[2]*100, 1)
export_tex(paste0(delinquency_change_threshold_precovid, "p.p."),
           "delinquency_change_threshold_precovid")

payshare_change_threshold_precovid <- round(rd_analysis_precovid_115$estimate[3]*100, 1)
export_tex(paste0(payshare_change_threshold_precovid, "p.p."),
           "payshare_change_threshold_precovid")


#---------+---------+---------+---------+---------+---------+
# Back-of-the-Envelope Calculations
#---------+---------+---------+---------+---------+---------+
threshold_data_pre <- estimation_dataset_all %>%
  filter(!is_rebill,
         !first_bill) %>%
  mutate(fa=linc_discount<0) %>%
  arrange(id, bill_date) %>%
  filter(account_status!="FINAL",
         bill_date>="2024-09-11", bill_date<="2024-12-11") %>%
  mutate(bill=O_t,
         payshare=-E_t/bill,
         payshare=ifelse(payshare>1, 1, payshare))

# First, calculate the revenue
revenue_pre <- threshold_data_pre %>%
  filter(bill>0) %>%
  summarise(revenue=sum(-E_t, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round(revenue_pre/1000000, 1), " million"),
           "revenue_pre")

revenue_summary <- threshold_data_pre %>%
  filter(bill<292.08, bill>0) %>%
  mutate(tile_bill=ntile(bill, 50)) %>%
  group_by(tile_bill) %>%
  summarise(mean_bill=mean(bill, na.rm=TRUE),
            mean_payshare=mean(payshare, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(old_revenue=mean_bill*mean_payshare,
         new_payshare=mean_payshare+payshare_change_threshold/100,
         new_payshare=ifelse(new_payshare>1, 1, new_payshare),
         new_revenue=mean_bill*new_payshare)

# Sum over revenue
revenue_pre_below_threshold <- revenue_summary %>%
  summarise(revenue=sum(old_revenue*n, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round(revenue_pre_below_threshold/1000000, 1), " million (",
                  round(revenue_pre_below_threshold/revenue_pre*100, 1), "\\%)"),
           "revenue_pre_threshold_0")

revenue_imputed <- revenue_summary %>%
  summarise(revenue=sum(new_revenue*n, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round((revenue_imputed-revenue_pre_below_threshold)/1000, 1), " thousand"),
           "revenue_imputed_threshold_0")

revenue_summary <- threshold_data_pre %>%
  filter(bill>292.08) %>%
  mutate(tile_bill=ntile(bill, 50)) %>%
  group_by(tile_bill) %>%
  summarise(mean_bill=mean(bill, na.rm=TRUE),
            mean_payshare=mean(payshare, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(old_revenue=mean_bill*mean_payshare,
         new_payshare=mean_payshare-payshare_change_threshold/100,
         new_payshare=ifelse(new_payshare>1, 1, new_payshare),
         new_revenue=mean_bill*new_payshare)

# Sum over revenue
revenue_pre_below_threshold <- revenue_summary %>%
  summarise(revenue=sum(old_revenue*n, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round(revenue_pre_below_threshold/1000000, 1), " million (",
                  round(revenue_pre_below_threshold/revenue_pre*100, 1), "\\%)"),
           "revenue_pre_threshold_infty")

revenue_imputed <- revenue_summary %>%
  summarise(revenue=sum(new_revenue*n, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round((-(revenue_imputed-revenue_pre_below_threshold))/1000000, 1), " million"),
           "revenue_imputed_threshold_infty")

revenue_summary <- threshold_data_pre %>%
  filter(bill>292.08, bill<1000) %>%
  mutate(tile_bill=ntile(bill, 50)) %>%
  group_by(tile_bill) %>%
  summarise(mean_bill=mean(bill, na.rm=TRUE),
            mean_payshare=mean(payshare, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(old_revenue=mean_bill*mean_payshare,
         new_payshare=mean_payshare-payshare_change_threshold/100,
         new_payshare=ifelse(new_payshare>1, 1, new_payshare),
         new_revenue=mean_bill*new_payshare)

# Sum over revenue
revenue_pre_below_threshold <- revenue_summary %>%
  summarise(revenue=sum(old_revenue*n, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round(revenue_pre_below_threshold/1000000, 1), " million (",
                  round(revenue_pre_below_threshold/revenue_pre*100, 1), "\\%)"),
           "revenue_pre_threshold_1000")

revenue_imputed <- revenue_summary %>%
  summarise(revenue=sum(new_revenue*n, na.rm=TRUE)) %>%
  pull(revenue)

export_tex(paste0("\\$",
                  round((-(revenue_imputed-revenue_pre_below_threshold))/1000000, 1), " million"),
           "revenue_imputed_threshold_1000")
