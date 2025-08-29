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

# Function for bar graphs
bar_graph_treatment <- function(df, outcome, facet_var="", 
                                ylim_upper, title, subtitle, x, y,
                                filename, width, height) {
  
  outcome_sym <- sym(outcome)
  facet_sym <- sym(facet_var)
  
  if (facet_var!="") {
    df_analysis <- df %>%
      group_by(discount_grid, !!facet_sym) %>%
      summarise(mean_outcome=mean(!!outcome_sym, na.rm=TRUE),
                sd_outcome=sd(!!outcome_sym, na.rm=TRUE),
                n=n()) %>%
      ungroup()
  } else {
    df_analysis <- df %>%
      group_by(discount_grid) %>%
      summarise(mean_outcome=mean(!!outcome_sym, na.rm=TRUE),
                sd_outcome=sd(!!outcome_sym, na.rm=TRUE),
                n=n()) %>%
      ungroup()
  }
  
  if (outcome %in% c("delinquent", "shutoff")) {
    df_analysis <- df_analysis %>%
      rowwise() %>%
      mutate(y_lower=binom_ci(mean_outcome, n)[1],
             y_upper=binom_ci(mean_outcome, n)[2]) %>%
      ungroup()
  } else {
    df_analysis <- df_analysis %>%
      mutate(se_outcome=sd_outcome/sqrt(n),
             y_lower=mean_outcome-1.96*se_outcome,
             y_upper=mean_outcome+1.96*se_outcome)
  }
  
  df_analysis <- df_analysis %>%
    filter(!is.na(!!facet_sym), n>1)
  
  if (facet_var!="") {
    df_analysis <- df_analysis %>%
      arrange(!!facet_sym, discount_grid)
  } else {
    df_analysis <- df_analysis %>%
      arrange(discount_grid)
  }
  
  break_size <- case_when(ylim_upper<0.25 ~ 0.05,
                          ylim_upper<1 ~ 0.1,
                          ylim_upper<500 ~ 50,
                          .default=100)
  
  minor_break_size <- case_when(ylim_upper<0.25 ~ 0.01,
                                ylim_upper<1 ~ 0.05,
                                ylim_upper<500 ~ 10,
                                .default=25)
  
  gg <- ggplot(df_analysis %>%
                 mutate(y_upper=ifelse(y_upper>ylim_upper, 
                                       ylim_upper, 
                                       y_upper)), 
               aes(x=discount_grid, y=mean_outcome)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=y_lower, ymax=y_upper),
                  width=3, size=1, alpha=0.7) +
    scale_y_continuous(breaks=seq(0, ylim_upper, break_size),
                       minor_breaks=seq(0, ylim_upper, minor_break_size)) +
    scale_fill_manual(values=colours_set) +
    labs(title=title,
         subtitle=subtitle,
         x=x,
         y=y) +
    fte_theme()
  
  if (facet_var!="") {
    gg <- gg +
      geom_label(aes(label=paste0("n=", n)), 
                 y=ylim_upper, size=4, family="serif", color="black") +
      facet_wrap(vars(!!facet_sym), nrow=1) +
      theme(strip.background=element_rect(fill="white"))
  }
  
  ggsave(gg, filename=filename, width=width, height=height)
  return(gg)
}


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
# RCT participants only
load(paste0(working_data_dir, "/servus/analysis/estimation_dataset.RData"))


#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
estimation_dataset <- estimation_dataset %>%
  filter(is.na(exit_reason),
         account_status!="FINAL",
         !is_rebill,
         !first_bill)

# Only choose those who received the "next bill"
# Only choose those who got "full service" (water, sewer, and stormwater)
# Only choose those who are not on a monthly installments
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
  filter(service_water, service_sewer, service_storm,
         t==0, 
         D_t>=0, !monthly_payment, is.na(payment_plan)) %>%
  mutate(bill=O_t-D_t)

threshold_data_rct <- threshold_data_rct %>%
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
         shutoff=!is.na(shutoff_date))

# Filter on the RCT control group
threshold_data_rct_control <- threshold_data_rct %>%
  filter(discount_grid==0)

#---------+---------+---------+---------+---------+---------+
# RCT Treatment Effect
#---------+---------+---------+---------+---------+---------+
# Calculate pre-discount amount
threshold_data_rct <- threshold_data_rct %>%
  mutate(pre_discount_bill=bill-rct_discount)

# Treatment effect for those over the threshold
threshold_data_rct_above_all <- threshold_data_rct %>% 
  filter(bill>=278.22) %>%
  distinct()

threshold_data_rct_above_pre_discount <- threshold_data_rct_above_all %>%
  mutate(cut=ntile(pre_discount_bill, n=10)) %>%
  group_by(cut) %>%
  summarise(mean_pre_discount_bill=mean(pre_discount_bill, na.rm=TRUE),
            n_delinquent=sum(delinquent, na.rm=TRUE),
            mean_delinquent=mean(delinquent, na.rm=TRUE),
            sd_delinquent=sd(delinquent, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(y_lower_delinquent=binom_ci(mean_delinquent, n)[1],
         y_upper_delinquent=binom_ci(mean_delinquent, n)[2]) %>%
  ungroup()

gg <- ggplot(threshold_data_rct_above_pre_discount, 
             aes(x=mean_pre_discount_bill, y=mean_delinquent)) +
  geom_point(color="navy", size=3) +
  geom_errorbar(aes(ymin=y_lower_delinquent, ymax=y_upper_delinquent),
                width=0.2, size=1, alpha=0.7) +
  scale_x_continuous(breaks=seq(200, 1000, 100),
                     minor_breaks=seq(200, 1000, 10),
                     label=scales::dollar) +
  labs(x="Pre-Discount Bill",
       y="Probability of Delinqeuncy",
       title="Probability of Delinquency by Pre-Discount Bill",
       subtitle="(Discount Bill Above Threshold)") +
  fte_theme()
gg
ggsave(gg,
       filename=paste0(output_dir, "/figures/rct_above_threshold_pre_delinquency.png"),
       width=12, height=8)

# Begin with pre-discount bills
threshold_data_rct_above_all_graph <- threshold_data_rct_above_all %>% 
  mutate(pre_discount_grid=case_when(
    pre_discount_bill<600 ~ "Pre-Discount Bill < $600",
    pre_discount_bill>=600 & pre_discount_bill<900 ~ "Pre-Discount Bill $600-900",
    pre_discount_bill>=900 ~ "Pre-Discount Bill > $900",
    TRUE ~ NA),
    pre_discount_grid=factor(pre_discount_grid,
                             levels=c("Pre-Discount Bill < $600", 
                                      "Pre-Discount Bill $600-900",
                                      "Pre-Discount Bill > $900")))

outcome_list <- data.frame(outcome=c("delinquent", "shutoff", "payment"),
                           title=c("Delinquency", "Shut-Off", "Revenue"),
                           y_label=c("Probability of Delinquency",
                                     "Probability of Shut-Off",
                                     "Revenue"),
                           ylim_upper=c(0.3, 0.12, 1250))

for (i in 1:nrow(outcome_list)) {
  gg <- bar_graph_treatment(threshold_data_rct_above_all_graph, 
                            outcome=outcome_list$outcome[i], 
                            facet_var="pre_discount_grid",
                            ylim_upper=outcome_list$ylim_upper[i],
                            title=paste0("Treatment Effect on ", outcome_list$title[i]),
                            subtitle="(Discount Bill Above Threshold)",
                            x="Discount Level",
                            y=outcome_list$y_label[i],
                            filename=paste0(output_dir, 
                                            "/figures/rct_above_threshold_",
                                            outcome_list$outcome[i], ".png"),
                            width=16, height=8)
  gg
}

# More granular pre-discount grid (but need a narrower discount cells)
threshold_data_rct_above_all_graph <- threshold_data_rct_above_all %>% 
  filter(discount_grid<40) %>%
  mutate(pre_discount_grid=case_when(
    pre_discount_bill<350 ~ "< $350",
    pre_discount_bill>=350 & pre_discount_bill<450 ~ "$350-450",
    pre_discount_bill>=450 & pre_discount_bill<550 ~ "$450-550",
    pre_discount_bill>=550 & pre_discount_bill<650 ~ "$550-650",
    pre_discount_bill>=650 & pre_discount_bill<750 ~ "$650-750",
    pre_discount_bill>=750 & pre_discount_bill<850 ~ "$750-850",
    pre_discount_bill>=850 ~ "> $850",
    TRUE ~ NA),
    pre_discount_grid=factor(pre_discount_grid,
                             levels=c("< $350", 
                                      "$350-450",
                                      "$450-550",
                                      "$550-650",
                                      "$650-750",
                                      "$750-850",
                                      "> $850")))

outcome_list <- data.frame(outcome=c("delinquent", "shutoff", "payment"),
                           title=c("Delinquency", "Shut-Off", "Revenue"),
                           y_label=c("Probability of Delinquency",
                                     "Probability of Shut-Off",
                                     "Revenue"),
                           ylim_upper=c(0.15, 0.12, 1150))

for (i in 1:nrow(outcome_list)) {
  gg <- bar_graph_treatment(threshold_data_rct_above_all_graph, 
                            outcome=outcome_list$outcome[i], 
                            facet_var="pre_discount_grid",
                            ylim_upper=outcome_list$ylim_upper[i],
                            title=paste0("Treatment Effect on ", outcome_list$title[i]),
                            subtitle="(Discount Bill Above Threshold)",
                            x="Discount Level",
                            y=outcome_list$y_label[i],
                            filename=paste0(output_dir, 
                                            "/figures/rct_above_threshold_granular_",
                                            outcome_list$outcome[i], ".png"),
                            width=20, height=8)
  gg
}


# Show the hybrid RD-RCT ####
hybrid_rd_rct <- function(df_control, df_all, subtitle, filename) {
  threshold_data_rct_control <- df_control %>%
    filter(bill>278.22-20, bill<278.22+20) %>%
    mutate(above_threshold=bill>=278.22) %>%
    group_by(above_threshold) %>%
    summarise(mean_delinquent=mean(delinquent, na.rm=TRUE),
              sd_delinquent=sd(delinquent, na.rm=TRUE),
              mean_payshare=mean(payshare, na.rm=TRUE),
              sd_payshare=sd(payshare, na.rm=TRUE),
              mean_payment=mean(payment, na.rm=TRUE),
              sd_payment=sd(payment, na.rm=TRUE),
              n=n()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(y_lower_delinquent=binom_ci(mean_delinquent, n)[1],
           y_upper_delinquent=binom_ci(mean_delinquent, n)[2],
           y_lower_payshare=binom_ci(mean_payshare, n)[1],
           y_upper_payshare=binom_ci(mean_payshare, n)[2]) %>%
    ungroup() %>%
    mutate(se_payment=sd_payment/sqrt(n),
           y_lower_payment=mean_payment-1.96*se_payment,
           y_upper_payment=mean_payment+1.96*se_payment,
           above_threshold=ifelse(above_threshold, 
                                  "0\n(Above)", 
                                  "0\n(Below)"))
  
  # Now show the treatment effect for those in the "above threshold" window
  threshold_data_rct_below <- df_all %>% 
    filter(pre_discount_bill>278.22-20, 
           pre_discount_bill<278.22)
  
  n <- nrow(threshold_data_rct_below)
  
  threshold_data_rct_graph <- threshold_data_rct_below %>%
    group_by(discount_grid) %>%
    summarise(mean_delinquent=mean(delinquent, na.rm=TRUE),
              sd_delinquent=sd(delinquent, na.rm=TRUE),
              mean_payshare=mean(payshare, na.rm=TRUE),
              sd_payshare=sd(payshare, na.rm=TRUE),
              mean_payment=mean(payment, na.rm=TRUE),
              sd_payment=sd(payment, na.rm=TRUE),
              n=n()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(y_lower_delinquent=binom_ci(mean_delinquent, n)[1],
           y_upper_delinquent=binom_ci(mean_delinquent, n)[2],
           y_lower_payshare=binom_ci(mean_payshare, n)[1],
           y_upper_payshare=binom_ci(mean_payshare, n)[2]) %>%
    ungroup() %>%
    mutate(se_payment=sd_payment/sqrt(n),
           y_lower_payment=mean_payment-1.96*se_payment,
           y_upper_payment=mean_payment+1.96*se_payment,
           discount_grid=as.character(discount_grid)) %>%
    filter(discount_grid!="0")
  
  threshold_data_rct_graph <- threshold_data_rct_graph %>%
    select(discount_grid,
           mean_delinquent, y_lower_delinquent, y_upper_delinquent,
           mean_payshare, y_lower_payshare, y_upper_payshare,
           mean_payment, y_lower_payment, y_upper_payment) %>%
    bind_rows(threshold_data_rct_control %>%
                select(discount_grid=above_threshold,
                       mean_delinquent, y_lower_delinquent, y_upper_delinquent,
                       mean_payshare, y_lower_payshare, y_upper_payshare,
                       mean_payment, y_lower_payment, y_upper_payment)) %>%
    mutate(below_threshold=discount_grid=="0\n(Below)",
           discount_grid=factor(discount_grid,
                                levels=c("0\n(Above)", "0\n(Below)",
                                         "10", "20", "30", "40", "50", "60", "70", "80")))
  
  gg <- ggplot(threshold_data_rct_graph, 
               aes(x=discount_grid, y=mean_delinquent, fill=below_threshold)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=y_lower_delinquent, ymax=y_upper_delinquent),
                  width=0.2, size=1, alpha=0.7) +
    scale_y_continuous(breaks=seq(0, 0.5, 0.1),
                       minor_breaks=seq(0, 0.5, 0.05)) +
    scale_fill_manual(values=colours_set) +
    labs(title="Treatment Effect on Delinquency",
         subtitle=subtitle,
         x="Discount Level",
         y="Probability of Delinquency") +
    fte_theme() +
    theme(legend.position="") +
    geom_vline(xintercept=1.5, colour="darkred", linetype="dashed") +
    annotate("label", x=1.5, y=0.35, label="Shut-Off Threshold",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=7.5, colour="darkred", linetype="dashed") +
    annotate("label", x=7.5, y=0.35, label="Late Notice Threshold",
             colour="darkred", size=5, family="serif") +
    annotate("label", x=1, y=0.2, label=paste0("n=", n),
             colour="black", size=5, family="serif")
  gg
  ggsave(gg,
         filename=paste0(output_dir, "/figures/", filename, "_threshold_delinquent.png"),
         width=12, height=8)
  
  gg <- ggplot(threshold_data_rct_graph, 
               aes(x=discount_grid, y=mean_payshare, fill=below_threshold)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=y_lower_payshare, ymax=y_upper_payshare),
                  width=0.2, size=1, alpha=0.7) +
    scale_y_continuous(breaks=seq(0, 1, 0.1),
                       minor_breaks=seq(0, 1, 0.05)) +
    scale_fill_manual(values=colours_set) +
    labs(title="Treatment Effect on Payment Share",
         subtitle=subtitle,
         x="Discount Level",
         y="Payment Share") +
    fte_theme() +
    theme(legend.position="") +
    geom_vline(xintercept=1.5, colour="darkred", linetype="dashed") +
    annotate("label", x=1.5, y=0.35, label="Shut-Off Threshold",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=7.5, colour="darkred", linetype="dashed") +
    annotate("label", x=7.5, y=0.35, label="Late Notice Threshold",
             colour="darkred", size=5, family="serif") +
    annotate("label", x=1, y=0.1, label=paste0("n=", n),
             colour="black", size=5, family="serif")
  gg
  ggsave(gg,
         filename=paste0(output_dir, "/figures/", filename, "_threshold_payshare.png"),
         width=12, height=8)
  
  gg <- ggplot(threshold_data_rct_graph, 
               aes(x=discount_grid, y=mean_payment, fill=below_threshold)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=y_lower_payment, ymax=y_upper_payment),
                  width=0.2, size=1, alpha=0.7) +
    scale_y_continuous(breaks=seq(0, 450, 50),
                       minor_breaks=seq(0, 450, 10),
                       label=scales::dollar) +
    scale_fill_manual(values=colours_set) +
    labs(title="Treatment Effect on Revenue",
         subtitle=subtitle,
         x="Discount Level",
         y="Revenue") +
    fte_theme() +
    theme(legend.position="") +
    geom_vline(xintercept=1.5, colour="darkred", linetype="dashed") +
    annotate("label", x=1.5, y=425, label="Shut-Off Threshold",
             colour="darkred", size=5, family="serif") +
    geom_vline(xintercept=7.5, colour="darkred", linetype="dashed") +
    annotate("label", x=7.5, y=425, label="Late Notice Threshold",
             colour="darkred", size=5, family="serif") +
    annotate("label", x=10, y=300, label=paste0("n=", n),
             colour="black", size=5, family="serif")
  gg
  ggsave(gg,
         filename=paste0(output_dir, "/figures/", filename, "_threshold_payment.png"),
         width=12, height=8)
}

# Execute!
hybrid_rd_rct(df_control=threshold_data_rct_control, 
              df_all=threshold_data_rct, 
              subtitle="(Pre-Discount Bill Right Below Threshold)",
              filename="rct")

hybrid_rd_rct(df_control=threshold_data_rct_control %>%
                filter(fa_eligible), 
              df_all=threshold_data_rct %>%
                filter(fa_eligible), 
              subtitle="(Pre-Discount Bill Right Below Threshold,\nIncome Below Means-Tested Upper Limit)",
              filename="rct_below_income")

hybrid_rd_rct(df_control=threshold_data_rct_control %>%
                filter(!fa_eligible), 
              df_all=threshold_data_rct %>%
                filter(!fa_eligible), 
              subtitle="(Pre-Discount Bill Right Below Threshold,\nIncome Above Means-Tested Upper Limit)",
              filename="rct_above_income")

hybrid_rd_rct(df_control=threshold_data_rct_control %>%
                filter(D_t>0), 
              df_all=threshold_data_rct %>%
                filter(D_t>0), 
              subtitle="(Pre-Discount Bill Right Below Threshold,\nHave Unpaid Debt)",
              filename="rct_delinquent")

hybrid_rd_rct(df_control=threshold_data_rct_control %>%
                filter(D_t==0), 
              df_all=threshold_data_rct %>%
                filter(D_t==0), 
              subtitle="(Pre-Discount Bill Right Below Threshold,\nNo Unpaid Debt)",
              filename="rct_not_delinquent")

