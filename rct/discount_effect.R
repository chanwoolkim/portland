#=========================================================================#
# discount_effect.R
# 
# Analysis on RCT discounts
#
# June 1, 2025
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
load(paste0(working_data_dir, "/transunion/analysis/estimation_dataset_all.RData"))

# RCT participants only
load(paste0(working_data_dir, "/transunion/analysis/estimation_dataset.RData"))


#---------+---------+---------+---------+---------+---------+
# Assemble Estimation Sample
#---------+---------+---------+---------+---------+---------+
# Only select relevant sample
estimation_dataset <- estimation_dataset %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  mutate(next_bill_date=lead(bill_date)) %>%
  ungroup() %>%
  filter(is.na(exit_reason),
         account_status!="FINAL",
         !is_rebill,
         !first_bill,
         (fa_type != "Tier2" | is.na(fa_type))) %>%
  mutate(B_t=ifelse(B_t<0, 0, B_t),
         lag_w_t=ifelse(lag_w_t<0, lag_w_t, lag_w_t),
         bill=if_else(t==0, O_t-D_t, O_t),
         payment=-E_t,
         pay=payment<bill,
         payshare=if_else(O_t==0, NaN, pmin(pmax(payment/bill, 0), 1)),
         deadbeat=case_when(
           ufh ~ "Below UFH",
           !ufh & below_median_income ~ "Below Median Income",
           !ufh & !below_median_income ~ "Above Median Income"),
         deadbeat=factor(deadbeat, 
                         levels=c("Below UFH", 
                                  "Below Median Income", 
                                  "Above Median Income")),
         delinquent=ifelse(D_t>0, "Have Unpaid Debt", "No Unpaid Debt"),
         delinquent=factor(delinquent,
                           levels=c("Have Unpaid Debt", "No Unpaid Debt")),
         iq=case_when(
           ufh ~ "Below UFH",
           !ufh & fa_eligible ~ "UFH to Means-Tested Cap",
           !ufh & !fa_eligible ~ "Above Means-Tested Cap"),
         iq=factor(iq,
                   levels=c("Below UFH", 
                            "UFH to Means-Tested Cap", 
                            "Above Means-Tested Cap")),
         cq=case_when(
           credit_quartile==1 ~ "Sub-Prime",
           credit_quartile==2 ~ "Near-Prime",
           credit_quartile==3 ~ "Prime",
           credit_quartile==4 ~ "Super-Prime"),
         cq=factor(cq, levels=c("Sub-Prime",
                                "Near-Prime",
                                "Prime",
                                "Super-Prime")))

# Information (t==-1)
info_treat_data <- estimation_dataset %>%
  filter(t==-1)

# Discount (t==0)
discount_treat_data <- estimation_dataset %>%
  filter(t==0)


#---------+---------+---------+---------+---------+---------+
# Create treatment effect graphs
#---------+---------+---------+---------+---------+---------+
treatment_effect_graph <- function(df, outcome, group="") {
  y_label <- case_when(
    outcome=="pay" ~ "Probability of Delinquency",
    outcome=="payshare" ~ "Payment Share",
    outcome=="payment" ~ "Expected Revenue (Per Customer)",
    outcome=="w_t" ~ "Water Use (Per Customer) (in ccf)",
    outcome=="bill" ~ "Bill",
    .default="")
  
  ylim <- case_when(
    outcome=="pay" ~ list(c(0, 0.5)),
    outcome=="payshare" ~ list(c(0, 1)),
    outcome=="payment" ~ list(c(0, 600)),
    outcome=="w_t" ~ list(c(0, 20)),
    outcome=="bill" ~ list(c(0, 800)),
    .default=list(c(0, 0)))
  
  group_name <- case_when(
    group=="iq" ~ "Income Quartile",
    group=="cq" ~ "Credit Quartile",
    group=="deadbeat" ~ "Income Status",
    group=="delinquent" ~ "Delinquency Status",
    .default="")
  
  title <- case_when(
    outcome=="pay" ~ "Delinquency",
    outcome=="payshare" ~ "Payment Share",
    outcome=="payment" ~ "Expected Revenue",
    outcome=="w_t" ~ "Water Use",
    outcome=="bill" ~ "Bill Amount",
    .default="")
  
  if (group!="") {
    title <- paste0(title, " by ", group_name)
  }
  
  subtitle <- case_when(
    df=="info" ~ "(Information Treatment)",
    df=="discount" ~ "(Discount Treatment)",
    .default=""
  )
  
  outcome_sym <- sym(outcome)
  group_sym <- sym(group)
  
  if (group=="") {
    df_analysis <- get(paste0(df, "_treat_data")) %>%
      group_by(discount_grid) %>%
      summarise(mean_outcome=mean(!!outcome_sym, na.rm=TRUE),
                sd_outcome=sd(!!outcome_sym, na.rm=TRUE),
                n=n()) %>%
      ungroup() %>%
      mutate(se_outcome=sd_outcome/sqrt(n),
             y_lower=mean_outcome-1.96*se_outcome,
             y_upper=mean_outcome+1.96*se_outcome)
    
    n_group <- 1
  } else {
    df_analysis <- get(paste0(df, "_treat_data")) %>%
      group_by(discount_grid, !!group_sym) %>%
      summarise(mean_outcome=mean(!!outcome_sym, na.rm=TRUE),
                sd_outcome=sd(!!outcome_sym, na.rm=TRUE),
                n=n()) %>%
      ungroup() %>%
      mutate(se_outcome=sd_outcome/sqrt(n),
             y_lower=mean_outcome-1.96*se_outcome,
             y_upper=mean_outcome+1.96*se_outcome) %>%
      filter(!is.na(!!group_sym))
    
    n_group <- n_distinct(df_analysis[[group]])
  }
  
  if (outcome %in% c("pay", "payshare")) {
    df_analysis <- df_analysis %>%
      rowwise() %>%
      mutate(y_lower=binom_ci(mean_outcome, n)[1],
             y_upper=binom_ci(mean_outcome, n)[2]) %>%
      ungroup()
  }
  
  gg <- ggplot(df_analysis,
               aes(x=discount_grid, y=mean_outcome)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=y_lower, ymax=y_upper),
                  width=3, size=1, alpha=0.7) +
    scale_x_continuous(breaks=seq(0, 80, 10),
                       minor_breaks=seq(0, 80, 10)) +
    scale_y_continuous(limits=ylim[[1]]) +
    labs(title=paste0("Treatment Effect on ", title),
         subtitle=subtitle,
         x="Discount Level",
         y=y_label) +
    fte_theme()
  
  if (outcome %in% c("pay", "payshare")) {
    gg <- gg + scale_y_continuous(labels=scales::percent, 
                                  limits=ylim[[1]])
  }
  
  if (outcome %in% c("payment", "bill")) {
    gg <- gg + scale_y_continuous(labels=scales::dollar, 
                                  limits=ylim[[1]])
  }
  
  gg_filename <- paste0(output_dir, "/figures/rct_", df, "_", outcome, ".png")
  
  if (group!="") {
    gg <- gg + 
      facet_wrap(as.formula(paste("~", group)), nrow=1) +
      theme(strip.background=element_rect(fill="white"))
    gg_filename <- paste0(output_dir, "/figures/rct_", df, "_", outcome, "_by_", group, ".png")
  }
  
  gg
  ggsave(gg, filename=gg_filename, width=12+(n_group-1)*2, height=8)
  
  return(gg)
}

# Execute!
rct_list <- data.frame(data=rep(c("info", "discount"), each=25),
                       outcome=rep(c("pay", "payshare", "payment", "w_t", "bill"), each=5, times=2),
                       group=rep(c("", "iq", "cq", "deadbeat", "delinquent"), times=10))

for (i in 1:nrow(rct_list)) {
  treatment_effect_graph(rct_list$data[i], rct_list$outcome[i], rct_list$group[i])
}
