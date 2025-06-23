usage_info <- read_csv(paste0(working_data_dir, "/servus_query/rct_usage.csv"))
rct_bill <- read_csv(paste0(working_data_dir, "/servus_query/rct_bill.csv"))
rct_subjects <- read_csv(paste0(working_data_dir, "/servus_query/account_rct.csv"))
monthly_accounts <- read_csv("~/Downloads/monthly_payers.csv")

rct_bill <- rct_bill %>%
  mutate_at(vars("account_number",
                 "cycle_code",
                 "previous_bill_amount",
                 "previous_unpaid_amount",
                 "fee_amount",
                 "amount_trans_billed",
                 "amount_billed",
                 "amount_paid",
                 "discount",
                 "adjustment",
                 "transfer",
                 "donation",
                 "write_off",
                 "lien",
                 "bankruptcy",
                 "running_owed",
                 "running_debt"), as.numeric)

rct_subjects <- rct_subjects %>%
  filter(linc_tier_type!="Tier2" | is.na(linc_tier_type)) %>%
  distinct()

rct_total <- rct_subjects %>%
  group_by(discount_percentage) %>%
  summarise(n_total=n_distinct(account_number, na.rm=TRUE))

usage_info <- usage_info %>%
  mutate_at(vars("account_number",
                 "cons_level_amount"), as.numeric)

usage_bill <- usage_info %>%
  group_by(account_number, bill_run_date) %>%
  summarise(usage_bill_amount=sum(bc_detail_amount, na.rm=TRUE))

rct_bill <- rct_bill %>%
  left_join(usage_bill, by=c("account_number", "bill_date"="bill_run_date")) %>%
  filter(account_number %in% rct_subjects$account_number,
         amount_paid<=0, usage_bill_amount>0) %>%
  mutate(pay_share_current=-amount_paid/usage_bill_amount,
         pay_share_all=-amount_paid/previous_bill_amount) %>%
  left_join(rct_subjects, by="account_number")

coef_graph <- function(df, filename) {
  fit <- lm(pay_share_current ~ factor(discount_percentage) - 1, data=df)
  coefs <- summary(fit)
  results <- data.frame(estimate=coefs$coefficients[,1],
                        std.error=coefs$coefficients[,2],
                        discount_percentage=0:8/10)
  
  gg <- ggplot(data=results, 
               aes(x=discount_percentage, y=estimate)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=estimate-1.96*std.error,
                      ymax=estimate+1.96*std.error),
                  width=0.02, colour="orange") +
    scale_x_continuous(breaks=0:8/10,
                       minor_breaks=0:8/10,
                       label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(x="Discount Percentage", 
         y="Payment Percentage") +
    fte_theme() +
    theme(plot.title=element_text(size=18, hjust=0.5),
          plot.subtitle=element_text(size=14, hjust=0.5),
          text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.position="bottom")
  gg
  ggsave(gg, 
         file=paste0(output_dir, "/figures/coefs_pay_share_current_", filename, ".png"), 
         width=6, height=4)
  
  fit <- lm(pay_full_current ~ factor(discount_percentage) - 1, data=df)
  coefs <- summary(fit)
  results <- data.frame(estimate=coefs$coefficients[,1],
                        std.error=coefs$coefficients[,2],
                        discount_percentage=0:8/10)
  
  gg <- ggplot(data=results, 
               aes(x=discount_percentage, y=estimate)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=estimate-1.96*std.error,
                      ymax=estimate+1.96*std.error),
                  width=0.02, colour="orange") +
    scale_x_continuous(breaks=0:8/10,
                       minor_breaks=0:8/10,
                       label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(x="Discount Percentage", 
         y="Share Paying Full Amount") +
    fte_theme() +
    theme(plot.title=element_text(size=18, hjust=0.5),
          plot.subtitle=element_text(size=14, hjust=0.5),
          text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.position="bottom")
  gg
  ggsave(gg,
         file=paste0(output_dir, "/figures/coefs_pay_full_current_", filename, ".png"), 
         width=6, height=4)
  
  fit <- lm(pay_none_current ~ factor(discount_percentage) - 1, data=df)
  coefs <- summary(fit)
  results <- data.frame(estimate=coefs$coefficients[,1],
                        std.error=coefs$coefficients[,2],
                        discount_percentage=0:8/10)
  
  gg <- ggplot(data=results, 
               aes(x=discount_percentage, y=estimate)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=estimate-1.96*std.error,
                      ymax=estimate+1.96*std.error),
                  width=0.02, colour="orange") +
    scale_x_continuous(breaks=0:8/10,
                       minor_breaks=0:8/10,
                       label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(x="Discount Percentage", 
         y="Share Paying Nothing") +
    fte_theme() +
    theme(plot.title=element_text(size=18, hjust=0.5),
          plot.subtitle=element_text(size=14, hjust=0.5),
          text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.position="bottom")
  gg
  ggsave(gg,
         file=paste0(output_dir, "/figures/coefs_pay_zero_current_", filename, ".png"), 
         width=6, height=4)
  
  fit <- lm(pay_share_all ~ factor(discount_percentage) - 1, data=df)
  coefs <- summary(fit)
  results <- data.frame(estimate=coefs$coefficients[,1],
                        std.error=coefs$coefficients[,2],
                        discount_percentage=0:8/10)
  
  gg <- ggplot(data=results, 
               aes(x=discount_percentage, y=estimate)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=estimate-1.96*std.error,
                      ymax=estimate+1.96*std.error),
                  width=0.02, colour="orange") +
    scale_x_continuous(breaks=0:8/10,
                       minor_breaks=0:8/10,
                       label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(x="Discount Percentage", 
         y="Payment Percentage") +
    fte_theme() +
    theme(plot.title=element_text(size=18, hjust=0.5),
          plot.subtitle=element_text(size=14, hjust=0.5),
          text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.position="bottom")
  gg
  ggsave(gg, 
         file=paste0(output_dir, "/figures/coefs_pay_share_all_", filename, ".png"), 
         width=6, height=4)
  
  fit <- lm(pay_full_all ~ factor(discount_percentage) - 1, data=df)
  coefs <- summary(fit)
  results <- data.frame(estimate=coefs$coefficients[,1],
                        std.error=coefs$coefficients[,2],
                        discount_percentage=0:8/10)
  
  gg <- ggplot(data=results, 
               aes(x=discount_percentage, y=estimate)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=estimate-1.96*std.error,
                      ymax=estimate+1.96*std.error),
                  width=0.02, colour="orange") +
    scale_x_continuous(breaks=0:8/10,
                       minor_breaks=0:8/10,
                       label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(x="Discount Percentage", 
         y="Share Paying Full Amount") +
    fte_theme() +
    theme(plot.title=element_text(size=18, hjust=0.5),
          plot.subtitle=element_text(size=14, hjust=0.5),
          text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.position="bottom")
  gg
  ggsave(gg,
         file=paste0(output_dir, "/figures/coefs_pay_full_all_", filename, ".png"), 
         width=6, height=4)
  
  fit <- lm(pay_none_all ~ factor(discount_percentage) - 1, data=df)
  coefs <- summary(fit)
  results <- data.frame(estimate=coefs$coefficients[,1],
                        std.error=coefs$coefficients[,2],
                        discount_percentage=0:8/10)
  
  gg <- ggplot(data=results, 
               aes(x=discount_percentage, y=estimate)) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=estimate-1.96*std.error,
                      ymax=estimate+1.96*std.error),
                  width=0.02, colour="orange") +
    scale_x_continuous(breaks=0:8/10,
                       minor_breaks=0:8/10,
                       label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(x="Discount Percentage", 
         y="Share Paying Nothing") +
    fte_theme() +
    theme(plot.title=element_text(size=18, hjust=0.5),
          plot.subtitle=element_text(size=14, hjust=0.5),
          text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.position="bottom")
  gg
  ggsave(gg,
         file=paste0(output_dir, "/figures/coefs_pay_zero_all_", filename, ".png"), 
         width=6, height=4)
}

rct_bill_winter <- rct_bill %>%
  filter(bill_date>=mdy("12/12/2024"), !(account_number %in% monthly_accounts$account_number)) %>%
  mutate(pay_share_current=ifelse(pay_share_current>1, 1, pay_share_current),
         pay_full_current=pay_share_current>=1,
         pay_none_current=pay_share_current<=0,
         pay_share_all=ifelse(pay_share_all>1, 1, pay_share_all),
         pay_share_all=ifelse(pay_share_all<0, 0, pay_share_all),
         pay_full_all=pay_share_all>=1,
         pay_none_all=pay_share_all<=0)

coef_graph(rct_bill_fall, "winter")
