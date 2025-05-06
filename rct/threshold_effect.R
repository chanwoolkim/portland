load(paste0(working_data_dir, "/estimation_dataset_all.RData"))
estimation_dataset_all <- estimation_dataset %>%
  filter(account_status!="FINAL",
         !is_rebill,
         !first_bill,
         service_water, service_sewer, service_storm)

load(paste0(working_data_dir, "/estimation_dataset.RData"))

# Filter
estimation_dataset <- estimation_dataset %>%
  filter(is.na(exit_reason),
         account_status!="FINAL",
         !is_rebill,
         !first_bill,
         (fa_type!="Tier2" | is.na(fa_type)))

threshold_data <- estimation_dataset %>%
  group_by(id) %>%
  filter(any(t==1, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(service_water, service_sewer, service_storm,
         t==0, B_t>0) %>%
  bind_rows(estimation_dataset_all %>%
              group_by(id) %>%
              filter(any(t==1, na.rm=TRUE)) %>%
              ungroup() %>%
              filter(t==0, B_t>0, linc_discount==0,
                     !id %in% estimation_dataset$id) %>%
              mutate(discount_grid=0)) %>%
  mutate(bill=B_t,
         payshare=-E_t/bill,
         payshare=ifelse(bill<=0, NA, payshare),
         payshare=round(payshare, 2),
         delinquent=payshare<1,
         paid_something=payshare>0,
         paid_nothing=payshare<0.01)

# Threshold from RCT
threshold_data <- threshold_data %>%
  filter(D_t==0) %>%
  mutate(below_115=bill<115,
         below_300=bill<300)

original_bill <- threshold_data %>%
  group_by(lag_w_t, discount_grid) %>%
  summarise(mean_bill=mean(bill, na.rm=TRUE),
            sd_bill=sd(bill, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  filter(lag_w_t>1, lag_w_t<=18)

original_n <- original_bill %>%
  group_by(lag_w_t) %>%
  summarise(n=sum(n, na.rm=TRUE))

gg <- ggplot(original_bill, 
             aes(x=lag_w_t, y=mean_bill, colour=factor(discount_grid))) +
  geom_line() +
  geom_point(size=3) +
  scale_x_continuous(breaks=0:18,
                     minor_breaks=0:18) +
  scale_y_continuous(label=scales::dollar) +
  scale_colour_manual(values=colours_set_sequential) +
  labs(x="Water Use (in ccf)",
       y="Average Bill Amount",
       title="Average Bill Amount by Water Usage",
       colour="Discount Level") +
  geom_hline(yintercept=115, colour="darkred", linetype="dashed") +
  annotate("label", x=17, y=115, label="Late Notice Threshold",
           colour="darkred", size=4, family="serif") +
  geom_hline(yintercept=300, colour="darkred", linetype="dashed") +
  annotate("label", x=3, y=300, label="Shut-Off Threshold",
           colour="darkred", size=4, family="serif") +
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
  guides(colour=guide_legend(nrow = 1)) +
  annotate("label", x=2, y=550, label=paste0("n=", original_n$n[original_n$lag_w_t==2]),
           colour="black", size=4, family="serif")

for (n in 3:18) {
  gg <- gg +
    annotate("label", x=n, y=550, label=original_n$n[original_n$lag_w_t==n],
             colour="black", size=4, family="serif")
}

gg
ggsave(gg, filename=paste0(output_dir, "/figures/bill_discount_usage.png"),
       width=12, height=8)

threshold_summary <- threshold_data %>%
  group_by(lag_w_t, below_300) %>%
  summarise(mean_delinquent=mean(delinquent, na.rm=TRUE),
            sd_delinquent=sd(delinquent, na.rm=TRUE),
            mean_paid_something=mean(paid_something, na.rm=TRUE),
            sd_paid_something=sd(paid_something, na.rm=TRUE),
            mean_paid_nothing=mean(paid_nothing, na.rm=TRUE),
            sd_paid_nothing=sd(paid_nothing, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(se_delinquent=sd_delinquent/sqrt(n),
         lower_bound_delinquent=mean_delinquent-1.96*se_delinquent,
         upper_bound_delinquent=mean_delinquent+1.96*se_delinquent,
         se_paid_something=sd_paid_something/sqrt(n),
         lower_bound_paid_something=mean_paid_something-1.96*se_paid_something,
         upper_bound_paid_something=mean_paid_something+1.96*se_paid_something,
         se_paid_nothing=sd_paid_nothing/sqrt(n),
         lower_bound_paid_nothing=mean_paid_nothing-1.96*se_paid_nothing,
         upper_bound_paid_nothing=mean_paid_nothing+1.96*se_paid_nothing,
         bill_group=ifelse(below_300, "Below", "Above"))

threshold_summary_plot <- threshold_summary %>%
  filter(lag_w_t>=7, lag_w_t<=18)

gg <- ggplot(threshold_summary_plot,
             aes(x=lag_w_t, y=mean_delinquent, fill=bill_group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower_bound_delinquent, ymax=upper_bound_delinquent),
                width=0.2, size=1, position=position_dodge(0.9)) +
  scale_x_continuous(breaks=7:18,
                     minor_breaks=7:18) +
  scale_y_continuous(breaks=seq(0, 0.225, 0.05),
                     limits=c(0, 0.225)) +
  scale_fill_manual(values=colours_set) +
  labs(x="Water Use (in ccf)",
       y="Probability of Not Paying Fully",
       fill="Threshold",
       title="Threshold Effect on Delinquency (Shut-Offs)") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg, filename=paste0(output_dir, "/figures/threshold_effect_shut_off_delinquency.png"),
       width=12, height=8)

gg <- ggplot(threshold_summary_plot,
             aes(x=lag_w_t, y=mean_paid_something, fill=bill_group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower_bound_paid_something, ymax=upper_bound_paid_something),
                width=0.2, size=1, position=position_dodge(0.9)) +
  scale_x_continuous(breaks=7:18,
                     minor_breaks=7:18) +
  scale_y_continuous(breaks=seq(0.85, 1.005, 0.05),
                     minor_breaks=seq(0.85, 1.005, 0.01))  +
  coord_cartesian(ylim=c(0.85, 1.005)) +
  scale_fill_manual(values=colours_set) +
  labs(x="Water Use (in ccf)",
       y="Probability of Paying Something",
       fill="Threshold",
       title="Threshold Effect on Paying Something (Shut-Offs)") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg, filename=paste0(output_dir, "/figures/threshold_effect_shut_off_paid_something.png"),
       width=12, height=8)

threshold_summary_revenue <- threshold_data %>%
  filter(bill>280, bill<320) %>%
  group_by(lag_w_t, below_300) %>%
  summarise(mean_revenue=mean(-E_t, na.rm=TRUE),
            sd_revenue=sd(-E_t, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(se_revenue=sd_revenue/sqrt(n),
         lower_bound_revenue=mean_revenue-1.96*se_revenue,
         upper_bound_revenue=mean_revenue+1.96*se_revenue) %>%
  filter(lag_w_t>=7, lag_w_t<=18) %>%
  mutate(bill_group=ifelse(below_300, "Below", "Above"))

gg <- ggplot(threshold_summary_revenue,
             aes(x=lag_w_t, y=mean_revenue, fill=bill_group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower_bound_revenue, ymax=upper_bound_revenue),
                width=0.2, size=1, position=position_dodge(0.9)) +
  scale_x_continuous(breaks=7:18,
                     minor_breaks=7:18) +
  scale_y_continuous(breaks=seq(200, 350, 50),
                     label=scales::dollar) +
  coord_cartesian(ylim=c(200, 350)) +
  scale_fill_manual(values=colours_set) +
  labs(x="Water Use (in ccf)",
       y="Revenue",
       fill="Threshold",
       title="Threshold Effect on Revenue (Shut-Offs)") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg, filename=paste0(output_dir, "/figures/threshold_effect_shut_off_revenue.png"),
       width=12, height=8)

threshold_summary <- threshold_data %>%
  group_by(lag_w_t, below_115) %>%
  summarise(mean_delinquent=mean(delinquent, na.rm=TRUE),
            sd_delinquent=sd(delinquent, na.rm=TRUE),
            mean_paid_something=mean(paid_something, na.rm=TRUE),
            sd_paid_something=sd(paid_something, na.rm=TRUE),
            mean_paid_nothing=mean(paid_nothing, na.rm=TRUE),
            sd_paid_nothing=sd(paid_nothing, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(se_delinquent=sd_delinquent/sqrt(n),
         lower_bound_delinquent=mean_delinquent-1.96*se_delinquent,
         upper_bound_delinquent=mean_delinquent+1.96*se_delinquent,
         se_paid_something=sd_paid_something/sqrt(n),
         lower_bound_paid_something=mean_paid_something-1.96*se_paid_something,
         upper_bound_paid_something=mean_paid_something+1.96*se_paid_something,
         se_paid_nothing=sd_paid_nothing/sqrt(n),
         lower_bound_paid_nothing=mean_paid_nothing-1.96*se_paid_nothing,
         upper_bound_paid_nothing=mean_paid_nothing+1.96*se_paid_nothing,
         bill_group=ifelse(below_115, "Below", "Above"))

threshold_summary_plot <- threshold_summary%>%
  filter(lag_w_t>=2, lag_w_t<=9)

gg <- ggplot(threshold_summary_plot,
             aes(x=lag_w_t, y=mean_delinquent, fill=bill_group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower_bound_delinquent, ymax=upper_bound_delinquent),
                width=0.2, size=1, position=position_dodge(0.9)) +
  scale_x_continuous(breaks=2:9,
                     minor_breaks=2:9) +
  scale_y_continuous(breaks=seq(0, 0.25, 0.05),
                     limits=c(0, 0.25)) +
  scale_fill_manual(values=colours_set) +
  labs(x="Water Use (in ccf)",
       y="Probability of Not Paying Fully",
       fill="Threshold",
       title="Threshold Effect on Delinquency (Late Notice)") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg, filename=paste0(output_dir, "/figures/threshold_effect_late_notice_delinquency.png"),
       width=12, height=8)

gg <- ggplot(threshold_summary_plot,
             aes(x=lag_w_t, y=mean_paid_something, fill=bill_group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower_bound_paid_something, ymax=upper_bound_paid_something),
                width=0.2, size=1, position=position_dodge(0.9)) +
  scale_x_continuous(breaks=2:9,
                     minor_breaks=2:9) +
  scale_y_continuous(breaks=seq(0.85, 1.005, 0.05),
                     minor_breaks=seq(0.85, 1.005, 0.01))  +
  coord_cartesian(ylim=c(0.85, 1.005)) +
  scale_fill_manual(values=colours_set) +
  labs(x="Water Use (in ccf)",
       y="Probability of Paying Something",
       fill="Threshold",
       title="Threshold Effect on Paying Something (Late Notice)") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg, filename=paste0(output_dir, "/figures/threshold_effect_late_notice_paid_something.png"),
       width=12, height=8)

threshold_revenue <- threshold_data %>%
  filter(bill>105, bill<135) %>%
  group_by(lag_w_t, below_115) %>%
  summarise(mean_revenue=mean(-E_t, na.rm=TRUE),
            sd_revenue=sd(-E_t, na.rm=TRUE),
            n=n()) %>%
  ungroup() %>%
  mutate(se_revenue=sd_revenue/sqrt(n),
         lower_bound_revenue=mean_revenue-1.96*se_revenue,
         upper_bound_revenue=mean_revenue+1.96*se_revenue) %>%
  filter(lag_w_t>=2, lag_w_t<=9) %>%
  mutate(bill_group=ifelse(below_115, "Below", "Above"))

gg <- ggplot(threshold_revenue,
             aes(x=lag_w_t, y=mean_revenue, fill=bill_group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower_bound_revenue, ymax=upper_bound_revenue),
                width=0.2, size=1, position=position_dodge(0.9)) +
  scale_x_continuous(breaks=2:9,
                     minor_breaks=2:9) +
  scale_y_continuous(breaks=seq(50, 150, 10),
                     label=scales::dollar) +
  coord_cartesian(ylim=c(50, 150)) +
  scale_fill_manual(values=colours_set) +
  labs(x="Water Use (in ccf)",
       y="Revenue",
       fill="Threshold",
       title="Threshold Effect on Revenue (Late Notice)") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg, filename=paste0(output_dir, "/figures/threshold_effect_late_notice_revenue.png"),
       width=12, height=8)


# All Bills ####
estimation_dataset_all <- estimation_dataset_all %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  fill(income, .direction="downup") %>%
  ungroup()

threshold_data <- estimation_dataset_all %>%
  filter(t==0,
         due_date<="2025-05-01",
         lag_w_t>0,
         !is.na(income)) %>%
  mutate(bill=O_t,
         income=income*4/1000,
         payshare=-E_t/bill,
         payshare=ifelse(bill<=0, NA, payshare),
         payshare=round(payshare, 2),
         delinquent=payshare<1,
         paid_something=payshare>0,
         paid_nothing=payshare<0.01,
         payment=-E_t)

threshold_data <- threshold_data %>%
  mutate(payment_plan=ifelse(is.na(payment_plan), FALSE, payment_plan),
         got_crisis_voucher=crisis_voucher<0)

outcome_variables <- c("payment", "delinquent", "paid_something", "paid_nothing", "lag_w_t", "income", "payment_plan", "got_crisis_voucher")
outcome_labels <- c("Revenue", 
                    "Probability of Not Paying Fully",
                    "Probability of Paying Something",
                    "Probability of Paying Nothing",
                    "Water Use (in ccf)",
                    "Income ($ thousands)")
outcome_bw <- c(5, 5, 5, 5, 30, 20)

threshold_data_300 <- threshold_data %>%  
  filter(bill>290, bill<310)

outcome_lower_y <- c(280, 0.03, 0.97, 0.005, 7.5, 25*4)
outcome_upper_y <- c(310, 0.065, 0.995, 0.03, 9.5, 35*4)

for (i in 1:length(outcome_variables)) {
  bins <- binscatter(threshold_data_300, 
                     y=outcome_variables[i], x="bill",
                     n.cut=5, threshold=300)
  
  linf <- loc_lin(threshold_data_300 %>% mutate(x=bill),
                  y=outcome_variables[i], 
                  gran=15, bw=outcome_bw[i], range=c(290, 310), cutoff=300)
  
  gg <- ggplot(bins$df_bin, aes(x=x, y=y)) +
    geom_point(color="navy", size=3) +
    geom_errorbar(aes(ymin=y_lower, ymax=y_upper),
                  width=0.2, size=1, alpha=0.7) +
    geom_line(data=linf %>% 
                bind_rows(.id="side"),
              aes(x=x, y=y, group=side),
              color="red") +
    scale_x_continuous(breaks=seq(290,310, 5),
                       minor_breaks=290:310,
                       label=scales::dollar) +
    scale_y_continuous(limits=c(outcome_lower_y[i], outcome_upper_y[i])) +
    labs(x="Bill",
         y=outcome_labels[i],
         title=paste0("Threshold Effect on ", outcome_labels[i], " (Shut-Offs)")) +
    fte_theme() +
    theme(plot.title=element_text(size=24, hjust=0.5),
          plot.subtitle=element_text(size=18, hjust=0.5),
          text=element_text(size=18),
          axis.text=element_text(size=18),
          axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          legend.title=element_text(size=18),
          legend.text=element_text(size=18),
          legend.position="bottom")
  
  if (outcome_variables[i] %in% c("payment", "income")) {
    gg <- gg + scale_y_continuous(label=scales::dollar,
                                  limits=c(outcome_lower_y[i], outcome_upper_y[i]))
  }
  
  ggsave(gg, 
         filename=paste0(output_dir, "/figures/rd_threshold_effect_", outcome_variables[i], "_shut_off.png"),
         width=12, height=8)
}

threshold_data_115 <- threshold_data %>%  
  filter(bill>105, bill<125)

outcome_lower_y <- c(100, 0.115 ,0.85, 0.08, 10.5, 20*4)
outcome_upper_y <- c(125, 0.2, 0.92, 0.15, 12, 30*4)

for (i in 1:length(outcome_variables)) {
  bins <- binscatter(threshold_data_115, 
                     y=outcome_variables[i], x="bill",
                     n.cut=5, threshold=115)
  
  linf <- loc_lin(threshold_data_115 %>% mutate(x=bill),
                  y=outcome_variables[i], 
                  gran=15, bw=outcome_bw[i], range=c(105, 125), cutoff=115)
  
  gg <- ggplot(bins$df_bin, aes(x=x, y=y)) +
    geom_point(color="navy", size=3) +
    geom_line(data=linf %>% 
                bind_rows(.id="side"),
              aes(x=x, y=y, group=side),
              color="red")  +
    scale_x_continuous(breaks=seq(105,125, 5),
                       minor_breaks=105:125,
                       label=scales::dollar) +
    scale_y_continuous(limits=c(outcome_lower_y[i], outcome_upper_y[i])) +
    labs(x="Bill",
         y=outcome_labels[i],
         title=paste0("Threshold Effect on ", outcome_labels[i], " (Late Notice)")) +
    fte_theme() +
    theme(plot.title=element_text(size=24, hjust=0.5),
          plot.subtitle=element_text(size=18, hjust=0.5),
          text=element_text(size=18),
          axis.text=element_text(size=18),
          axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          legend.title=element_text(size=18),
          legend.text=element_text(size=18),
          legend.position="bottom")
  
  if (outcome_variables[i] %in% c("payment", "income")) {
    gg <- gg + scale_y_continuous(label=scales::dollar,
                                  limits=c(outcome_lower_y[i], outcome_upper_y[i]))
  }
  
  ggsave(gg, 
         filename=paste0(output_dir, "/figures/rd_threshold_effect_", outcome_variables[i], "_late_notice.png"),
         width=12, height=8)
}