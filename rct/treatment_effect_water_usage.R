usage_info <- read_csv(paste0(working_data_dir, "/servus_query/rct_usage.csv"))
rct_subjects <- read_csv(paste0(working_data_dir, "/servus_query/account_rct.csv"))

rct_subjects <- rct_subjects %>%
  filter(linc_tier_type!="Tier2" | is.na(linc_tier_type)) %>%
  distinct()

rct_total <- rct_subjects %>%
  group_by(discount_percentage) %>%
  summarise(n_total=n_distinct(account_number, na.rm=TRUE))

usage_info <- usage_info %>%
  mutate_at(vars("account_number",
                 "cons_level_amount"), as.numeric) %>%
  filter(report_context=="WCONS", cons_level_amount>0)

usage_info <- usage_info %>%
  group_by(account_number, bill_run_date) %>%
  summarise(cons_level_amount=sum(cons_level_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(account_number, bill_run_date) %>%
  group_by(account_number) %>%
  mutate(prev_cons=lag(cons_level_amount)) %>%
  ungroup() %>%
  filter(bill_run_date>=mdy("12/12/2024"),
         bill_run_date<mdy("02/10/2025"))

usage_merge <- usage_info %>%
  filter(account_number %in% rct_subjects$account_number, 
         cons_level_amount<9000, prev_cons<9000) %>%
  left_join(rct_subjects, by="account_number")

fit <- lm(cons_level_amount ~ factor(discount_percentage) - 1, data=usage_merge)
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
  labs(x="Discount Percentage", 
       y="Average Consumption (in ccf)") +
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
ggsave(gg, file=paste0(output_dir, "/figures/coefs_usage.png"), width=6, height=4)

fit <- lm(cons_level_amount ~ factor(discount_percentage) + prev_cons - 1, data=usage_merge)
coefs <- summary(fit)
results <- data.frame(estimate=coefs$coefficients[1:9,1],
                      std.error=coefs$coefficients[1:9,2],
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
  labs(x="Discount Percentage", 
       y="Coefficients (conditioning on the lagged outcome)") +
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
ggsave(gg, file=paste0(output_dir, "/figures/coefs_usage_lag.png"), width=6, height=4)
