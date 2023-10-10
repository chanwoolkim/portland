# Additional analysis on financial assistance

load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))
load(file=paste0(working_data_dir, "/portland_panel.RData"))


# Account that received financial assistance ####
# Only consider last available quarter
financial_assist_detail <- financial_assist_detail %>%
  filter(BILL_DT>=mdy("03/01/2023"))

# Only consider accounts in the panel dataset
financial_assist_detail <- financial_assist_detail %>%
  filter(ACCOUNT_NO %in% portland_panel$ACCOUNT_NO)

# Percentage discounts
financial_assist_detail_stat <- financial_assist_detail %>%
  filter(!is.na(LINC_TIER_TYPE)) %>%
  mutate(discount_rate=LINC_DISCOUNT_AMT*(-1)/BILLED_AMT_BEFORE_DIS,
         discount_rate=case_when(discount_rate<0 ~ 0,
                                 discount_rate>1 ~ 1,
                                 .default=discount_rate)) %>%
  group_by(LINC_TIER_TYPE) %>%
  summarise(mean_discount_rate=mean(discount_rate, na.rm=TRUE),
            median_discount_rate=median(discount_rate, na.rm=TRUE),
            sd_discount_rate=sd(discount_rate, na.rm=TRUE),
            min_discount_rate=min(discount_rate, na.rm=TRUE),
            max_discount_rate=max(discount_rate, na.rm=TRUE),
            n=n_distinct(ACCOUNT_NO)) %>%
  ungroup()

tab <- TexRow(c("Tier", "Mean", "Median", "SD", "Min", "Max", "N")) +
  TexMidrule() +
  TexRow(c("Tier 1",
           paste0(round((financial_assist_detail_stat[1, 2:6] %>%
                           as.numeric())*100, 2), "\\%"),
           round(financial_assist_detail_stat[1, 7]) %>% as.numeric())) +
  TexRow(c("Tier 2",
           paste0(round((financial_assist_detail_stat[2, 2:6] %>%
                           as.numeric())*100, 2), "\\%"),
           round(financial_assist_detail_stat[2, 7]) %>% as.numeric()))

tab
TexSave(tab,
        filename="assistance_discount",
        positions=c("l", "c", "c", "c", "c", "c", "c"),
        pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Average payment by tier and special status
financial_assist_detail_stat <- financial_assist_detail %>%
  filter(!is.na(LINC_TIER_TYPE)) %>%
  group_by(LINC_TIER_TYPE, senior_disabilities) %>%
  summarise(mean_bill=mean(NET_BILL_AMT, na.rm=TRUE),
            median_bill=median(NET_BILL_AMT, na.rm=TRUE),
            sd_bill=sd(NET_BILL_AMT, na.rm=TRUE),
            min_bill=min(NET_BILL_AMT, na.rm=TRUE),
            max_bill=max(NET_BILL_AMT, na.rm=TRUE),
            n=n_distinct(ACCOUNT_NO)) %>%
  ungroup()

tab <- TexRow(c("Tier", "Senior/Disabilities",
                "Mean", "Median", "SD", "Min", "Max", "N")) +
  TexMidrule() +
  TexRow(c("Tier 1", "No",
           paste0("\\$", round((financial_assist_detail_stat[1, 3:7] %>%
                                  as.numeric()))),
           round(financial_assist_detail_stat[1, 8]) %>% as.numeric())) +
  TexRow(c("Tier 1", "Yes",
           paste0("\\$", round((financial_assist_detail_stat[2, 3:7] %>%
                                  as.numeric()))),
           round(financial_assist_detail_stat[2, 8]) %>% as.numeric())) +
  TexRow(c("Tier 2", "No",
           paste0("\\$", round((financial_assist_detail_stat[3, 3:7] %>%
                                  as.numeric()))),
           round(financial_assist_detail_stat[3, 8]) %>% as.numeric())) +
  TexRow(c("Tier 2", "Yes",
           paste0("\\$", round((financial_assist_detail_stat[4, 3:7] %>%
                                  as.numeric()))),
           round(financial_assist_detail_stat[4, 8]) %>% as.numeric()))

tab
TexSave(tab,
        filename="assistance_bill",
        positions=c("l", "c", "c", "c", "c", "c", "c", "c"),
        pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

gg <- ggplot(financial_assist_detail %>%
               filter(LINC_TIER_TYPE=="Tier1") %>%
               mutate(NET_BILL_AMT=ifelse(NET_BILL_AMT>500, 500, NET_BILL_AMT)),
             aes(x=NET_BILL_AMT, fill=senior_disabilities)) + 
  geom_histogram(aes(y=after_stat(density)), bins=30, position="dodge") +
  fte_theme() +
  xlab("Net Bill Amount ($)") + ylab("Proportion (%)") +
  scale_y_continuous(labels=scales::percent) +
  labs(fill="Senior/Disabilities") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/assistance_tier1_bill_hist.png"),
       width=6, height=4)

gg <- ggplot(financial_assist_detail %>%
               filter(LINC_TIER_TYPE=="Tier2") %>%
               mutate(NET_BILL_AMT=ifelse(NET_BILL_AMT>500, 500, NET_BILL_AMT)),
             aes(x=NET_BILL_AMT, fill=senior_disabilities)) + 
  geom_histogram(aes(y=after_stat(density)), bins=30, position="dodge") +
  fte_theme() +
  xlab("Net Bill Amount ($)") + ylab("Proportion (%)") +
  scale_y_continuous(labels=scales::percent) +
  labs(fill="Senior/Disabilities") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/assistance_tier2_bill_hist.png"),
       width=6, height=4)
