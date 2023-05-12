# Additional analysis on financial assistance

load(file=paste0(working_data_dir, "/account_info_analysis.RData"))

financial_assist_detail <- list.files(path=data_dir,
                                      pattern="*.csv",
                                      full.names=TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

financial_assist_detail <- financial_assist_detail %>%
  filter(FINAL=="N") %>%
  mutate(bill_year=year(mdy(BILL_DT)))


# Account that received financial assistance ####
# Only consider single family
account_info_subset <- account_info_merge %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST")) %>%
  select(ACCOUNT_NO) %>%
  mutate(account=TRUE)

financial_assist_detail <-
  left_join(financial_assist_detail %>%
              mutate(ACCOUNT_NO=as.character(ACCOUNT_NO)),
            account_info_subset,
            by="ACCOUNT_NO") %>%
  filter(account)

financial_assist_detail_agg <- financial_assist_detail %>%
  group_by(ACCOUNT_NO, LINC_TIER_TYPE) %>%
  summarise(BILLED_AMT_BEFORE_DIS=sum(BILLED_AMT_BEFORE_DIS, na.rm=TRUE),
            LINC_DISCOUNT_AMT=sum(LINC_DISCOUNT_AMT, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(discount_rate=LINC_DISCOUNT_AMT*(-1)/BILLED_AMT_BEFORE_DIS) %>%
  filter(!is.na(LINC_TIER_TYPE))

financial_assist_detail_stat <- financial_assist_detail_agg %>%
  group_by(LINC_TIER_TYPE) %>%
  summarise(mean_discount_rate=mean(discount_rate, na.rm=TRUE),
            median_discount_rate=median(discount_rate, na.rm=TRUE),
            sd_discount_rate=sd(discount_rate, na.rm=TRUE),
            min_discount_rate=min(discount_rate, na.rm=TRUE),
            max_discount_rate=max(discount_rate, na.rm=TRUE),
            n=n()) %>%
  ungroup()

tab <- TR(c("Tier", "Mean", "Median", "SD", "Min", "Max")) +
  midrule() +
  TR("Tier 1") %:%
  TR(paste0(round((financial_assist_detail_stat[1, 2:6] %>% as.numeric())*100, 2), "\\%")) +
  TR("Tier 2") %:%
  TR(paste0(round((financial_assist_detail_stat[2, 2:6] %>% as.numeric())*100, 2), "\\%"))

tab
TS(tab, file="assistance_discount", header=c("lccccc"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)
