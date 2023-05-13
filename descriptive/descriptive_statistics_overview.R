# Descriptive Statistics

load(file=paste0(working_data_dir, "/account_info_analysis.RData"))


# First pass on the account info ####
account_info_all <- account_info %>%
  filter(BILLING_STAT=="REGLR") %>%
  mutate(
    ACCOUNT_CLASS_DFLT=trimws(ACCOUNT_CLASS_DFLT),
    class_description=case_when(
      ACCOUNT_CLASS_DFLT=="RESSF" ~ "Residential Single Family",
      ACCOUNT_CLASS_DFLT=="RESMF" ~ "Residential Multi Family",
      ACCOUNT_CLASS_DFLT=="COMM" ~ "Commercial",
      ACCOUNT_CLASS_DFLT=="ASST" ~ "Residential Single Family on Assistance",
      ACCOUNT_CLASS_DFLT=="SWRLT" ~ "Sewer Only Tenant",
      ACCOUNT_CLASS_DFLT=="SWRLO" ~ "Sewer Only Owner",
      ACCOUNT_CLASS_DFLT=="URGNT" ~ "Urgent",
      ACCOUNT_CLASS_DFLT=="RMNDR" ~ "Reminder",
      ACCOUNT_CLASS_DFLT=="MISAR" ~ "Temporary"))

account_info_all <- account_info_all %>%
  group_by(class_description) %>%
  summarise(count=n())

account_info_all <- account_info_all %>%
  mutate(proportion=count/sum(account_info_all$count)*100) %>%
  arrange(desc(proportion)) %>%
  mutate(class_description=ifelse(is.na(class_description), "NA", class_description))

tab_row <- function(row) {
  out <- TR(account_info_all[row, 1] %>% as.character()) %:%
    TR(account_info_all[row, 2:3] %>% as.numeric(), dec=c(5, 2))
  return(out)
}

tab <- TR(c("Category", "Count", "Proportion")) +
  midrule() +
  tab_row(1) + tab_row(2) + tab_row(3) + tab_row(4) +
  tab_row(5) + tab_row(6) + tab_row(7) + tab_row(8) +
  midrule() +
  TR("Total") %:%
  TR(c(sum(account_info_all$count), sum(account_info_all$proportion)), dec=c(5, 2))

tab <- fix_0(tab)
TS(tab, file="account_all", header=c("l|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Total delinquent amount decomposition
delinquency_amount_type <- account_info_merge %>%
  mutate(ACCOUNT_CLASS_DFLT=ifelse(ACCOUNT_CLASS_DFLT=="ASST",
                                   "RESSF",
                                   ACCOUNT_CLASS_DFLT)) %>%
  group_by(ACCOUNT_CLASS_DFLT) %>%
  summarise(delinquent_amount=sum(delinquent_amount, na.rm=TRUE)) %>%
  mutate(class_description=case_when(
    ACCOUNT_CLASS_DFLT=="RESSF" ~ "Residential Single Family",
    ACCOUNT_CLASS_DFLT=="RESMF" ~ "Residential Multi Family"),
    delinquent_amount_round=str_c("$",
                                  round(delinquent_amount/1000000, 2),
                                  " Million"))

gg <- ggplot(delinquency_amount_type,
             aes(x="", y=delinquent_amount, fill=class_description)) + 
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label=delinquent_amount_round),
            position=position_stack(vjust=0.5),
            color="white", size=4, family="serif") +
  coord_polar("y", start=0) +
  pie_theme() +
  labs(fill="Account Type")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/delinquent_amount_pie.png"),
       width=6, height=4)

delinquency_amount_type <- account_info_merge %>%
  mutate(ACCOUNT_CLASS_DFLT=ifelse(ACCOUNT_CLASS_DFLT=="ASST",
                                   "RESSF",
                                   ACCOUNT_CLASS_DFLT)) %>%
  group_by(ACCOUNT_CLASS_DFLT) %>%
  summarise(delinquentamount_all=sum(delinquent_amount, na.rm=TRUE),
            delinquentamount_2019=sum(delinquent_amount_2019, na.rm=TRUE),
            delinquentamount_2020=sum(delinquent_amount_2020, na.rm=TRUE),
            delinquentamount_2021=sum(delinquent_amount_2021, na.rm=TRUE),
            delinquentamount_2022=sum(delinquent_amount_2022, na.rm=TRUE),
            totalbill_all=sum(total_bill, na.rm=TRUE),
            totalbill_2019=sum(total_bill_2019, na.rm=TRUE),
            totalbill_2020=sum(total_bill_2020, na.rm=TRUE),
            totalbill_2021=sum(total_bill_2021, na.rm=TRUE),
            totalbill_2022=sum(total_bill_2022, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquentpercent_all=delinquentamount_all/totalbill_all,
         delinquentpercent_2019=delinquentamount_2019/totalbill_2019,
         delinquentpercent_2020=delinquentamount_2020/totalbill_2020,
         delinquentpercent_2021=delinquentamount_2021/totalbill_2021,
         delinquentpercent_2022=delinquentamount_2022/totalbill_2022) %>%
  ungroup() %>%
  pivot_longer(cols=c(starts_with("delinquentamount"),
                      starts_with("totalbill"),
                      starts_with("delinquentpercent")),
               names_to=c(".value", "year"),
               names_sep="_") %>%
  pivot_wider(id_cols=year, 
              names_from=ACCOUNT_CLASS_DFLT, 
              values_from=c("delinquentamount",
                            "totalbill",
                            "delinquentpercent"),
              values_fill=0) %>%
  mutate(delinquentamount_RESSF=
           paste0("\\$", round(delinquentamount_RESSF/1000000, 2), " Million"),
         delinquentamount_RESMF=
           paste0("\\$", round(delinquentamount_RESMF/1000000, 2), " Million"),
         totalbill_RESSF=paste0("\\$", round(totalbill_RESSF/1000000, 2), " Million"),
         totalbill_RESMF=paste0("\\$", round(totalbill_RESMF/1000000, 2), " Million"),
         delinquentpercent_RESSF=paste0(round(delinquentpercent_RESSF*100, 2), "\\%"),
         delinquentpercent_RESMF=paste0(round(delinquentpercent_RESMF*100, 2), "\\%"),
         year=ifelse(year=="all", "2019-2022", year)) %>%
  select(year,
         delinquentamount_RESSF, totalbill_RESSF, delinquentpercent_RESSF,
         delinquentamount_RESMF, totalbill_RESMF, delinquentpercent_RESMF)

tab_row <- function(row) {
  out <- TR(delinquency_amount_type[row, 1:7] %>% as.character())
  return(out)
}

tab <- TR(c("Year", "Residential Single Family", "Residential Multi Family"),
          cspan=c(1, 3, 3)) +
  midrulep(list(c(2, 4), c(5, 7))) +
  TR(c("",
       "Delinquent Amount", "Total Bill", "Percent",
       "Delinquent Amount", "Total Bill", "Percent")) +
  midrule() +
  tab_row(1) + tab_row(2) + tab_row(3) + tab_row(4) + tab_row(5)

TS(tab, file="total_delinquent", header=c("c|ccc|ccc"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)
