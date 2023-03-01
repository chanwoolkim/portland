# Descriptive Statistics

load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))

# First pass on the account info
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


# Calculate descriptive statistics ####
# List of variables to work with
payment_var_list <- c("delinquency_rate",
                      "delinquency_rate_2019",
                      "delinquency_rate_2020",
                      "delinquency_rate_2021",
                      "delinquency_rate_2022",
                      "delinquent_amount",
                      "delinquent_amount_2019",
                      "delinquent_amount_2020",
                      "delinquent_amount_2021",
                      "delinquent_amount_2022",
                      "cutoff",
                      "cutoff_2019",
                      "cutoff_2020",
                      "cutoff_2021",
                      "cutoff_2022",
                      "payment_arrange",
                      "payment_arrange_2019",
                      "payment_arrange_2020",
                      "payment_arrange_2021",
                      "payment_arrange_2022",
                      "arrange_amount_paid",
                      "arrange_amount_terminated",
                      "financial_assist",
                      "financial_assist_2019",
                      "financial_assist_2020",
                      "financial_assist_2021",
                      "financial_assist_2022",
                      "tier_2020",
                      "tier_2021",
                      "tier_2022",
                      "discount_amount",
                      "discount_amount_2020",
                      "discount_amount_2021",
                      "discount_amount_2022",
                      "crisis_voucher",
                      "crisis_voucher_2020",
                      "crisis_voucher_2021",
                      "crisis_voucher_2022")

payment_var_name_list <- c("Delinquency Rate",
                           "Delinquency Rate - 2019",
                           "Delinquency Rate - 2020",
                           "Delinquency Rate - 2021",
                           "Delinquency Rate - 2022",
                           "Delinquent Amount",
                           "Delinquent Amount - 2019",
                           "Delinquent Amount - 2020",
                           "Delinquent Amount - 2021",
                           "Delinquent Amount - 2022",
                           "Cutoff",
                           "Cutoff - 2019",
                           "Cutoff - 2020",
                           "Cutoff - 2021",
                           "Cutoff - 2022",
                           "Payment Arrangement",
                           "Payment Arrangement - 2019",
                           "Payment Arrangement - 2020",
                           "Payment Arrangement - 2021",
                           "Payment Arrangement - 2022",
                           "Payment Arrangement - Good Amount",
                           "Payment Arrangement - Terminated Amount",
                           "Financial Assistance",
                           "Financial Assistance - 2019",
                           "Financial Assistance - 2020",
                           "Financial Assistance - 2021",
                           "Financial Assistance - 2022",
                           "LINC Tier - 2020",
                           "LINC Tier - 2021",
                           "LINC Tier - 2022",
                           "Financial Assistance Amount",
                           "Financial Assistance Amount - 2020",
                           "Financial Assistance Amount - 2021",
                           "Financial Assistance Amount - 2022",
                           "Crisis Voucher Amount",
                           "Crisis Voucher Amount - 2020",
                           "Crisis Voucher Amount - 2021",
                           "Crisis Voucher Amount - 2022")

payment_var_round_list <- c("delinquent_amount",
                            "delinquent_amount_2019",
                            "delinquent_amount_2020",
                            "delinquent_amount_2021",
                            "delinquent_amount_2022",
                            "arrange_amount_paid",
                            "arrange_amount_terminated",
                            "discount_amount",
                            "discount_amount_2020",
                            "discount_amount_2021",
                            "discount_amount_2022",
                            "crisis_voucher",
                            "crisis_voucher_2020",
                            "crisis_voucher_2021",
                            "crisis_voucher_2022")

payment_var_tier_list <- c("tier_2020",
                           "tier_2021",
                           "tier_2022")

payment_var_percent_list <-
  payment_var_list[-which(payment_var_list %in%
                            c(payment_var_round_list, payment_var_tier_list))]

census_var_list <- c("total_hh",
                     "hh_size",
                     "hh_income",
                     "hh_cash_assistance",
                     "food_stamp",
                     "unemployment",
                     "hh_poverty",
                     "hispanic",
                     "black",
                     "life_expectancy")

census_var_name_list <- c("Total Number of Households",
                          "Household Size",
                          "Household Income",
                          "Household Cash Assistance",
                          "\\% Food Stamp",
                          "\\% Unemployment",
                          "\\% Below Poverty Line",
                          "\\% Hispanic",
                          "\\% Black",
                          "Life Expectancy")

census_var_round_list <- c("total_hh",
                           "hh_income",
                           "hh_cash_assistance",
                           "life_expectancy")

# Functions
stats_calculate <- function(df, group_var, var) {
  df <- df %>%
    group_by({{group_var}}) %>%
    summarise(variable=var,
              size=n(),
              mean := mean(get(var), na.rm=TRUE),
              min := min(get(var), na.rm=TRUE),
              max := max(get(var), na.rm=TRUE),
              sd := sd(get(var), na.rm=TRUE),
              sum := sum(get(var), na.rm=TRUE))
  return(df)
}

stats_df <- function(df, portland_demographics, census=FALSE) {
  if (census) {
    payment_stat <- data.frame()
    for (var in payment_var_list) {
      payment_stat <- rbind(payment_stat,
                            stats_calculate(df, census_tract, var))
    }
    
    census_payment_stat <- left_join(payment_stat,
                                     portland_demographics,
                                     by=c("census_tract"="tract"))
    
    census_stat <- census_payment_stat %>%
      group_by(variable) %>%
      summarise(sd := sqrt(wtd.var(mean, total_hh, na.rm=TRUE)),
                min := min(mean, na.rm=TRUE),
                max := max(mean, na.rm=TRUE),
                mean := wtd.mean(mean, total_hh, na.rm=TRUE),
                sum := sum(sum, na.rm=TRUE)) %>%
      select(variable, mean, min, max, sd, sum)
    
    census_character <- data.frame()
    for (var in census_var_list) {
      var_stat <- portland_demographics %>%
        group_by() %>%
        summarise(sd := sqrt(wtd.var(get(var), total_hh, na.rm=TRUE)),
                  min := min(get(var), na.rm=TRUE),
                  max := max(get(var), na.rm=TRUE),
                  mean := wtd.mean(get(var), total_hh, na.rm=TRUE),
                  sum := sum(get(var), na.rm=TRUE)) %>%
        ungroup() %>%
        mutate(variable=var) %>%
        select(variable, mean, min, max, sd, sum)
      
      census_character <- rbind(census_character, var_stat)
    }
    
    census_character <- census_character %>%
      mutate(mean=ifelse(variable %in% census_var_round_list, round(mean), mean),
             min=ifelse(variable %in% census_var_round_list, round(min), min),
             max=ifelse(variable %in% census_var_round_list, round(max), max),
             sd=ifelse(variable %in% census_var_round_list, round(sd), sd),
             sum=ifelse(variable %in% census_var_round_list, round(sum), sum))
    
    census_stat <- rbind(census_stat,
                         census_character %>% select(variable, mean, min, max, sd, sum))
    
    census_stat <- census_stat %>%
      mutate(mean=ifelse(variable %in% c(payment_var_round_list, census_var_round_list),
                         round(mean), mean),
             min=ifelse(variable %in% c(payment_var_round_list, census_var_round_list),
                        round(min), min),
             max=ifelse(variable %in% c(payment_var_round_list, census_var_round_list),
                        round(max), max),
             sd=ifelse(variable %in% c(payment_var_round_list, census_var_round_list),
                       round(sd), sd),
             sum=ifelse(variable %in% c(payment_var_round_list, census_var_round_list),
                        round(sum), sum),
             mean=ifelse(variable %in% payment_var_percent_list, mean*100, mean),
             min=ifelse(variable %in% payment_var_percent_list, min*100, min),
             max=ifelse(variable %in% payment_var_percent_list, max*100, max),
             sd=ifelse(variable %in% payment_var_percent_list, sd*100, sd),
             sum=ifelse(variable %in% payment_var_percent_list, sum*100, sum))
    
    census_stat$variable <- factor(census_stat$variable,
                                   levels=c(census_var_list, payment_var_list))
    census_stat <- census_stat %>% arrange(variable)
    
    census_stat[,1] <- c(census_var_name_list, payment_var_name_list)
    colnames(census_stat)[1] <- "Variable"
    df_out <- census_stat
  } else {
    payment_stat <- data.frame()
    for (var in payment_var_list) {
      payment_stat <- rbind(payment_stat,
                            stats_calculate(df, "", var))
    }
    
    payment_stat <- payment_stat %>%
      mutate(mean=ifelse(variable %in% payment_var_round_list, round(mean), mean),
             min=ifelse(variable %in% payment_var_round_list, round(min), min),
             max=ifelse(variable %in% payment_var_round_list, round(max), max),
             sd=ifelse(variable %in% payment_var_round_list, round(sd), sd),
             sum=ifelse(variable %in% payment_var_round_list, round(sum), sum),
             mean=ifelse(variable %in% payment_var_percent_list, mean*100, mean),
             min=ifelse(variable %in% payment_var_percent_list, min*100, min),
             max=ifelse(variable %in% payment_var_percent_list, max*100, max),
             sd=ifelse(variable %in% payment_var_percent_list, sd*100, sd),
             sum=ifelse(variable %in% payment_var_percent_list, sum*100, sum))
    
    payment_stat[,1] <- payment_var_name_list
    colnames(payment_stat)[1] <- "Variable"
    df_out <- payment_stat
  }
  return(df_out)
}

tab_df <- function(df, n, census=FALSE) {
  tab_row <- function(row, hskip=TRUE, sum=FALSE, digit=2) {
    if (sum) {
      if (hskip) {
        out <- TR(paste0("\\quad ", df[row, 1] %>% as.character())) %:%
          TR(df[row, 2:6] %>% as.numeric(), dec=digit)
      } else {
        out <- TR(df[row, 1] %>% as.character()) %:%
          TR(df[row, 2:6] %>% as.numeric(), dec=digit)
      }
    } else {
      if (hskip) {
        out <- TR(paste0("\\quad ", df[row, 1] %>% as.character())) %:%
          TR(df[row, 2:5] %>% as.numeric(), dec=digit)
      } else {
        out <- TR(df[row, 1] %>% as.character()) %:%
          TR(df[row, 2:5] %>% as.numeric(), dec=digit)
      }
    }
    return(out)
  }
  
  add_dollar <- function(row_tab) {
    for (col in 2:row_tab$ncol) {
      row_tab$row_list[[1]][col] <-
        str_c("\\$", row_tab$row_list[[1]][col])
    }
    return(row_tab)
  }
  
  add_percent <- function(row_tab) {
    for (col in 2:row_tab$ncol) {
      row_tab$row_list[[1]][col] <-
        str_c(row_tab$row_list[[1]][col], "\\%")
    }
    return(row_tab)
  }
  
  if (census) {
    tab <- TR(c("Variable", "Mean", "Min", "Max", "SD", "Sum")) +
      midrule() +
      tab_row(1, FALSE, TRUE, 5) +
      tab_row(2, TRUE, FALSE, 2) +
      add_dollar(tab_row(3, FALSE, FALSE, 5)) +
      add_dollar(tab_row(4, TRUE, FALSE, 5))
    for (i in 5:9) {tab <- tab + add_percent(tab_row(i, FALSE))}
    tab <- tab + tab_row(10, FALSE, FALSE, 5) + midrule()
    tab <- tab + add_percent(tab_row(11, FALSE))
    for (i in 12:15) {tab <- tab + add_percent(tab_row(i))}
    tab <- tab + midrule() + add_dollar(tab_row(16, FALSE, TRUE, 5))
    for (i in 17:20) {tab <- tab + add_dollar(tab_row(i, TRUE, TRUE, 5))}
    for (i in c(21, 26)) {
      tab <- tab + midrule() + add_percent(tab_row(i, FALSE))
      for (j in 1:4) {
        tab <- tab + add_percent(tab_row(i+j))
      }
    }
    tab <- tab + add_dollar(tab_row(31, TRUE, TRUE, 5)) +
      add_dollar(tab_row(32, TRUE, TRUE, 5)) + midrule() +
      add_percent(tab_row(33, FALSE))
    for (j in 1:4) {
      tab <- tab + add_percent(tab_row(33+j))
    }
    tab <- tab + midrule()
    for (j in 1:3) {
      tab <- tab + tab_row(37+j, FALSE, FALSE, 2)
    }
    for (i in c(41, 45)) {
      tab <- tab + midrule() + add_dollar(tab_row(i, FALSE, TRUE, 5))
      for (j in 1:3) {
        tab <- tab + add_dollar(tab_row(i+j, TRUE, TRUE, 5))
      }
    }
    tab <- tab + midrule() +
      TR("n") %:% TR(c(n, NA, NA, NA, NA), dec=5)
  } else {
    tab <- TR(c("Variable", "Mean", "Min", "Max", "SD", "Sum")) +
      midrule() + add_percent(tab_row(1, FALSE))
    for (i in 2:5) {tab <- tab + add_percent(tab_row(i))}
    tab <- tab + midrule() + add_dollar(tab_row(6, FALSE, TRUE, 5))
    for (i in 7:10) {tab <- tab + add_dollar(tab_row(i, TRUE, TRUE, 5))}
    for (i in c(11, 16)) {
      tab <- tab + midrule() + add_percent(tab_row(i, FALSE))
      for (j in 1:4) {
        tab <- tab + add_percent(tab_row(i+j))
      }
    }
    tab <- tab + add_dollar(tab_row(21, TRUE, TRUE, 5)) +
      add_dollar(tab_row(22, TRUE, TRUE, 5)) + midrule() +
      add_percent(tab_row(23, FALSE))
    for (j in 1:4) {
      tab <- tab + add_percent(tab_row(23+j))
    }
    tab <- tab + midrule()
    for (j in 1:3) {
      tab <- tab + tab_row(27+j, FALSE, FALSE, 2)
    }
    for (i in c(31, 35)) {
      tab <- tab + midrule() + add_dollar(tab_row(i, FALSE, TRUE, 5))
      for (j in 1:3) {
        tab <- tab + add_dollar(tab_row(i+j, TRUE, TRUE, 5))
      }
    }
    tab <- tab + midrule() +
      TR("n") %:% TR(c(n, NA, NA, NA, NA), dec=5)
  }
  tab <- fix_0(tab)
  return(tab)
}

# Only consider single family
account_info_merge <- account_info_merge %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST"))

# Aggregate for the city
city_payment <- stats_df(account_info_merge, portland_demographics_tract_wide) %>%
  select(Variable, mean, min, max, sd, sum)

tab <- tab_df(city_payment, n=nrow(account_info_merge))
TS(tab, file="city_payment", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for each Census tracts
census_character <- stats_df(account_info_merge, portland_demographics_tract_wide, census=TRUE)

tab <- tab_df(census_character,  n=nrow(portland_demographics_tract_wide), census=TRUE)
TS(tab, file="census_character", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for Census tracts with highest delinquency rates
payment_stat <- data.frame()
for (var in payment_var_list) {
  payment_stat <- rbind(payment_stat,
                        stats_calculate(account_info_merge, census_tract, var))
}

census_payment_stat <- left_join(payment_stat,
                                 portland_demographics_tract_wide,
                                 by=c("census_tract"="tract"))

highest_delinquency_tracts <- census_payment_stat %>%
  filter(variable=="delinquency_rate") %>%
  arrange(desc(mean))
highest_delinquency_tracts <- highest_delinquency_tracts$census_tract[1:20]

highest_delinquency_account <- account_info_merge %>%
  filter(census_tract %in% highest_delinquency_tracts)

portland_demographics_highest_delinquency <-
  portland_demographics_tract_wide %>%
  filter(tract %in% highest_delinquency_tracts)

census_character_high_delinquency <-
  stats_df(highest_delinquency_account,
           portland_demographics_highest_delinquency, census=TRUE)

tab <- tab_df(census_character_high_delinquency,
              n=nrow(portland_demographics_highest_delinquency), census=TRUE)
TS(tab, file="census_character_high_delinquency", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for Census tracts with highest cutoff rates
highest_cutoff_tracts <- census_payment_stat %>%
  filter(variable=="cutoff") %>%
  arrange(desc(mean))
highest_cutoff_tracts <- highest_cutoff_tracts$census_tract[1:20]

highest_cutoff_account <- account_info_merge %>%
  filter(census_tract %in% highest_cutoff_tracts)

portland_demographics_highest_cutoff <-
  portland_demographics_tract_wide %>%
  filter(tract %in% highest_cutoff_tracts)

census_character_high_cutoff <-
  stats_df(highest_cutoff_account,
           portland_demographics_highest_cutoff, census=TRUE)

tab <- tab_df(census_character_high_cutoff,
              n=nrow(portland_demographics_highest_cutoff), census=TRUE)
TS(tab, file="census_character_high_cutoff", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for Census tracts with highest minority population
highest_minority_tracts <- portland_demographics_tract_wide %>%
  mutate(minority=hispanic+black) %>%
  arrange(desc(minority))
highest_minority_tracts <- highest_minority_tracts$tract[1:20]

highest_minority_account <- account_info_merge %>%
  filter(census_tract %in% highest_minority_tracts)

portland_demographics_highest_minority <-
  portland_demographics_tract_wide %>%
  filter(tract %in% highest_minority_tracts)

census_character_high_minority <-
  stats_df(highest_minority_account,
           portland_demographics_highest_minority, census=TRUE)

tab <- tab_df(census_character_high_minority,
              n=nrow(portland_demographics_highest_minority), census=TRUE)
TS(tab, file="census_character_high_minority", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for Census tracts with highest poverty rates
highest_poverty_tracts <- portland_demographics_tract_wide %>%
  arrange(desc(hh_poverty))
highest_poverty_tracts <- highest_poverty_tracts$tract[1:20]

highest_poverty_account <- account_info_merge %>%
  filter(census_tract %in% highest_poverty_tracts)

portland_demographics_highest_poverty <-
  portland_demographics_tract_wide %>%
  filter(tract %in% highest_poverty_tracts)

census_character_high_poverty <-
  stats_df(highest_poverty_account,
           portland_demographics_highest_poverty, census=TRUE)

tab <- tab_df(census_character_high_poverty,
              n=nrow(portland_demographics_highest_poverty), census=TRUE)
TS(tab, file="census_character_high_poverty", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Table of counts for delinquency measures ####
counts_calculate <- function(df) {
  df <- df %>%
    mutate(delinquent=delinquent>0,
           delinquent_2019=delinquent_2019>0,
           delinquent_2020=delinquent_2020>0,
           delinquent_2021=delinquent_2021>0,
           delinquent_2022=delinquent_2022>0,
           arrange_paid=arrange_amount_terminated==0,
           arrange_terminated=arrange_amount_terminated>0,
           tier_2020_1=tier_2020==1,
           tier_2020_2=tier_2020==2,
           tier_2021_1=tier_2021==1,
           tier_2021_2=tier_2021==2,
           tier_2022_1=tier_2022==1,
           tier_2022_2=tier_2022==2,
           crisis_voucher=!is.na(crisis_voucher),
           crisis_voucher_2020=!is.na(crisis_voucher_2020),
           crisis_voucher_2021=!is.na(crisis_voucher_2021),
           crisis_voucher_2022=!is.na(crisis_voucher_2022),
           group=NA) %>%
    group_by(group) %>%
    summarise(count=n(),
              delinquent=sum(delinquent, na.rm=TRUE),
              delinquent_2019=sum(delinquent_2019, na.rm=TRUE),
              delinquent_2020=sum(delinquent_2020, na.rm=TRUE),
              delinquent_2021=sum(delinquent_2021, na.rm=TRUE),
              delinquent_2022=sum(delinquent_2022, na.rm=TRUE),
              cutoff=sum(cutoff, na.rm=TRUE),
              cutoff_2019=sum(cutoff_2019, na.rm=TRUE),
              cutoff_2020=sum(cutoff_2020, na.rm=TRUE),
              cutoff_2021=sum(cutoff_2021, na.rm=TRUE),
              cutoff_2022=sum(cutoff_2022, na.rm=TRUE),
              payment_arrange=sum(payment_arrange, na.rm=TRUE),
              payment_arrange_2019=sum(payment_arrange_2019, na.rm=TRUE),
              payment_arrange_2020=sum(payment_arrange_2020, na.rm=TRUE),
              payment_arrange_2021=sum(payment_arrange_2021, na.rm=TRUE),
              payment_arrange_2022=sum(payment_arrange_2022, na.rm=TRUE),
              arrange_paid=sum(arrange_paid, na.rm=TRUE),
              arrange_terminated=sum(arrange_terminated, na.rm=TRUE),
              financial_assist=sum(financial_assist, na.rm=TRUE),
              financial_assist_2019=sum(financial_assist_2019, na.rm=TRUE),
              financial_assist_2020=sum(financial_assist_2020, na.rm=TRUE),
              financial_assist_2021=sum(financial_assist_2021, na.rm=TRUE),
              financial_assist_2022=sum(financial_assist_2022, na.rm=TRUE),
              tier_2020_1=sum(tier_2020_1, na.rm=TRUE),
              tier_2020_2=sum(tier_2020_2, na.rm=TRUE),
              tier_2021_1=sum(tier_2021_1, na.rm=TRUE),
              tier_2021_2=sum(tier_2021_2, na.rm=TRUE),
              tier_2022_1=sum(tier_2022_1, na.rm=TRUE),
              tier_2022_2=sum(tier_2022_2, na.rm=TRUE),
              crisis_voucher=sum(crisis_voucher, na.rm=TRUE),
              crisis_voucher_2020=sum(crisis_voucher_2020, na.rm=TRUE),
              crisis_voucher_2021=sum(crisis_voucher_2021, na.rm=TRUE),
              crisis_voucher_2022=sum(crisis_voucher_2022, na.rm=TRUE))
  return(df)
}

count_all <- rbind(counts_calculate(account_info_merge) %>% select(-group) %>% mutate(data="all"),
                   counts_calculate(highest_delinquency_account) %>% select(-group) %>% mutate(data="delinquency"),
                   counts_calculate(highest_cutoff_account) %>% select(-group) %>% mutate(data="cutoff"),
                   counts_calculate(highest_minority_account) %>% select(-group) %>% mutate(data="minority"),
                   counts_calculate(highest_poverty_account) %>% select(-group) %>% mutate(data="poverty"))
count_all <- count_all %>%
  pivot_longer(!data, names_to="variable", values_to="count") %>%
  pivot_wider(id_cols=variable, 
              names_from=data, 
              values_from=count,
              values_fill=NA)

count_var_name_list <- c("Total Count",
                         "Delinquent",
                         "Delinquent - 2019",
                         "Delinquent - 2020",
                         "Delinquent - 2021",
                         "Delinquent - 2022",
                         "Cutoff",
                         "Cutoff - 2019",
                         "Cutoff - 2020",
                         "Cutoff - 2021",
                         "Cutoff - 2022",
                         "Payment Arrangement",
                         "Payment Arrangement - 2019",
                         "Payment Arrangement - 2020",
                         "Payment Arrangement - 2021",
                         "Payment Arrangement - 2022",
                         "Payment Arrangement - Good",
                         "Payment Arrangement - Terminated",
                         "Financial Assistance",
                         "Financial Assistance - 2019",
                         "Financial Assistance - 2020",
                         "Financial Assistance - 2021",
                         "Financial Assistance - 2022",
                         "Tier 1 - 2020",
                         "Tier 2 - 2020",
                         "Tier 1 - 2021",
                         "Tier 2 - 2021",
                         "Tier 1 - 2022",
                         "Tier 2 - 2022",
                         "Crisis Voucher",
                         "Crisis Voucher - 2020",
                         "Crisis Voucher - 2021",
                         "Crisis Voucher - 2022")

count_all <- count_all %>% mutate(Variable=count_var_name_list) %>%
  select(Variable, all, delinquency, cutoff, minority, poverty)

tab_count <- function(df) {
  tab_row <- function(row, hskip=TRUE, digit=5) {
    if (hskip) {
      out <- TR(paste0("\\quad ", df[row, 1] %>% as.character())) %:%
        TR(df[row, 2:6] %>% as.numeric(), dec=digit)
    } else {
      out <- TR(df[row, 1] %>% as.character()) %:%
        TR(df[row, 2:6] %>% as.numeric(), dec=digit)
    }
    return(out)
  }
  
  tab <- TR(c("Variable", "All",
              "High Delinquency", "High Cutoff",
              "High Minority", "High Poverty")) +
    midrule() +
    tab_row(1, FALSE)
  for (i in c(2, 7, 12)) {
    tab <- tab + midrule() + tab_row(i, FALSE)
    for (j in 1:4) {
      tab <- tab + tab_row(i+j)
    }
  }
  tab <- tab + tab_row(17) + tab_row(18) + midrule() +
    tab_row(19, FALSE)
  for (j in 1:4) {
    tab <- tab + tab_row(19+j)
  }
  tab <- tab + midrule()
  for (j in 1:6) {
    tab <- tab + tab_row(23+j)
  }
  tab <- tab + midrule() +
    tab_row(30, FALSE) + tab_row(31) + tab_row(32) + tab_row(33)
  tab <- fix_0(tab)
  return(tab)
}

tab <- tab_count(count_all)
TS(tab, file="count_all", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)


# Regression ####
# Choose relevant payment statistics
census_payment_base <- census_payment_stat %>%
  select(census_tract, variable, mean) %>%
  spread(key=variable, value=mean)

census_base <- left_join(census_payment_base,
                         portland_demographics_tract_wide,
                         by=c("census_tract"="tract"))

model1 <- glm(delinquency_rate~black+food_stamp+hh_poverty+hispanic,
              weights=total_hh,
              family=binomial(link='logit'),
              data=census_base)

margin_model1 <- margins(model1,
                         variables=c("black", "food_stamp", "hh_poverty", "hispanic"))

model2 <- glm(cutoff~black+food_stamp+hh_poverty+hispanic,
              weights=total_hh,
              family=binomial(link='logit'),
              data=census_base)

margin_model2 <- margins(model2,
                         variables=c("black", "food_stamp", "hh_poverty", "hispanic"))

tab_data <- data.frame(coef_1=summary(model1)$coefficients[,1],
                       coef_margin_1=c(NA,
                                       summary(margin_model1)$AME %>% as.numeric()),
                       coef_2=summary(model2)$coefficients[,1],
                       coef_margin_2=c(NA,
                                       summary(margin_model2)$AME %>% as.numeric()),              
                       se_1=summary(model1)$coefficients[,2],
                       se_margin_1=c(NA,
                                     summary(margin_model1)$SE %>% as.numeric()),
                       se_2=summary(model2)$coefficients[,2],
                       se_margin_2=c(NA,
                                     summary(margin_model2)$SE %>% as.numeric()),   
                       p_1=summary(model1)$coefficients[,4],
                       p_margin_1=c(NA,
                                    summary(margin_model1)$p %>% as.numeric()),
                       p_2=summary(model2)$coefficients[,4],
                       p_margin_2=c(NA,
                                    summary(margin_model2)$p %>% as.numeric()))

tab <- TR(c("", "Delinquency Rate", "Cutoff Rate"), cspan=c(1, 2, 2)) +
  midrulep(list(c(2, 3), c(4, 5))) +
  TR(c("", "Coefficients", "AME", "Coefficients", "AME")) +
  TR(c("", "(1)", "(2)", "(3)", "(4)")) +
  midrule() +
  TR("Black") %:% TR(tab_data[2,1:4] %>% as.numeric(), dec=c(3,5,3,5),
                     pvalues=tab_data[2,9:12] %>% as.numeric()) +
  TR("") %:% TR(tab_data[2,5:8] %>% as.numeric(), dec=c(3,5,3,5), se=T) +
  TR("Hispanic") %:% TR(tab_data[5,1:4] %>% as.numeric(), dec=c(3,5,3,5),
                        pvalues=tab_data[5,9:12] %>% as.numeric()) +
  TR("") %:% TR(tab_data[5,5:8] %>% as.numeric(), dec=c(3,5,3,5), se=T) +
  TR("Food Stamp") %:% TR(tab_data[3,1:4] %>% as.numeric(), dec=c(3,5,3,5),
                          pvalues=tab_data[3,9:12] %>% as.numeric()) +
  TR("") %:% TR(tab_data[3,5:8] %>% as.numeric(), dec=c(3,5,3,5), se=T) +
  TR("Poverty") %:% TR(tab_data[4,1:4] %>% as.numeric(), dec=c(3,5,3,5), 
                       pvalues=tab_data[4,9:12] %>% as.numeric()) +
  TR("") %:% TR(tab_data[4,5:8] %>% as.numeric(), dec=c(3,5,3,5), se=T) +
  TR("(Intercept)") %:% TR(tab_data[1,1:4] %>% as.numeric(), dec=c(3,5,3,5), 
                           pvalues=tab_data[1,9:12] %>% as.numeric()) +
  TR("") %:% TR(tab_data[1,5:8] %>% as.numeric(), dec=c(3,5,3,5), se=T)

tab
TS(tab, file="character_reg", header=c("lcccc"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)
