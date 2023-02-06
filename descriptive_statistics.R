# Descriptive Statistics

# Basic data cleaning ####
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/financial_info.RData"))
load(file=paste0(working_data_dir, "/geocode_address_info_subset.RData"))
load(file=paste0(working_data_dir, "/acs_tract.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))

tracts <- read.csv(file=paste0(working_data_dir, "/portland_geoid.csv"),
                   header=TRUE)$GEOID

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


# Choose valid accounts
account_info <- account_info %>%
  filter(BILLING_STAT=="REGLR",
         ACCOUNT_CLASS_DFLT %in% c("RESSF", "RESMF", "ASST"))

# Merge in location info and ACS info
account_info_merge <-
  left_join(account_info,
            location_relation %>%
              select(ACCT_TO_FRC_CONNECT,
                     LOCATION_NO,
                     PERSON_NO),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT",
                 "PERSON_NO"))

account_info_merge <-
  left_join(account_info_merge,
            geocode_address_info_subset %>%
              select(LOCATION_NO, census_tract),
            by="LOCATION_NO")

account_info_merge <-
  left_join(account_info_merge,
            portland_demographics_tract_wide,
            by=c("census_tract"="tract"))

account_info_merge <- account_info_merge %>% unique()

# Merge in all financial info
account_info_merge <-
  left_join(account_info_merge,
            delinquency_status %>%
              mutate(ACCOUNT_NO=as.character(ACCOUNT_NO),
                     delinquency_match=TRUE),
            by="ACCOUNT_NO") %>%
  rowwise() %>%
  mutate(n_bill=n_bill_2019+
           n_bill_2020+
           n_bill_2021+
           n_bill_2022,
         delinquent=delinquent_2019+
           delinquent_2020+
           delinquent_2021+
           delinquent_2022,
         delinquency_rate=delinquent/n_bill,
         delinquent_amount=delinquent_amount_2019+
           delinquent_amount_2020+
           delinquent_amount_2021+
           delinquent_amount_2022)

account_info_merge <-
  left_join(account_info_merge,
            payment_arrange_by_year,
            by="ACCOUNT_NO") %>%
  replace_na(list(payment_arrange_2019=FALSE,
                  payment_arrange_2020=FALSE,
                  payment_arrange_2021=FALSE,
                  payment_arrange_2022=FALSE)) %>%
  mutate(payment_arrange=payment_arrange_2019 | 
           payment_arrange_2020 |
           payment_arrange_2021 | 
           payment_arrange_2022)

account_info_merge <-
  left_join(account_info_merge,
            financial_assist_by_year,
            by="ACCOUNT_NO") %>%
  replace_na(list(financial_assist_2019=FALSE,
                  financial_assist_2020=FALSE,
                  financial_assist_2021=FALSE,
                  financial_assist_2022=FALSE)) %>%
  mutate(financial_assist=financial_assist_2019 |
           financial_assist_2020 |
           financial_assist_2021 |
           financial_assist_2022)

account_info_merge <-
  left_join(account_info_merge,
            cutoff_reconnect %>%
              select(ACCOUNT_NO, cutoff_2019, cutoff_2020, cutoff_2021, cutoff_2022),
            by="ACCOUNT_NO") %>%
  replace_na(list(cutoff_2019=FALSE,
                  cutoff_2020=FALSE,
                  cutoff_2021=FALSE,
                  cutoff_2022=FALSE)) %>%
  mutate(cutoff=cutoff_2019 | cutoff_2020 | cutoff_2021 | cutoff_2022)

# Consider only the sample with valid Census tract
account_info_merge <- account_info_merge %>%
  filter(!is.na(census_tract)) %>%
  unique()

# Consider only the sample with one location code
multiple_location <- account_info_merge %>%
  group_by(ACCOUNT_NO) %>%
  summarise(count=n()) %>%
  filter(count>1)

account_info_merge <- account_info_merge %>%
  filter(!(ACCOUNT_NO %in% multiple_location$ACCOUNT_NO)) %>%
  unique()

# Consider only the sample with bill info
account_info_merge <- account_info_merge %>%
  filter(delinquency_match) %>%
  unique() %>%
  select(-delinquency_match)


# Calculate descriptive statistics ####
stats_calculate <- function(df, group_var, var) {
  df <- df %>%
    group_by({{group_var}}) %>%
    summarise(variable=var,
              size=n(),
              mean := mean(get(var), na.rm=TRUE),
              min := min(get(var), na.rm=TRUE),
              max := max(get(var), na.rm=TRUE),
              sd := sd(get(var), na.rm=TRUE))
  return(df)
}

# Aggregate for the city
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
                      "financial_assist",
                      "financial_assist_2019",
                      "financial_assist_2020",
                      "financial_assist_2021",
                      "financial_assist_2022")

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
                           "Financial Assistance",
                           "Financial Assistance - 2019",
                           "Financial Assistance - 2020",
                           "Financial Assistance - 2021",
                           "Financial Assistance - 2022")

city_payment <- data.frame()

for (var in payment_var_list) {
  city_payment <- rbind(city_payment,
                        stats_calculate(account_info_merge, "", var))
}

city_payment[,1] <- payment_var_name_list
colnames(city_payment)[1] <- "Variable"

tab_row <- function(row, hskip=TRUE, digit=3) {
  if (hskip) {
    out <- TR(paste0("\\quad ", city_payment[row, 1] %>% as.character())) %:%
      TR(city_payment[row, 3:7] %>% as.numeric(), dec=c(5, digit, digit, digit, digit))
  } else {
    out <- TR(city_payment[row, 1] %>% as.character()) %:%
      TR(city_payment[row, 3:7] %>% as.numeric(), dec=c(5, digit, digit, digit, digit))
  }
  return(out)
}

tab <- TR(c("Variable", "n", "Mean", "Min", "Max", "SD")) +
  midrule() +
  tab_row(1, FALSE) + tab_row(2) + tab_row(3) + tab_row(4) + tab_row(5) + midrule() +
  tab_row(6, FALSE, 2) +
  tab_row(7, TRUE, 2) +
  tab_row(8, TRUE, 2) +
  tab_row(9, TRUE, 2) +
  tab_row(10, TRUE, 2) +
  midrule() +
  tab_row(11, FALSE) + tab_row(12) + tab_row(13) + tab_row(14) + tab_row(15) + midrule() +
  tab_row(16, FALSE) + tab_row(17) + tab_row(18) + tab_row(19) + tab_row(20) + midrule() +
  tab_row(21, FALSE) + tab_row(22) + tab_row(23) + tab_row(24) + tab_row(25)

tab <- fix_0(tab)
TS(tab, file="city_payment", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for each Census tracts
census_payment <- data.frame()

for (var in payment_var_list) {
  census_payment <- rbind(census_payment,
                          stats_calculate(account_info_merge, census_tract, var))
}

census_payment <- left_join(census_payment,
                            portland_demographics_tract_wide,
                            by=c("census_tract"="tract"))

census_payment_stat <- census_payment %>%
  group_by(variable) %>%
  summarise(sd := sqrt(wtd.var(mean, total_hh, na.rm=TRUE)),
            min := min(mean, na.rm=TRUE),
            max := max(mean, na.rm=TRUE),
            mean := wtd.mean(mean, total_hh, na.rm=TRUE)) %>%
  select(variable, mean, min, max, sd)

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

census_character <- data.frame()
for (var in census_var_list) {
  var_stat <- portland_demographics_tract_wide %>%
    group_by() %>%
    summarise(sd := sqrt(wtd.var(get(var), total_hh, na.rm=TRUE)),
              min := min(get(var), na.rm=TRUE),
              max := max(get(var), na.rm=TRUE),
              mean := wtd.mean(get(var), total_hh, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(variable=var) %>%
    select(variable, mean, min, max, sd)
  
  census_character <- rbind(census_character, var_stat)
}

census_character <- census_character %>%
  mutate(mean=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(mean), mean),
         min=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(min), min),
         max=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(max), max),
         sd=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(sd), sd))

census_stat <- rbind(census_payment_stat,
                     census_character %>% select(variable, mean, min, max, sd))
census_stat$variable <- factor(census_stat$variable,
                               levels=c(census_var_list, payment_var_list))
census_stat <- census_stat %>% arrange(variable)

census_stat[,1] <- c(census_var_name_list, payment_var_name_list)
colnames(census_stat)[1] <- "Variable"

tab_row <- function(row, hskip=TRUE, digit=3) {
  if (hskip) {
    out <- TR(paste0("\\quad ", census_stat[row, 1] %>% as.character())) %:%
      TR(census_stat[row, 2:5] %>% as.numeric(), dec=digit)
  } else {
    out <- TR(census_stat[row, 1] %>% as.character()) %:%
      TR(census_stat[row, 2:5] %>% as.numeric(), dec=digit)
  }
  return(out)
}

tab <- TR(c("Variable", "Mean", "Min", "Max", "SD")) +
  midrule() +
  tab_row(1, FALSE, 5) + tab_row(2, TRUE, 2) + tab_row(3, FALSE, 5) + tab_row(4, TRUE, 5) +
  tab_row(5, FALSE) + tab_row(6, FALSE) + tab_row(7, FALSE) +
  tab_row(8, FALSE) + tab_row(9, FALSE) + tab_row(10, FALSE) +
  midrule() +
  tab_row(11, FALSE) + tab_row(12) + tab_row(13) + tab_row(14) + tab_row(15) + midrule() +
  tab_row(16, FALSE) + tab_row(17) + tab_row(18) + tab_row(19) + tab_row(20) + midrule() +
  tab_row(21, FALSE) + tab_row(22) + tab_row(23) + tab_row(24) + tab_row(25) + midrule() +
  tab_row(26, FALSE) + tab_row(27) + tab_row(28) + tab_row(29) + tab_row(30) + midrule() +
  tab_row(31, FALSE) + tab_row(32) + tab_row(33) + tab_row(34) + tab_row(35)

tab <- fix_0(tab)
TS(tab, file="census_character", header=c("l|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for Census tracts with highest delinquency rates
highest_delinquency_tracts <- census_payment %>%
  filter(variable=="delinquency_rate") %>%
  arrange(desc(mean))
highest_delinquency_tracts <- highest_delinquency_tracts$census_tract[1:20]

census_payment_highest_delinquency <- census_payment %>%
  filter(census_tract %in% highest_delinquency_tracts)

census_payment_highest_delinquency_stat <-
  census_payment_highest_delinquency %>%
  group_by(variable) %>%
  summarise(sd := sqrt(wtd.var(mean, total_hh, na.rm=TRUE)),
            min := min(mean, na.rm=TRUE),
            max := max(mean, na.rm=TRUE),
            mean := wtd.mean(mean, total_hh, na.rm=TRUE)) %>%
  select(variable, mean, min, max, sd)

portland_demographics_highest_delinquency <-
  portland_demographics_tract_wide %>%
  filter(tract %in% highest_delinquency_tracts)

census_character_highest_delinquency <- data.frame()
for (var in census_var_list) {
  var_stat <- portland_demographics_highest_delinquency %>%
    group_by() %>%
    summarise(sd := sqrt(wtd.var(get(var), total_hh, na.rm=TRUE)),
              min := min(get(var), na.rm=TRUE),
              max := max(get(var), na.rm=TRUE),
              mean := wtd.mean(get(var), total_hh, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(variable=var) %>%
    select(variable, mean, min, max, sd)
  
  census_character_highest_delinquency <- rbind(census_character_highest_delinquency, var_stat)
}

census_character_highest_delinquency <- census_character_highest_delinquency %>%
  mutate(mean=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(mean), mean),
         min=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(min), min),
         max=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(max), max),
         sd=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(sd), sd))

census_stat_highest_delinquency <-
  rbind(census_payment_highest_delinquency_stat,
        census_character_highest_delinquency %>%
          select(variable, mean, min, max, sd))
census_stat_highest_delinquency$variable <-
  factor(census_stat_highest_delinquency$variable,
         levels=c(census_var_list, payment_var_list))
census_stat_highest_delinquency <- census_stat_highest_delinquency %>% arrange(variable)

census_stat_highest_delinquency[,1] <- c(census_var_name_list, payment_var_name_list)
colnames(census_stat_highest_delinquency)[1] <- "Variable"

tab_row <- function(row, hskip=TRUE, digit=3) {
  if (hskip) {
    out <- TR(paste0("\\quad ", census_stat_highest_delinquency[row, 1] %>% as.character())) %:%
      TR(census_stat_highest_delinquency[row, 2:5] %>% as.numeric(), dec=digit)
  } else {
    out <- TR(census_stat_highest_delinquency[row, 1] %>% as.character()) %:%
      TR(census_stat_highest_delinquency[row, 2:5] %>% as.numeric(), dec=digit)
  }
  return(out)
}

tab <- TR(c("Variable", "Mean", "Min", "Max", "SD")) +
  midrule() +
  tab_row(1, FALSE, 5) + tab_row(2, TRUE, 2) + tab_row(3, FALSE, 5) + tab_row(4, TRUE, 5) +
  tab_row(5, FALSE) + tab_row(6, FALSE) + tab_row(7, FALSE) +
  tab_row(8, FALSE) + tab_row(9, FALSE) + tab_row(10, FALSE) +
  midrule() +
  tab_row(11, FALSE) + tab_row(12) + tab_row(13) + tab_row(14) + tab_row(15) + midrule() +
  tab_row(16, FALSE) + tab_row(17) + tab_row(18) + tab_row(19) + tab_row(20) + midrule() +
  tab_row(21, FALSE) + tab_row(22) + tab_row(23) + tab_row(24) + tab_row(25) + midrule() +
  tab_row(26, FALSE) + tab_row(27) + tab_row(28) + tab_row(29) + tab_row(30) + midrule() +
  tab_row(31, FALSE) + tab_row(32) + tab_row(33) + tab_row(34) + tab_row(35)

tab <- fix_0(tab)
TS(tab, file="census_character_high_delinquency", header=c("l|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

# Aggregate for Census tracts with highest cutoff rates
highest_cutoff_tracts <- census_payment %>%
  filter(variable=="cutoff") %>%
  arrange(desc(mean))
highest_cutoff_tracts <- highest_cutoff_tracts$census_tract[1:20]

census_payment_highest_cutoff <- census_payment %>%
  filter(census_tract %in% highest_cutoff_tracts)

census_payment_highest_cutoff_stat <-
  census_payment_highest_cutoff %>%
  group_by(variable) %>%
  summarise(sd := sqrt(wtd.var(mean, total_hh, na.rm=TRUE)),
            min := min(mean, na.rm=TRUE),
            max := max(mean, na.rm=TRUE),
            mean := wtd.mean(mean, total_hh, na.rm=TRUE)) %>%
  select(variable, mean, min, max, sd)

portland_demographics_highest_cutoff <-
  portland_demographics_tract_wide %>%
  filter(tract %in% highest_cutoff_tracts)

census_character_highest_cutoff <- data.frame()
for (var in census_var_list) {
  var_stat <- portland_demographics_highest_cutoff %>%
    group_by() %>%
    summarise(sd := sqrt(wtd.var(get(var), total_hh, na.rm=TRUE)),
              min := min(get(var), na.rm=TRUE),
              max := max(get(var), na.rm=TRUE),
              mean := wtd.mean(get(var), total_hh, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(variable=var) %>%
    select(variable, mean, min, max, sd)
  
  census_character_highest_cutoff <- rbind(census_character_highest_cutoff, var_stat)
}

census_character_highest_cutoff <- census_character_highest_cutoff %>%
  mutate(mean=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(mean), mean),
         min=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(min), min),
         max=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(max), max),
         sd=ifelse(variable %in% c("total_hh", "hh_income", "hh_cash_assistance"), round(sd), sd))

census_stat_highest_cutoff <-
  rbind(census_payment_highest_cutoff_stat,
        census_character_highest_cutoff %>%
          select(variable, mean, min, max, sd))
census_stat_highest_cutoff$variable <-
  factor(census_stat_highest_cutoff$variable,
         levels=c(census_var_list, payment_var_list))
census_stat_highest_cutoff <- census_stat_highest_cutoff %>% arrange(variable)

census_stat_highest_cutoff[,1] <- c(census_var_name_list, payment_var_name_list)
colnames(census_stat_highest_cutoff)[1] <- "Variable"

tab_row <- function(row, hskip=TRUE, digit=3) {
  if (hskip) {
    out <- TR(paste0("\\quad ", census_stat_highest_cutoff[row, 1] %>% as.character())) %:%
      TR(census_stat_highest_cutoff[row, 2:5] %>% as.numeric(), dec=digit)
  } else {
    out <- TR(census_stat_highest_cutoff[row, 1] %>% as.character()) %:%
      TR(census_stat_highest_cutoff[row, 2:5] %>% as.numeric(), dec=digit)
  }
  return(out)
}

tab <- TR(c("Variable", "Mean", "Min", "Max", "SD")) +
  midrule() +
  tab_row(1, FALSE, 5) + tab_row(2, TRUE, 2) + tab_row(3, FALSE, 5) + tab_row(4, TRUE, 5) +
  tab_row(5, FALSE) + tab_row(6, FALSE) + tab_row(7, FALSE) +
  tab_row(8, FALSE) + tab_row(9, FALSE) + tab_row(10, FALSE) +
  midrule() +
  tab_row(11, FALSE) + tab_row(12) + tab_row(13) + tab_row(14) + tab_row(15) + midrule() +
  tab_row(16, FALSE) + tab_row(17) + tab_row(18) + tab_row(19) + tab_row(20) + midrule() +
  tab_row(21, FALSE) + tab_row(22) + tab_row(23) + tab_row(24) + tab_row(25) + midrule() +
  tab_row(26, FALSE) + tab_row(27) + tab_row(28) + tab_row(29) + tab_row(30) + midrule() +
  tab_row(31, FALSE) + tab_row(32) + tab_row(33) + tab_row(34) + tab_row(35)

tab <- fix_0(tab)
TS(tab, file="census_character_high_cutoff", header=c("l|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)


# Draw map of all the descriptive statistics ####
# Base
acs_tract_base <- acs_tract_geometry %>% filter(GEOID %in% tracts) %>% select(GEOID, geometry)

# Choose relevant payment statistics
census_payment_base <- census_payment %>%
  select(census_tract, variable, mean) %>%
  spread(key=variable, value=mean)

census_base <- left_join(census_payment_base,
                         portland_demographics_tract_wide,
                         by=c("census_tract"="tract"))

acs_tract_map_base <- left_join(acs_tract_base %>%
                                  mutate(census_tract=substr(GEOID, 6, 12)),
                                census_base,
                                by="census_tract")

# Names
var_list <- data.frame(var=c(payment_var_list, census_var_list),
                       var_name=c(payment_var_name_list, census_var_name_list))

var_list <- var_list %>%
  mutate(var_name=gsub("\\", "", var_name, fixed=TRUE))

# Draw graphs!
for (var in var_list$var) {
  gg <- ggplot(acs_tract_map_base,
               aes(fill=get(var))) + 
    geom_sf() +
    map_theme() +
    labs(fill=var_list$var_name[which(var_list$var==var)]) +
    scale_fill_gradient(low="#FBCF61", high="#00CC99", 
                        space="Lab", guide="colourbar")
  gg
  ggsave(plot=gg,
         file=paste0(output_dir, "/", var, "_map.png"),
         width=6, height=4)
}

acs_tract_map_base <- acs_tract_map_base %>%
  mutate(high_delinquency=census_tract %in% highest_delinquency_tracts,
         high_cutoff=census_tract %in% highest_cutoff_tracts)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_delinquency)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Delinquency Area")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_delinquency_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_cutoff)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Cutoff Area")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_cutoff_map.png"),
       width=6, height=4)
