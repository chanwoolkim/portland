# Merge all data into account

# Basic data cleaning ####
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/delinquency_info.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))
load(file=paste0(working_data_dir, "/geocode_address_info_subset.RData"))
load(file=paste0(working_data_dir, "/acs_tract.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))
load(file=paste0(working_data_dir, "/location_financial.RData"))

tracts <- read.csv(file=paste0(auxiliary_data_dir, "/portland_geoid.csv"),
                   header=TRUE)$GEOID

# Choose valid accounts
account_info_filtered <- account_info %>%
  mutate(occupancy_code=trimws(occupancy_code)) %>%
  filter(occupancy_code %in% c("RESSF", "RESMF", "ASST"))

# Merge in location info and ACS info
account_info_merge <-
  left_join(account_info_filtered,
            location_account_relation %>%
              select(location_number, account_number),
            by="account_number")

account_info_merge <- account_info_merge %>%
  left_join(location_relation %>%
              select(location_number,
                     person_number,
                     account_number),
            by=c("account_number",
                 "person_number"))

account_info_merge <- account_info_merge %>%
  mutate(last_bill_date=mdy(last_bill_date)) %>%
  left_join(location_financial %>%
              select(account_number, location_number, bill_date),
            by=c("account_number", "last_bill_date"="bill_date"))

# Prioritise location relation
account_info_merge <- account_info_merge %>%
  mutate(location_number=ifelse(is.na(location_number),
                                ifelse(is.na(location_number.x),
                                       location_number.y,
                                       location_number.x),
                                location_number)) %>%
  select(-location_number.x, -location_number.y)

account_info_merge <- account_info_merge %>%
  left_join(geocode_address_info_subset %>%
              select(location_number, census_tract),
            by="location_number")

account_info_merge <- account_info_merge %>%
  left_join(portland_demographics_tract_wide,
            by=c("census_tract"="tract"))

account_info_merge <- account_info_merge %>% distinct()

# Merge in all financial info
account_info_merge <- account_info_merge %>%
  left_join(delinquency_status %>%
              mutate(delinquency_match=TRUE),
            by="account_number") %>%
  rowwise() %>%
  mutate(n_bill=n_bill_2020+
           n_bill_2021+
           n_bill_2022+
           n_bill_2023+
           n_bill_2024,
         delinquent=delinquent_2020+
           delinquent_2021+
           delinquent_2022+
           delinquent_2023+
           delinquent_2024,
         delinquency_rate=delinquent/n_bill,
         delinquent_amount=delinquent_amount_2020+
           delinquent_amount_2021+
           delinquent_amount_2022+
           delinquent_amount_2023+
           delinquent_amount_2024,
         delinquent_amount=ifelse(delinquent_amount==0, NA, delinquent_amount),
         delinquent_amount_2020=
           ifelse(delinquent_amount_2020==0, NA, delinquent_amount_2020),
         delinquent_amount_2021=
           ifelse(delinquent_amount_2021==0, NA, delinquent_amount_2021),
         delinquent_amount_2022=
           ifelse(delinquent_amount_2022==0, NA, delinquent_amount_2022),
         delinquent_amount_2023=
           ifelse(delinquent_amount_2023==0, NA, delinquent_amount_2023),
         delinquent_amount_2024=
           ifelse(delinquent_amount_2024==0, NA, delinquent_amount_2024),
         total_bill=total_bill_2020+
           total_bill_2021+
           total_bill_2022+
           total_bill_2023+
           total_bill_2024) %>%
  ungroup()

account_info_merge <- account_info_merge %>%
  left_join(payment_arrange_amount,
            by=c("account_number"))

account_info_merge <- account_info_merge %>%
  left_join(payment_arrange_by_year,
            by="account_number") %>%
  replace_na(list(payment_arrange_2019=FALSE,
                  payment_arrange_2020=FALSE,
                  payment_arrange_2021=FALSE,
                  payment_arrange_2022=FALSE,
                  payment_arrange_2023=FALSE)) %>%
  mutate(payment_arrange=payment_arrange_2019 | 
           payment_arrange_2020 |
           payment_arrange_2021 | 
           payment_arrange_2022 | 
           payment_arrange_2023)

account_info_merge <- account_info_merge %>%
  left_join(linc_info,
            by="location_number") %>%
  rowwise() %>% 
  mutate(discount_amount=sum(discount_amount_2019,
                             discount_amount_2020,
                             discount_amount_2021,
                             discount_amount_2022,
                             discount_amount_2023,
                             discount_amount_2024, na.rm=TRUE),
         crisis_voucher=sum(crisis_voucher_2019,
                            crisis_voucher_2020,
                            crisis_voucher_2021,
                            crisis_voucher_2022,
                            crisis_voucher_2023,
                            crisis_voucher_2024, na.rm=TRUE),
         discount_amount=ifelse(discount_amount==0, NA, discount_amount),
         discount_amount_2019=
           ifelse(discount_amount_2019==0, NA, discount_amount_2019),
         discount_amount_2020=
           ifelse(discount_amount_2020==0, NA, discount_amount_2020),
         discount_amount_2021=
           ifelse(discount_amount_2021==0, NA, discount_amount_2021),
         discount_amount_2022=
           ifelse(discount_amount_2022==0, NA, discount_amount_2022),
         discount_amount_2023=
           ifelse(discount_amount_2023==0, NA, discount_amount_2023),
         discount_amount_2024=
           ifelse(discount_amount_2024==0, NA, discount_amount_2024),
         crisis_voucher=ifelse(crisis_voucher==0, NA, crisis_voucher),
         crisis_voucher_2019=
           ifelse(crisis_voucher_2019==0, NA, crisis_voucher_2019),
         crisis_voucher_2020=
           ifelse(crisis_voucher_2020==0, NA, crisis_voucher_2020),
         crisis_voucher_2021=
           ifelse(crisis_voucher_2021==0, NA, crisis_voucher_2021),
         crisis_voucher_2022=
           ifelse(crisis_voucher_2022==0, NA, crisis_voucher_2022),
         crisis_voucher_2023=
           ifelse(crisis_voucher_2023==0, NA, crisis_voucher_2023),
         crisis_voucher_2024=
           ifelse(crisis_voucher_2024==0, NA, crisis_voucher_2024)) %>%
  ungroup()

account_info_merge <- account_info_merge %>%
  left_join(financial_assist_by_year,
            by="account_number") %>%
  replace_na(list(financial_assist_2019=FALSE,
                  financial_assist_2020=FALSE,
                  financial_assist_2021=FALSE,
                  financial_assist_2022=FALSE,
                  financial_assist_2023=FALSE)) %>%
  mutate(financial_assist=financial_assist_2019 |
           financial_assist_2020 |
           financial_assist_2021 |
           financial_assist_2022 |
           financial_assist_2023)

account_info_merge <- account_info_merge %>%
  left_join(cutoff_reconnect %>%
              select(account_number,
                     cutoff_2019, cutoff_2020, cutoff_2021, cutoff_2022, cutoff_2023),
            by="account_number") %>%
  replace_na(list(cutoff_2019=FALSE,
                  cutoff_2020=FALSE,
                  cutoff_2021=FALSE,
                  cutoff_2022=FALSE,
                  cutoff_2023=FALSE)) %>%
  mutate(cutoff=cutoff_2019 | cutoff_2020 | cutoff_2021 | cutoff_2022 | cutoff_2023)

# Consider only the sample with valid Census tract
account_info_merge <- account_info_merge %>%
  filter(!is.na(census_tract)) %>%
  distinct()

# Consider only the sample with one location code
multiple_location <- account_info_merge %>%
  group_by(account_number) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  filter(count>1)

account_info_merge <- account_info_merge %>%
  filter(!(account_number %in% multiple_location$account_number)) %>%
  distinct()

# Consider only the sample with bill info
account_info_merge <- account_info_merge %>%
  filter(delinquency_match) %>%
  distinct() %>%
  select(-delinquency_match)

# ACS base map
acs_tract_base <- acs_tract_geometry %>%
  filter(GEOID %in% tracts) %>%
  select(GEOID, geometry)

save(account_info, account_info_merge, acs_tract_base,
     file=paste0(working_data_dir, "/account_info_analysis.RData"))
