cycle_bill_date <- read_csv(paste0(working_data_dir, '/transunion/analysis/cycle_bill_date.csv'))
estimation_dataset <- read_csv(paste0(working_data_dir, '/transunion/analysis/estimation_dataset.csv'))
payment_plan <- read_csv(paste0(working_data_dir, '/transunion/analysis/payment_plan.csv'))

fix_sequence <- function(x) {
  n <- length(x)
  if (n==0) return(x)
  
  fixed <- numeric(n)
  fixed[1] <- x[1]
  
  for (i in 2:n) {
    fixed[i] <- fixed[i-1]+1
  }
  
  offset <- x[n]-fixed[n]
  fixed <- fixed+offset
  
  return(fixed)
}

census_variables <- c('percent_age_under_5',
                      'percent_age_5_to_9',
                      'percent_age_10_to_14',
                      'percent_age_15_to_19',
                      'percent_age_20_to_24',
                      'percent_age_25_to_34',
                      'percent_age_35_to_44',
                      'percent_age_45_to_54',
                      'percent_age_55_to_59',
                      'percent_age_60_to_64',
                      'percent_age_65_to_74',
                      'percent_age_75_to_84',
                      'percent_age_over_85',
                      'educational_attainment_no_diploma',
                      'educational_attainment_diploma',
                      'educational_attainment_associates_degree',
                      'educational_attainment_bachelors_degree',
                      'educational_attainment_post_bachelor',
                      'percent_vehicles_owned_0',
                      'percent_vehicles_owned_1',
                      'percent_vehicles_owned_2',
                      'percent_vehicles_owned_more_than_3',
                      'total_house_holds',
                      'average_house_hold_size',
                      'median_age',
                      'unemployment_rate_in_labor_force',
                      'percent_of_population_with_no_health_insurance',
                      'percent_of_population_includes_native_american',
                      'percent_of_population_includes_hispanic',
                      'percent_of_population_includes_asian',
                      'percent_of_population_includes_pacific_islander',
                      'percent_of_population_includes_black',
                      'percent_of_population_includes_white',
                      'percent_of_population_includes_other_race',
                      'median_number_of_rooms',
                      'housing_cost_renter',
                      'housing_cost_owner',
                      'percent_of_house_hold_renting',
                      'percent_of_house_hold_owning',
                      'median_housing_market_value',
                      'median_house_hold_income',
                      'percent_of_house_holds_in_poverty',
                      'percent_of_house_holds_with_under_18',
                      'percent_of_house_holds_with_65_plus',
                      'percent_of_house_holds_with_retirement_income',
                      'percent_of_house_holds_with_ssi',
                      'percent_of_house_holds_with_cash_assistance',
                      'percent_of_house_holds_with_food_stamps')

cycle_bill_date <- cycle_bill_date %>%
  mutate(cycle_code=as.numeric(cycle_code),
         t=if_else(bill_date>='2024/12/12' & bill_date<='2025/03/14', 0, NA)) %>%
  arrange(cycle_code, bill_date) %>%
  group_by(cycle_code) %>%
  mutate(t=if_else(is.na(t), row_number()-which(t==0), t)) %>%
  ungroup()

estimation_dataset <- estimation_dataset %>%
  mutate_at(c("cycle_code",
              "previous_bill_amount",
              "previous_unpaid_amount",
              "current_amount",
              "amount_billed",
              "amount_trans_billed",
              "fees",
              "amount_paid",
              "cleanriver_discount",
              "rct_discount",
              "linc_discount",
              "discount",
              "liens",
              "writeoff",
              "refund",
              "transfer",
              "running_owed",
              "water_consumption",
              "usage_bill",
              "payment_plan_amount"),
            as.numeric) %>%
  mutate(due_date=case_when(
    monthly_payment | payment_plan ~ bill_date + days(90),
    TRUE ~ due_date
  )) %>%
  arrange(tu_id, bill_date) %>%
  group_by(tu_id) %>%
  mutate(first_bill=row_number()==1) %>%
  filter(bill_date>="2019/01/01")

estimation_dataset <- estimation_dataset %>%
  full_join(cycle_bill_date, by="cycle_code") %>%
  mutate(date_diff=abs(as.numeric(difftime(bill_date.x, bill_date.y, units="days")))) %>%
  group_by(tu_id, bill_date.x, due_date) %>%
  slice_min(order_by=date_diff, n=1, with_ties=FALSE) %>%
  ungroup()

estimation_dataset <- bind_rows(estimation_dataset %>%
                                  filter(!first_bill, type_code!="FINAL", !is_rebill) %>%
                                  group_by(tu_id) %>%
                                  mutate(n=n()) %>%
                                  filter(n>1 & any(date_diff>=30)) %>%
                                  ungroup() %>%
                                  arrange(tu_id, bill_date.x) %>%
                                  group_by(tu_id) %>%
                                  mutate(t=fix_sequence(t)) %>%
                                  ungroup(),
                                estimation_dataset %>%
                                  filter(!first_bill, type_code!="FINAL", !is_rebill) %>%
                                  group_by(tu_id) %>%
                                  mutate(n=n()) %>%
                                  filter(n<=1 | all(date_diff<30)) %>%
                                  ungroup(),
                                estimation_dataset %>%
                                  filter(first_bill | type_code=="FINAL" | is_rebill))

estimation_dataset <- bind_rows(estimation_dataset %>%
                                  filter(!first_bill, type_code!="FINAL", !is_rebill) %>%
                                  group_by(tu_id) %>%
                                  mutate(got_discount=sum(rct_discount<0, na.rm=TRUE)) %>%
                                  filter(got_discount==1) %>%
                                  ungroup() %>%
                                  mutate(t=ifelse(rct_discount<0, 0, NA)) %>%
                                  arrange(tu_id, bill_date.x) %>%
                                  group_by(tu_id) %>%
                                  mutate(anchor_row=which(t==0),
                                         t=row_number()-anchor_row) %>%
                                  ungroup(),
                                estimation_dataset %>%
                                  filter(!first_bill, type_code!="FINAL", !is_rebill) %>%
                                  group_by(tu_id) %>%
                                  mutate(got_discount=sum(rct_discount<0, na.rm=TRUE)) %>%
                                  filter(got_discount!=1) %>%
                                  ungroup(),
                                estimation_dataset %>%
                                  filter(first_bill | type_code=="FINAL" | is_rebill)) %>%
  select(-date_diff, -n, -got_discount, -anchor_row)

estimation_dataset <- estimation_dataset %>%
  distinct()

estimation_dataset <- estimation_dataset %>%
  arrange(tu_id, start_date) %>%
  group_by(tu_id) %>%
  mutate(D_t=case_when(
    first_bill ~ 0,
    TRUE ~ previous_unpaid_amount)) %>%
  ungroup()

estimation_dataset <- estimation_dataset %>%
  rename_with(~paste0("census_", .x), census_variables)

estimation_dataset <- estimation_dataset %>%
  mutate(has_environment_discount=cleanriver_discount<0) %>%
  select(id=tu_id,
         bill_date=bill_date.x,
         due_date,
         t,
         B_t=current_amount,
         D_t=D_t,
         F_t=fees,
         O_t=amount_billed,
         E_t=amount_paid,
         lag_w_t=water_consumption,
         discount_grid=discount_percentage,
         fa_type=linc_tier_type,
         delinquent_at_randomization=delinquent,
         has_environment_discount,
         shutoff_date=cutoff_date,
         reconnect_date,
         tu_credit_score=credit_score,
         account_status=type_code,
         ufh=ufh_status,
         fa_eligible=fa_status,
         below_median_income=median_status,
         tu_income=etie,
         credit_quartile,
         payment_plan,
         monthly_payment,
         linc_tier_type_at_bill,
         is_honored_citizen,
         is_rebill,
         first_bill,
         exit_reason,
         cycle_code,
         state_id,
         county_id,
         tract_id,
         service_water,
         service_sewer,
         service_storm,
         rct_discount,
         cleanriver_discount,
         linc_discount,
         crisis_voucher=discount,
         contains("census_")) %>%
  arrange(id, t) %>%
  group_by(id) %>%
  mutate(w_t=lead(lag_w_t, 1),
         lag_F_t=lag(F_t, 1)) %>%
  ungroup() %>%
  mutate(lag_F_t=ifelse(is.na(lag_F_t), 0, lag_F_t))

save(estimation_dataset,
     file=paste0(working_data_dir, '/estimation_dataset.RData'))
