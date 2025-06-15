cycle_bill_date <- read_csv(paste0(working_data_dir, "/servus_query/cycle_bill_date.csv"))
estimation_dataset_all <- read_csv(paste0(working_data_dir, "/servus_query/estimation_dataset_all.csv"))
payment_plan <- read_csv(paste0(working_data_dir, "/servus_query/payment_plan.csv"))

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

cycle_bill_date <- cycle_bill_date %>%
  mutate(cycle_code=as.numeric(cycle_code),
         t=if_else(bill_date>="2024/12/12" & bill_date<="2025/03/14", 0, NA)) %>%
  arrange(cycle_code, bill_date) %>%
  group_by(cycle_code) %>%
  mutate(t=if_else(is.na(t), row_number()-which(t==0), t)) %>%
  ungroup()

# Add in supposed t==1
cycle_bill_date <- bind_rows(cycle_bill_date %>% 
                               filter(t==0, cycle_code>4, cycle_code<14) %>%
                               mutate(bill_date=bill_date+days(90), 
                                      t=1),
                             cycle_bill_date)

estimation_dataset_all <- estimation_dataset_all %>%
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
  arrange(account_number, bill_date) %>%
  group_by(account_number) %>%
  mutate(first_bill=row_number()==1) %>%
  filter(bill_date>="2019/01/01")

estimation_dataset_all <- estimation_dataset_all %>%
  full_join(cycle_bill_date, by="cycle_code") %>%
  mutate(date_diff=abs(as.numeric(difftime(bill_date.x, bill_date.y, units="days")))) %>%
  group_by(account_number, bill_date.x, due_date) %>%
  slice_min(order_by=date_diff, n=1, with_ties=FALSE) %>%
  ungroup()

estimation_dataset_all <- bind_rows(estimation_dataset_all %>%
                                      filter(!first_bill, type_code!="FINAL", !is_rebill) %>%
                                      group_by(account_number) %>%
                                      mutate(n=n()) %>%
                                      filter(n>1 & any(date_diff>=30)) %>%
                                      ungroup() %>%
                                      arrange(account_number, bill_date.x) %>%
                                      group_by(account_number) %>%
                                      mutate(t=fix_sequence(t)) %>%
                                      ungroup(),
                                    estimation_dataset_all %>%
                                      filter(!first_bill, type_code!="FINAL", !is_rebill) %>%
                                      group_by(account_number) %>%
                                      mutate(n=n()) %>%
                                      filter(n<=1 | all(date_diff<30)) %>%
                                      ungroup(),
                                    estimation_dataset_all %>%
                                      filter(first_bill | type_code=="FINAL" | is_rebill))

estimation_dataset_all <- estimation_dataset_all %>%
  arrange(account_number, start_date) %>%
  group_by(account_number) %>%
  mutate(D_t=case_when(
    first_bill ~ 0,
    TRUE ~ previous_unpaid_amount)) %>%
  ungroup()

estimation_dataset_all <- estimation_dataset_all %>%
  group_by(account_number) %>%
  mutate(exist_t1=any(t==1)) %>%
  ungroup()

estimation_dataset_all <- estimation_dataset_all %>%
  transmute(id=account_number,
            bill_date=bill_date.x,
            due_date,
            t,
            B_t=current_amount,
            D_t=D_t,
            F_t=fees,
            O_t=amount_billed,
            E_t=amount_paid,
            lag_w_t=water_consumption,
            has_environment_discount=cleanriver_discount<0,
            shutoff_date=cutoff_date,
            reconnect_date,
            account_status=type_code,
            ufh=ufh_status,
            fa_eligible=fa_status,
            below_median_income=median_status,
            tu_income=tu_income,
            tu_credit_score,
            credit_quartile,
            payment_plan,
            monthly_payment,
            is_rebill,
            first_bill,
            cycle_code,
            state_id,
            county_id,
            tract_id,
            block_group_id,
            occupancy_status,
            age,
            estimated_current_home_value,
            aspire_income=income,
            household_size,
            adult_count,
            child_count,
            service_water,
            service_sewer,
            service_storm,
            rct_discount,
            cleanriver_discount,
            linc_discount,
            crisis_voucher=discount) %>%
  arrange(id, t) %>%
  group_by(id) %>%
  mutate(w_t=lead(lag_w_t, 1),
         lag_F_t=lag(F_t, 1)) %>%
  ungroup() %>%
  mutate(lag_F_t=ifelse(is.na(lag_F_t), 0, lag_F_t))

save(estimation_dataset_all,
     file=paste0(working_data_dir, "/estimation_dataset_all.RData"))
