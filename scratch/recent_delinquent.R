#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
rm(list=ls())
start_time <- Sys.time()

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")

source(paste0(code_dir, "/utilities/preliminary.R"))

# Load data
load(paste0(working_data_dir, "/portland_panel_estimation_old.RData"))
portland_panel_estimation_old <- portland_panel_estimation

load(paste0(working_data_dir, "/portland_panel_estimation.RData"))

# Homogenise two datasets
portland_panel_estimation_old <- portland_panel_estimation_old %>%
  transmute(account_number=as.numeric(ACCOUNT_NO),
            person_number=PERSON_NO,
            due_date=DUE_DT,
            bill_date=BILL_RUN_DT,
            bill_year, previous_bill, total_payments, leftover_debt, current_bill,
            source_code=SOURCE_CD,
            type_code=BILL_TP,
            is_off_cycle=OFF_CYCLE_YN,
            start_date=PERIOD_FROM_DT,
            end_date=PERIOD_TO_DT,
            cycle_code=CYCLE_CD,
            payment_arrange, payment_arrange_status, financial_assist, cutoff,
            usage_bill_amount, water_cons, sewer_cons,
            bill_penalty, bill_bankrupt, bill_donate, bill_leaf,
            price_water, price_sewer, price_fixed, price_donation, price_discount,
            occupancy=OCCUPANCY,
            location_number=LOCATION_NO,
            tract,
            linc_tier_type=LINC_TIER_TYPE,
            net_after_assistance, bill_before_assistance, discount_assistance, crisis_voucher_amount,
            senior_disabilities,
            writeoff_amount, collection_sent_amount, collection_collected_amount,
            final_payment, final_writeoff,
            agg,
            delinquent)

portland_panel_estimation <- rbind(portland_panel_estimation_old,
                                   portland_panel_estimation) %>%
  distinct()

portland_panel_2024q1 <- portland_panel_estimation %>%
  filter(year(bill_date)==2024, quarter(bill_date)==1)

portland_panel_2024q1 %>%
  select(account_number) %>%
  distinct() %>%
  count() #148,467

portland_panel_2024q1 <- portland_panel_2024q1 %>%
  filter(type_code=="REGLR")

portland_panel_2024q1 %>%
  select(account_number) %>%
  distinct() %>%
  count() #143,255

portland_panel_2024q1 %>%
  group_by(linc_tier_type, senior_disabilities, delinquent) %>%
  summarise(n=n_distinct(account_number))

portland_panel_2024q1 <- portland_panel_2024q1 %>%
  filter(!senior_disabilities)

portland_panel_2024q1 %>%
  select(account_number) %>%
  distinct() %>%
  count() #141,249

# Descriptive on accounts
delinquent_threshold <- function(d) {
  portland_panel_2024q1 %>%
    mutate(delinquent=leftover_debt>d) %>%
    group_by(linc_tier_type, delinquent) %>%
    summarise(n=n_distinct(account_number))
}

delinquent_threshold(0)
delinquent_threshold(10)
delinquent_threshold(50)
delinquent_threshold(115)

portland_panel_2024q1 <- portland_panel_estimation %>%
  filter(account_number %in% portland_panel_2024q1$account_number)

delinquent_threshold <- function(d) {
  portland_panel_2024q1 %>%
    mutate(delinquent=leftover_debt>d) %>%
    arrange(account_number, bill_date) %>%
    group_by(account_number) %>%
    mutate(delinquent_lag=lag(delinquent, 1)) %>%
    ungroup() %>%
    mutate(delinquent_consistent=(delinquent & delinquent_lag)) %>%
    filter(year(bill_date)==2024, quarter(bill_date)==1,
           type_code=="REGLR", !senior_disabilities) %>%
    group_by(linc_tier_type, delinquent_consistent) %>%
    summarise(n=n_distinct(account_number))
}

delinquent_threshold(0)
delinquent_threshold(10)
delinquent_threshold(50)
delinquent_threshold(115)

delinquent_threshold <- function(d) {
  portland_panel_2024q1 %>%
    mutate(delinquent=leftover_debt>d) %>%
    arrange(account_number, bill_date) %>%
    group_by(account_number) %>%
    mutate(delinquent_lag=lag(delinquent, 1),
           delinquent_lag_lag=lag(delinquent, 2)) %>%
    ungroup() %>%
    mutate(delinquent_consistent=(delinquent & delinquent_lag & delinquent_lag_lag)) %>%
    filter(year(bill_date)==2024, quarter(bill_date)==1,
           type_code=="REGLR", !senior_disabilities) %>%
    group_by(linc_tier_type, delinquent_consistent) %>%
    summarise(n=n_distinct(account_number))
}

delinquent_threshold(0)
delinquent_threshold(10)
delinquent_threshold(50)
delinquent_threshold(115)

# Non-consecutive missers
portland_panel_estimation %>%
  arrange(account_number, bill_date) %>%
  group_by(account_number) %>%
  mutate(delinquent_lag=lag(delinquent, 1)) %>%
  ungroup() %>%
  mutate(delinquent_once=(delinquent & !delinquent_lag)) %>%
  filter(type_code=="REGLR") %>%
  group_by(account_number, bill_year) %>%
  summarise(delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_once=sum(delinquent_once, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquent=delinquent>0,
         delinquent_once=delinquent_once>0) %>%
  group_by(delinquent, delinquent_once, bill_year) %>%
  summarise(n=n_distinct(account_number))

portland_panel_estimation %>%
  arrange(account_number, bill_date) %>%
  group_by(account_number) %>%
  filter(any(bill_year==2021),
         any(bill_year==2024)) %>%
  mutate(delinquent_lag=lag(delinquent, 1)) %>%
  ungroup() %>%
  mutate(delinquent_once=(delinquent & !delinquent_lag)) %>%
  filter(type_code=="REGLR") %>%
  group_by(account_number, bill_year) %>%
  summarise(delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_once=sum(delinquent_once, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(never_delinquent=delinquent==0,
         delinquent_once=delinquent==delinquent_once & delinquent>0,
         delinquent_more=delinquent>delinquent_once & delinquent>0) %>%
  group_by(never_delinquent, delinquent_once, delinquent_more) %>%
  summarise(n=n_distinct(account_number))

# Financial assistance
portland_panel_2024q1 <- portland_panel_estimation %>%
  filter(year(bill_date)==2024, quarter(bill_date)==1, type_code=="REGLR")

portland_panel_2024q1_fa <- portland_panel_2024q1 %>%
  filter(!is.na(linc_tier_type), !senior_disabilities)

portland_panel_2024q1_fa_summary <- portland_panel_2024q1_fa %>%
  mutate(discount_rate=discount_assistance*(-1)/bill_before_assistance,
         discount_rate=case_when(discount_rate<0 ~ 0,
                                 discount_rate>1 ~ 1,
                                 .default=discount_rate)) %>%
  group_by(linc_tier_type, delinquent) %>%
  summarise(mean_bill_before_assistance=mean(bill_before_assistance, na.rm=TRUE),
            median_bill_before_assistance=median(bill_before_assistance, na.rm=TRUE),
            sd_bill_before_assistance=sd(bill_before_assistance, na.rm=TRUE),
            min_bill_before_assistance=min(bill_before_assistance, na.rm=TRUE),
            max_bill_before_assistance=max(bill_before_assistance, na.rm=TRUE),
            mean_net_after_assistance=mean(net_after_assistance, na.rm=TRUE),
            median_net_after_assistance=median(net_after_assistance, na.rm=TRUE),
            sd_net_after_assistance=sd(net_after_assistance, na.rm=TRUE),
            min_net_after_assistance=min(net_after_assistance, na.rm=TRUE),
            max_net_after_assistance=max(net_after_assistance, na.rm=TRUE),
            mean_discount_rate=mean(discount_rate, na.rm=TRUE),
            median_discount_rate=median(discount_rate, na.rm=TRUE),
            sd_discount_rate=sd(discount_rate, na.rm=TRUE),
            min_discount_rate=min(discount_rate, na.rm=TRUE),
            max_discount_rate=max(discount_rate, na.rm=TRUE),
            n=n_distinct(account_number)) %>%
  ungroup()
