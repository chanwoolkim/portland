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
load(paste0(working_data_dir, "/portland_transunion.RData"))

portland_panel_2024q2 <- portland_transunion %>%
  filter(year(bill_date)==2024, quarter(bill_date)==2)

portland_panel_2024q2 %>%
  select(tu_id) %>%
  distinct() %>%
  count() #149,328

portland_panel_2024q2 <- portland_panel_2024q2 %>%
  filter(type_code=="REGLR")

portland_panel_2024q2 %>%
  select(tu_id) %>%
  distinct() %>%
  count() #146,063

portland_panel_seniors <- portland_panel_2024q2 %>%
  group_by(linc_tier_type, senior_disabilities, delinquent) %>%
  summarise(n=n_distinct(tu_id)) %>%
  ungroup()

portland_panel_seniors <- portland_panel_seniors %>%
  spread(delinquent, n) %>%
  mutate(n_account=`FALSE`+`TRUE`,
         n_delinquent=`TRUE`) %>%
  select(-`TRUE`, -`FALSE`) %>%
  bind_rows(summarise(., 
                      across(where(is.numeric), sum),
                      across(where(is.character), ~'Total'))) %>%
  mutate(share_delinquent=n_delinquent/n_account*100)

tab <- TexRow(c("Assistance", 
                "Seniors and Disabled",
                "# Accounts", 
                "# Delinquent Accounts",
                "Share")) +
  TexMidrule() +
  TexRow(c("Tier 1", "Yes")) /
  TexRow(portland_panel_seniors[2, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_seniors[1, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("Tier 2", "Yes")) /
  TexRow(portland_panel_seniors[4, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_seniors[3, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("No Assistance", "N/A")) /
  TexRow(portland_panel_seniors[5, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("Total", "N/A")) /
  TexRow(portland_panel_seniors[6, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE))
  
TexSave(tab, filename="fa_honored", positions=rep('c', 5),
        output_path=output_dir, stand_alone=FALSE)

portland_panel_2024q2 <- portland_panel_2024q2 %>%
  filter(!senior_disabilities)

portland_panel_2024q2 %>%
  select(tu_id) %>%
  distinct() %>%
  count() #141,249

# Descriptive on accounts
delinquent_threshold <- function(d) {
  portland_panel_2024q2 %>%
    mutate(delinquent=leftover_debt>d) %>%
    group_by(linc_tier_type, delinquent) %>%
    summarise(n=n_distinct(tu_id))
}

delinquent_threshold(0)
delinquent_threshold(10)
delinquent_threshold(50)
delinquent_threshold(115)

portland_panel_2024q2 <- portland_panel_estimation %>%
  filter(tu_id %in% portland_panel_2024q2$tu_id)

delinquent_threshold <- function(d) {
  portland_panel_2024q2 %>%
    mutate(delinquent=leftover_debt>d) %>%
    arrange(tu_id, bill_date) %>%
    group_by(tu_id) %>%
    mutate(delinquent_lag=lag(delinquent, 1)) %>%
    ungroup() %>%
    mutate(delinquent_consistent=(delinquent & delinquent_lag)) %>%
    filter(year(bill_date)==2024, quarter(bill_date)==1,
           type_code=="REGLR", !senior_disabilities) %>%
    group_by(linc_tier_type, delinquent_consistent) %>%
    summarise(n=n_distinct(tu_id))
}

delinquent_threshold(0)
delinquent_threshold(10)
delinquent_threshold(50)
delinquent_threshold(115)

delinquent_threshold <- function(d) {
  portland_panel_2024q2 %>%
    mutate(delinquent=leftover_debt>d) %>%
    arrange(tu_id, bill_date) %>%
    group_by(tu_id) %>%
    mutate(delinquent_lag=lag(delinquent, 1),
           delinquent_lag_lag=lag(delinquent, 2)) %>%
    ungroup() %>%
    mutate(delinquent_consistent=(delinquent & delinquent_lag & delinquent_lag_lag)) %>%
    filter(year(bill_date)==2024, quarter(bill_date)==1,
           type_code=="REGLR", !senior_disabilities) %>%
    group_by(linc_tier_type, delinquent_consistent) %>%
    summarise(n=n_distinct(tu_id))
}

delinquent_threshold(0)
delinquent_threshold(10)
delinquent_threshold(50)
delinquent_threshold(115)

# Non-consecutive missers
portland_panel_estimation %>%
  arrange(tu_id, bill_date) %>%
  group_by(tu_id) %>%
  mutate(delinquent_lag=lag(delinquent, 1)) %>%
  ungroup() %>%
  mutate(delinquent_once=(delinquent & !delinquent_lag)) %>%
  filter(type_code=="REGLR") %>%
  group_by(tu_id, bill_year) %>%
  summarise(delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_once=sum(delinquent_once, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquent=delinquent>0,
         delinquent_once=delinquent_once>0) %>%
  group_by(delinquent, delinquent_once, bill_year) %>%
  summarise(n=n_distinct(tu_id))

portland_panel_estimation %>%
  arrange(tu_id, bill_date) %>%
  group_by(tu_id) %>%
  filter(any(bill_year==2021),
         any(bill_year==2024)) %>%
  mutate(delinquent_lag=lag(delinquent, 1)) %>%
  ungroup() %>%
  mutate(delinquent_once=(delinquent & !delinquent_lag)) %>%
  filter(type_code=="REGLR") %>%
  group_by(tu_id, bill_year) %>%
  summarise(delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_once=sum(delinquent_once, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(never_delinquent=delinquent==0,
         delinquent_once=delinquent==delinquent_once & delinquent>0,
         delinquent_more=delinquent>delinquent_once & delinquent>0) %>%
  group_by(never_delinquent, delinquent_once, delinquent_more) %>%
  summarise(n=n_distinct(tu_id))

# Financial assistance
portland_panel_2024q2 <- portland_panel_estimation %>%
  filter(year(bill_date)==2024, quarter(bill_date)==1, type_code=="REGLR")

portland_panel_2024q2_fa <- portland_panel_2024q2 %>%
  filter(!is.na(linc_tier_type), !senior_disabilities)

portland_panel_2024q2_fa_summary <- portland_panel_2024q2_fa %>%
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
            n=n_distinct(tu_id)) %>%
  ungroup()
