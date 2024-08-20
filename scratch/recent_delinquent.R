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
tu_data_dir <- paste0(wd, "/data/raw/TU")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))

# Load data
load(paste0(working_data_dir, "/portland_transunion.RData"))

portland_panel_2024q2 <- portland_transunion %>%
  filter(year(bill_date)==2024, quarter(bill_date)==2) %>%
  mutate(etie=etie*1000)

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

# Seniors and disabled
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
                "\\# Accounts", 
                "\\# Delinquent Accounts",
                "Share")) +
  TexMidrule() +
  TexRow(c("Tier 1", "Yes")) /
  TexRow(portland_panel_seniors[2, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_seniors[1, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("Tier 2", "Yes")) /
  TexRow(portland_panel_seniors[4, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_seniors[3, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("No Assistance", "N/A")) /
  TexRow(portland_panel_seniors[5, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
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
  count() #143,923

# Descriptive on TU match
portland_panel_tu <- portland_panel_2024q2 %>%
  group_by(linc_tier_type, delinquent, tu_match) %>%
  summarise(n=n_distinct(tu_id)) %>%
  ungroup()

portland_panel_tu <- portland_panel_tu %>%
  spread(tu_match, n) %>%
  mutate(n_account=`FALSE`+`TRUE`,
         n_missing=`FALSE`) %>%
  select(-`TRUE`, -`FALSE`) %>%
  bind_rows(summarise(., 
                      across(where(is.numeric), sum),
                      across(where(is.character), ~'Total'))) %>%
  mutate(share_delinquent=n_missing/n_account*100)

tab <- TexRow(c("Assistance",
                "Delinquent",
                "\\# Accounts", 
                "\\# Accounts Without TU",
                "Share")) +
  TexMidrule() +
  TexRow(c("Tier 1", "Yes")) /
  TexRow(portland_panel_tu[2, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu[1, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("Tier 2", "Yes")) /
  TexRow(portland_panel_tu[4, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu[3, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("No Assistance", "Yes")) /
  TexRow(portland_panel_tu[6, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu[5, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("Total", "N/A")) /
  TexRow(portland_panel_tu[7, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE))

TexSave(tab, filename="tu_match", positions=rep('c', 5),
        output_path=output_dir, stand_alone=FALSE)

# TU match with income and ethnicity info
portland_panel_tu_all <- portland_panel_2024q2 %>%
  mutate(tu_all=!is.na(etie)) %>%
  group_by(linc_tier_type, delinquent, tu_all) %>%
  summarise(n=n_distinct(tu_id)) %>%
  ungroup()

portland_panel_tu_all <- portland_panel_tu_all %>%
  spread(tu_all, n) %>%
  mutate(n_account=`FALSE`+`TRUE`,
         n_missing=`FALSE`) %>%
  select(-`TRUE`, -`FALSE`) %>%
  bind_rows(summarise(., 
                      across(where(is.numeric), sum),
                      across(where(is.character), ~'Total'))) %>%
  mutate(share_delinquent=n_missing/n_account*100)

tab <- TexRow(c("Assistance",
                "Delinquent",
                "\\# Accounts", 
                "\\# Accounts Without TU",
                "Share")) +
  TexMidrule() +
  TexRow(c("Tier 1", "Yes")) /
  TexRow(portland_panel_tu_all[2, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_all[1, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("Tier 2", "Yes")) /
  TexRow(portland_panel_tu_all[4, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_all[3, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("No Assistance", "Yes")) /
  TexRow(portland_panel_tu_all[6, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_all[5, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE)) +
  TexMidrule() +
  TexRow(c("Total", "N/A")) /
  TexRow(portland_panel_tu_all[7, 3:5] %>% as.numeric(),
         dec=c(0, 0, 1), percentage=c(FALSE, FALSE, TRUE))

TexSave(tab, filename="tu_match_all", positions=rep('c', 5),
        output_path=output_dir, stand_alone=FALSE)

# TU income, ethnicity, and credit info
portland_panel_tu_credit <- portland_panel_2024q2 %>%
  filter(!is.na(credit_score)) %>%
  mutate(missing_income=is.na(etie)) %>%
  group_by(linc_tier_type, delinquent, missing_income) %>%
  summarise(mean_credit_score=mean(credit_score, na.rm=TRUE),
            sd_credit_score=sd(credit_score, na.rm=TRUE),
            n=n_distinct(tu_id)) %>%
  ungroup() %>%
  pivot_wider(names_from=missing_income,
              values_from=c(mean_credit_score, sd_credit_score, n))

tab <- TexRow(c("", "Non-Missing Income", "Missing Income"), 
              cspan=c(2, 3, 3)) +
  TexMidrule(list(c(3, 5), c(6, 8))) +
  TexRow(c("Assistance", "Delinquent", "Mean", "SD", "N", "Mean", "SD","N")) +
  TexMidrule() +
  TexRow(c("Tier 1", "Yes")) /
  TexRow(portland_panel_tu_credit[2, c(3, 5, 7, 4, 6, 8)] %>% as.numeric(),
         dec=rep(0, 6)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_credit[1, c(3, 5, 7, 4, 6, 8)] %>% as.numeric(),
         dec=rep(0, 6)) +
  TexMidrule() +
  TexRow(c("Tier 2", "Yes")) /
  TexRow(portland_panel_tu_credit[4, c(3, 5, 7, 4, 6, 8)] %>% as.numeric(),
         dec=rep(0, 6)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_credit[3, c(3, 5, 7, 4, 6, 8)] %>% as.numeric(),
         dec=rep(0, 6)) +
  TexMidrule() +
  TexRow(c("No Assistance", "Yes")) /
  TexRow(portland_panel_tu_credit[6, c(3, 5, 7, 4, 6, 8)] %>% as.numeric(),
         dec=rep(0, 6)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_credit[5, c(3, 5, 7, 4, 6, 8)] %>% as.numeric(),
         dec=rep(0, 6))

TexSave(tab, filename="tu_credit", positions=rep('c', 8),
        output_path=output_dir, stand_alone=FALSE)

# TU income, ethnicity, and credit info
portland_panel_tu_summary <- portland_panel_2024q2 %>%
  filter(!is.na(credit_score), !is.na(etie)) %>%
  group_by(linc_tier_type, delinquent) %>%
  summarise(mean_credit_score=mean(credit_score, na.rm=TRUE),
            sd_credit_score=sd(credit_score, na.rm=TRUE),
            mean_hh_income=mean(etie, na.rm=TRUE),
            sd_hh_income=sd(etie, na.rm=TRUE),
            n=n_distinct(tu_id)) %>%
  ungroup()

tab <- TexRow(c("", "Credit Score", "Estimated Household Income", ""), 
              cspan=c(2, 2, 2, 1)) +
  TexMidrule(list(c(3, 4), c(5, 6))) +
  TexRow(c("Assistance", "Delinquent", "Mean", "SD", "Mean", "SD", "N")) +
  TexMidrule() +
  TexRow(c("Tier 1", "Yes")) /
  TexRow(portland_panel_tu_summary[2, 3:7] %>% as.numeric(),
         dec=rep(0, 5)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_summary[1, 3:7] %>% as.numeric(),
         dec=rep(0, 5)) +
  TexMidrule() +
  TexRow(c("Tier 2", "Yes")) /
  TexRow(portland_panel_tu_summary[4, 3:7] %>% as.numeric(),
         dec=rep(0, 5)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_summary[3, 3:7] %>% as.numeric(),
         dec=rep(0, 5)) +
  TexMidrule() +
  TexRow(c("No Assistance", "Yes")) /
  TexRow(portland_panel_tu_summary[6, 3:7] %>% as.numeric(),
         dec=rep(0, 5)) +
  TexRow(c("", "No")) /
  TexRow(portland_panel_tu_summary[5, 3:7] %>% as.numeric(),
         dec=rep(0, 5))

TexSave(tab, filename="tu_summary", positions=rep('c', 7),
        output_path=output_dir, stand_alone=FALSE)

# % unpaid conditional on defaulting
portland_panel_2024q2 <- portland_panel_2024q2 %>%
  filter(!is.na(credit_score), !is.na(etie)) %>%
  mutate(unpaid_percent=leftover_debt/previous_bill,
         unpaid_percent=case_when(
           unpaid_percent>1 ~ 1,
           unpaid_percent<0 ~ 0,
           TRUE ~ unpaid_percent)) %>%
  filter(delinquent)

gg <- ggplot() +
  geom_histogram(data=portland_panel_2024q2, 
                 aes(x=unpaid_percent), 
                 bins=20) +
  labs(x="Unpaid Percent",
       y="Number of Accounts") +
  scale_x_continuous(labels=scales::percent) +
  fte_theme()
gg
ggsave(gg,
       filename=paste0(output_dir, "/unpaid_percent.png"),
       width=6, height=4)
