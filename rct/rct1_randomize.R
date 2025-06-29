# Choose RCT1 subjects
set.seed(2024)

# Load data
load(paste0(working_data_dir, "/portland_transunion.RData"))
load(paste0(working_data_dir, "/portland_demographics_tract.RData"))

recent_delinquent <- read_xlsx(paste0(auxiliary_data_dir,
                                      "/TU IDs for Booth Anonymized 10112024.xlsx"))

exclusion_accounts <- read_xlsx(paste0(auxiliary_data_dir,
                                       "/SDP RCT 1 Exclusions Anonymized 10112024 .xlsx"))

rct_n <- 20000
rct_discount_percentage <- seq(0, 80, 10)
rct_discount_tier2 <- seq(50, 80, 10)

portland_panel_2024q2 <- portland_transunion %>%
  left_join(portland_demographics_tract_wide %>%
              mutate(tract=as.numeric(tract)),
            by="tract") %>%
  filter(year(bill_date)==2024, quarter(bill_date)==2,
         type_code=="REGLR", cycle_code=="QUARTER")

portland_panel_2024q2 %>% summarise(n=n_distinct(tu_id)) #146,063

portland_tu_id_count <- portland_panel_2024q2 %>% 
  summarise(n=n_distinct(tu_id)) %>%
  ungroup() %>%
  pull(n)

export_tex(prettyNum(portland_tu_id_count, big.mark=",", scientific=FALSE),
           "portland_tu_id_count")

portland_panel_2024q2 <- portland_panel_2024q2 %>%
  filter(!senior_disabilities)

portland_panel_2024q2 %>% summarise(n=n_distinct(tu_id)) #143,923

portland_exclude_honored_count <- portland_panel_2024q2 %>% 
  summarise(n=n_distinct(tu_id)) %>%
  ungroup() %>%
  pull(n)

portland_exclude_honored_share <- round(portland_exclude_honored_count/portland_tu_id_count*100, 1)
export_tex(paste0("(", portland_exclude_honored_share, "\\%)"),
           "portland_exclude_honored_count")

portland_panel_2024q2 <- portland_panel_2024q2 %>%
  filter(!is.na(credit_score), !is.na(etie)) %>%
  distinct(tu_id, .keep_all=TRUE)

portland_panel_2024q2 %>% summarise(n=n_distinct(tu_id)) #131,576

portland_delinquent <- portland_panel_2024q2 %>%
  filter(delinquent) %>%
  select(tu_id)

rct_delinquent_n <- portland_delinquent %>% count() %>% as.numeric() #7,674
export_tex(prettyNum(rct_delinquent_n, big.mark=",", scientific=FALSE),
           "portland_rct_delinquent")

rct_delinquent_rev_n <- 20000-rct_delinquent_n
export_tex(prettyNum(rct_delinquent_rev_n, big.mark=",", scientific=FALSE),
           "portland_rct_delinquent_rev")

portland_panel_2024q2 %>% summarise(n=sum(!delinquent, na.rm=TRUE)) #123,902

portland_rct_subject <- bind_rows(portland_delinquent,
                                  portland_panel_2024q2 %>%
                                    filter(!delinquent) %>%
                                    sample_n(rct_n-rct_delinquent_n) %>%
                                    select(tu_id))

portland_rct_subject <- portland_panel_2024q2 %>%
  filter(tu_id %in% portland_rct_subject$tu_id)

# Assign percentage
portland_non_tier2 <- portland_rct_subject %>%
  filter(linc_tier_type!="Tier2" | is.na(linc_tier_type)) %>%
  mutate(discount_percentage=sample(rct_discount_percentage, n(), replace=TRUE))

portland_tier2 <- portland_rct_subject %>%
  filter(linc_tier_type=="Tier2") %>%
  mutate(discount_percentage=sample(rct_discount_tier2, n(), replace=TRUE))

portland_rct_subject <- bind_rows(portland_non_tier2, portland_tier2)

# Descriptive statistics
portland_rct_subject_descriptive <- bind_rows(
  portland_rct_subject %>%
    group_by(discount_percentage) %>%
    summarise(n=n(), 
              n_tier1=sum(linc_tier_type=="Tier1", na.rm=TRUE),
              n_tier2=sum(linc_tier_type=="Tier2", na.rm=TRUE),
              n_delinquent=sum(delinquent, na.rm=TRUE),
              mean_credit_score=mean(credit_score, na.rm=TRUE),
              mean_income=mean(etie*1000, na.rm=TRUE),
              mean_hh_income=mean(hh_income, na.rm=TRUE),
              mean_hh_size=mean(hh_size, na.rm=TRUE),
              mean_hh_poverty=mean(hh_poverty, na.rm=TRUE),
              mean_black=mean(black, na.rm=TRUE),
              mean_hispanic=mean(hispanic, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(discount_percentage=as.character(discount_percentage)),
  portland_rct_subject %>%
    summarise(n=n(), 
              n_tier1=sum(linc_tier_type=="Tier1", na.rm=TRUE),
              n_tier2=sum(linc_tier_type=="Tier2", na.rm=TRUE),
              n_delinquent=sum(delinquent, na.rm=TRUE),
              mean_credit_score=mean(credit_score, na.rm=TRUE),
              mean_income=mean(etie*1000, na.rm=TRUE),
              mean_hh_income=mean(hh_income, na.rm=TRUE),
              mean_hh_size=mean(hh_size, na.rm=TRUE),
              mean_hh_poverty=mean(hh_poverty, na.rm=TRUE),
              mean_black=mean(black, na.rm=TRUE),
              mean_hispanic=mean(hispanic, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(discount_percentage="Total"))

descriptive_stat_tex <- function(descriptive_result) {
  row_tr <- function(row) {
    tab_row <- TexRow(descriptive_result[row, 1] %>% as.character()) /
      TexRow(descriptive_result[row, 2:5] %>% as.numeric(), 
             dec=rep(0, 4))
    return(tab_row)
  }
  
  tab <- TexRow(c("", "Number of Accounts"),
                cspan=c(1, 4)) +
    TexMidrule(list(c(2, 5))) +
    TexRow(c("\\% Discount", 
             "Total", "FA: Tier 1", "FA: Tier 2", "Delinquent")) +
    TexMidrule() +
    row_tr(1) + row_tr(2) + row_tr(3) + 
    row_tr(4) + row_tr(5) + row_tr(6) + 
    row_tr(7) + row_tr(8) + row_tr(9) +
    TexMidrule() +
    row_tr(10)
  return(tab)
}

tab <- descriptive_stat_tex(portland_rct_subject_descriptive)
TexSave(tab, filename="rct_subject_n", positions=rep('c', 5),
        output_path=output_dir, stand_alone=FALSE)

descriptive_stat_tex <- function(descriptive_result) {
  row_tr <- function(row) {
    tab_row <- TexRow(descriptive_result[row, 1] %>% as.character()) /
      TexRow(descriptive_result[row, 6:12] %>% as.numeric(), 
             dec=c(rep(0, 3), rep(2, 4)))
    return(tab_row)
  }
  
  tab <- TexRow(c("", "TransUnion Statistics", "Census Statistics"),
                cspan=c(1, 2, 5)) +
    TexMidrule(list(c(2, 3), c(4, 8))) +
    TexRow(c("\\% Discount",
             "Credit Score", "Income", 
             "Income", "Household Size", "\\% Poverty", "\\% Black", "\\% Hispanic")) +
    TexMidrule() +
    row_tr(1) + row_tr(2) + row_tr(3) + 
    row_tr(4) + row_tr(5) + row_tr(6) + 
    row_tr(7) + row_tr(8) + row_tr(9) +
    TexMidrule() +
    row_tr(10)
  return(tab)
}

# Distribution of billing cycles
for (p in rct_discount_percentage) {
  portland_rct_percent <- portland_rct_subject %>%
    filter(discount_percentage==p)
  
  gg <- ggplot() +
    geom_histogram(data=portland_rct_percent,
                   aes(x=cycle_num)) +
    fte_theme() +
    labs(x="Billing Cycle", y="Number of Accounts")
  gg
  ggsave(gg,
         filename=paste0(output_dir, "/rct_subject_cycle_", p, ".png"),
         width=6, height=4)
}

tab <- descriptive_stat_tex(portland_rct_subject_descriptive)
TexSave(tab, filename="rct_subject_descriptive", positions=rep('c', 8),
        output_path=output_dir, stand_alone=FALSE)

write_csv(portland_rct_subject %>%
            select(tu_id, discount_percentage, cycle_num, linc_tier_type, delinquent),
          file=paste0(working_data_dir, "/portland_rct_subject.csv"))


# Choose additional subjects ####
n_additional <- nrow(exclusion_accounts)

portland_additional <- portland_panel_2024q2 %>%
  filter(tu_id %in% recent_delinquent$TU_NUMBER,
         !tu_id %in% portland_rct_subject$tu_id,
         cycle_num %in% c(1:13, 48:63)) %>%
  select(tu_id, linc_tier_type, cycle_num, delinquent)

portland_additional_random <- portland_panel_2024q2 %>%
  filter(!tu_id %in% portland_additional$tu_id,
         !tu_id %in% portland_rct_subject$tu_id,
         !tu_id %in% exclusion_accounts$`Tu Id`,
         cycle_num %in% c(1:13, 48:63)) %>%
  sample_n(n_additional-nrow(portland_additional)) %>%
  select(tu_id, linc_tier_type, cycle_num, delinquent)

portland_additional <- bind_rows(portland_additional, portland_additional_random)

# Assign percentage
portland_non_tier2 <- portland_additional %>%
  filter(linc_tier_type!="Tier2" | is.na(linc_tier_type)) %>%
  mutate(discount_percentage=sample(rct_discount_percentage, n(), replace=TRUE))

portland_tier2 <- portland_additional %>%
  filter(linc_tier_type=="Tier2") %>%
  mutate(discount_percentage=sample(rct_discount_tier2, n(), replace=TRUE))

portland_additional <- bind_rows(portland_non_tier2, portland_tier2)

write_csv(portland_additional %>% 
            select(tu_id, discount_percentage, cycle_num, linc_tier_type, delinquent),
          file=paste0(working_data_dir, "/portland_rct_additional.csv"))
