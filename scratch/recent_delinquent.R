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


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Get Relevant Data
#---------+---------+---------+---------+---------+---------+
account_info <- read.csv(file=paste0(data_dir, "/UM00200M.csv"),
                         fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

bill_info <- read.csv(file=paste0(data_dir, "/UM00260T.csv"),
                      fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

financial_assist <- read.csv(file=paste0(data_dir, "/UM00232T.csv"),
                             fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

bill_info <- bill_info %>%
  mutate(PERIOD_FROM_DT=mdy(START_DATE),
         PERIOD_TO_DT=mdy(END_DATE),
         DUE_DT=mdy(DUE_DATE),
         BILL_DATE=mdy(BILL_DATE))

bill_info_filtered <- bill_info %>% 
  filter(!IS_CANCELED,
         !is.na(PERIOD_FROM_DT), 
         !is.na(PERIOD_TO_DT),
         !is.na(DUE_DT),
         !IS_ERROR,
         !IS_VOIDED,
         TYPE_CODE %in% c("REGLR", "MSTMT", "FINAL"),
         SOURCE_CODE %in% c("", "QB1", "QB2", "QB3"),
         !IS_CORRECTED)

# Attach financial info onto payment arrangement
financial_assist_account <- financial_assist %>%
  rename(ACCOUNT_NO=ACCOUNT_NUMBER) %>%
  mutate(financial_assist_start=mdy(EFFECTIVE_DATE),
         financial_assist_end=mdy(EXPIRATION_DATE)) %>%
  filter(year(financial_assist_start)>=2019 |
           year(financial_assist_end)>=2019) %>%
  select(ACCOUNT_NO, financial_assist_start, financial_assist_end)

# Financial assistance
financial_assist_time <- financial_assist_account %>%
  mutate(SS_ACCOUNT_NO=ACCOUNT_NO) %>%
  group_by(SS_ACCOUNT_NO) %>%
  arrange(financial_assist_start, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(financial_assist_start)) >
                            cummax(as.numeric(financial_assist_end)))[-n()])) %>%
  group_by(SS_ACCOUNT_NO, indx) %>%
  summarise(financial_assist_start=min(financial_assist_start), 
            financial_assist_end=max(financial_assist_end)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(financial_assist_start=ifelse(is.na(financial_assist_start),
                                       mdy("12/31/2099"),
                                       financial_assist_start),
         financial_assist_end=ifelse(is.na(financial_assist_end),
                                     mdy("12/31/2099"),
                                     financial_assist_end))

financial_assist_time_count <- financial_assist_time %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(count=n())

financial_assist_time_count_1 <- financial_assist_time_count %>%
  filter(count==1)

financial_assist_time_count_above_1 <- financial_assist_time_count %>%
  filter(count>1)

financial_assist_time_above_1 <- financial_assist_time %>%
  filter(SS_ACCOUNT_NO %in% financial_assist_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status <- bill_info_filtered %>%
  rename(ACCOUNT_NO=ACCOUNT_NUMBER)

delinquency_status_none <- delinquency_status %>%
  filter(!(ACCOUNT_NO %in% c(financial_assist_time_count_above_1$SS_ACCOUNT_NO,
                             financial_assist_time_count_1$SS_ACCOUNT_NO))) %>%
  mutate(financial_assist=FALSE)

delinquency_status_sub <- delinquency_status %>%
  filter(ACCOUNT_NO %in% financial_assist_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_rest <- delinquency_status %>%
  filter(ACCOUNT_NO %in% financial_assist_time_count_1$SS_ACCOUNT_NO) %>%
  left_join(financial_assist_time %>%
              filter(SS_ACCOUNT_NO %in% financial_assist_time_count_1$SS_ACCOUNT_NO),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO")) %>%
  rowwise() %>%
  mutate(financial_assist=between(BILL_DATE, financial_assist_start, financial_assist_end)) %>%
  ungroup() %>%
  mutate(financial_assist=ifelse(is.na(financial_assist), FALSE, financial_assist)) %>%
  select(-financial_assist_start, -financial_assist_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(financial_assist=any(BILL_DATE %between%
                                list(subset(financial_assist_time_above_1,
                                            SS_ACCOUNT_NO==ACCOUNT_NO)$financial_assist_start,
                                     subset(financial_assist_time_above_1,
                                            SS_ACCOUNT_NO==ACCOUNT_NO)$financial_assist_end))) %>%
  ungroup()

portland_panel <- rbind(delinquency_status_sub,
                        delinquency_status_rest,
                        delinquency_status_none)

account_writeoff <- account_info %>%
  rename(ACCOUNT_NO=ACCOUNT_NUMBER,
         LAST_BILL_DT=LAST_BILL_DATE,
         LAST_BILL_AMT=LAST_BILL_AMOUNT) %>%
  filter(STATUS_CODE=="WRTOF") %>%
  select(ACCOUNT_NO, LAST_BILL_DT, WRITEOFF_AMT=LAST_BILL_AMT) %>%
  mutate(LAST_BILL_DT=mdy(LAST_BILL_DT),
         WRITEOFF_AMT=as.numeric(WRITEOFF_AMT))

portland_panel <- portland_panel %>%
  left_join(account_writeoff,
            by=c("ACCOUNT_NO", "BILL_DATE"="LAST_BILL_DT"))

# Flag first bill and resumption
portland_panel <- portland_panel %>%
  rename(BILL_TP=TYPE_CODE,
         PREV_BILL_AMT=PREVIOUS_BILL_AMOUNT,
         AR_DUE_BEFORE_BILL=AR_DUE_BEFORE_BILL,
         AR_DUE_AFTER_BILL=AR_DUE_AFTER_BILL) %>%
  arrange(ACCOUNT_NO, BILL_DATE) %>%
  group_by(ACCOUNT_NO) %>%
  mutate(rnum=row_number(),
         lag_final=lag(BILL_TP)) %>%
  ungroup() %>%
  mutate(lag_PREV_BILL=lag(PREV_BILL_AMT)) %>%
  filter(!(lag_PREV_BILL==PREV_BILL_AMT & lag_final=="FINAL" & BILL_TP=="FINAL")) %>%
  mutate(BILL_TP=
           case_when(BILL_TP!="FINAL" & PREV_BILL_AMT<=0 & TOTAL_PAYMENTS>=0 & rnum==1 ~ "FIRST",
                     BILL_TP!="FINAL" & lag_final=="FINAL" & !is.na(lag_final) ~ "RESUME",
                     BILL_TP!="FINAL" & PREV_BILL_AMT<=0 & TOTAL_PAYMENTS<0 & rnum==1 ~ "RESUME",
                     .default=BILL_TP)) %>%
  select(-lag_final, -lag_PREV_BILL, -rnum)

# Get financial assistance info
financial_assist_detail <- list.files(path=paste0(data_dir, "/LINC Data"),
                                      pattern="Linc Data - .*\\.csv",
                                      full.names=TRUE) %>% 
  lapply(read_csv, col_types=cols(.default="c")) %>%
  bind_rows

financial_assist_xlsx <- list.files(path=paste0(data_dir, "/LINC Data"),
                                    pattern="Linc Data - .*\\.xlsx",
                                    full.names=TRUE) %>% 
  lapply(read_xlsx) %>%
  bind_rows

financial_assist_detail <- rbind(financial_assist_detail %>%
                                   mutate(BILL_DT=mdy(BILL_DT),
                                          LINC_EFFECTIVE_DATE=mdy(LINC_EFFECTIVE_DATE),
                                          LINC_EXPIRY_DATE=mdy(LINC_EXPIRY_DATE)),
                                 financial_assist_xlsx %>%
                                   mutate(BILL_DT=ymd(BILL_DT),
                                          LINC_EFFECTIVE_DATE=ymd(LINC_EFFECTIVE_DATE),
                                          LINC_EXPIRY_DATE=ymd(LINC_EXPIRY_DATE)))

financial_assist_detail <- financial_assist_detail %>%
  mutate(bill_year=year(BILL_DT),
         linc_expiry_year=year(LINC_EXPIRY_DATE),
         senior_disabilities=linc_expiry_year>2050) %>%
  mutate_at(c("ACCOUNT_NO",
              "NET_BILL_AMT", "BILLED_AMT_BEFORE_DIS", "LINC_DISCOUNT_AMT",
              "WATER_CONS", "SEWER_CONS",
              "PENALTY_FEES", "PENALTY_FEES_REVERSED",
              "CRISIS_VOUCHER_AMT"),
            as.numeric)

portland_panel <- portland_panel %>%
  left_join(financial_assist_detail %>%
              select(ACCOUNT_NO, BILL_DT, LINC_TIER_TYPE,
                     NET_BILL_AMT, BILLED_AMT_BEFORE_DIS, LINC_DISCOUNT_AMT,
                     CRISIS_VOUCHER_AMT, senior_disabilities),
            by=c("ACCOUNT_NO", "BILL_DATE"="BILL_DT"))

# Aggregate for monthly payments
portland_panel_sub <- portland_panel %>%
  filter(SOURCE_CODE %in% c("QB1", "QB2", "QB3"))

portland_panel <- portland_panel %>%
  filter(SOURCE_CODE=="")

# First sequence must start with QB1, assign group numbers
portland_panel_sub <- portland_panel_sub %>%
  arrange(ACCOUNT_NO, BILL_DATE) %>%
  mutate(source_num=substr(SOURCE_CODE, 3, 3) %>% as.numeric(),
         source_lag=lag(source_num),
         source_lag_lag=lag(source_lag),
         to_keep=case_when(
           source_num==1 ~ TRUE,
           source_num==2 & source_lag==1 ~ TRUE,
           source_num==2 & source_lag!=1 ~ FALSE,
           source_num==3 & source_lag==2 & source_lag_lag==1 ~ TRUE,
           source_num==3 & source_lag==2 & source_lag_lag!=1 ~ FALSE,
           source_num==3 & source_lag!=2 ~ FALSE)) %>%
  filter(to_keep)

portland_panel_sub <- portland_panel_sub %>%
  arrange(source_num, ACCOUNT_NO, BILL_DATE) %>%
  group_by(ACCOUNT_NO) %>%
  mutate(group_num=row_number()) %>%
  ungroup() %>%
  arrange(ACCOUNT_NO, BILL_DATE) %>%
  mutate(group_num=ifelse(source_num!=1, NA, group_num)) %>%
  group_by(ACCOUNT_NO) %>%
  fill(group_num) %>%
  ungroup()

portland_panel_sub <- portland_panel_sub %>%
  mutate(AR_DUE_BEFORE_BILL=replace_na(AR_DUE_BEFORE_BILL, 0)) %>%
  select(-source_num, -source_lag, -source_lag_lag, -to_keep, -group_num)

portland_panel <- rbind(portland_panel_sub, portland_panel) %>%
  arrange(ACCOUNT_NO, BILL_DATE) %>%
  rename(previous_bill=PREV_BILL_AMT,
         total_payments=TOTAL_PAYMENTS,
         leftover_debt=AR_DUE_BEFORE_BILL,
         current_bill=AR_DUE_AFTER_BILL,
         net_after_assistance=NET_BILL_AMT,
         bill_before_assistance=BILLED_AMT_BEFORE_DIS,
         discount_assistance=LINC_DISCOUNT_AMT,
         crisis_voucher_amount=CRISIS_VOUCHER_AMT,
         writeoff_amount=WRITEOFF_AMT) %>%
  distinct()

portland_panel <- portland_panel %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  replace_na(list(senior_disabilities=FALSE))

# Aggregate monthly payments
# First aggregate consumption and relevant bills and assistances
portland_panel_sub <- portland_panel %>%
  filter(SOURCE_CODE!="") %>%
  arrange(ACCOUNT_NO, BILL_DATE) %>%
  mutate(BILL_DATE=ifelse(SOURCE_CODE!="QB1", NA, BILL_DATE)) %>%
  fill(BILL_DATE, .direction="downup") %>%
  mutate(BILL_DATE=as_date(BILL_DATE)) %>%
  group_by(ACCOUNT_NO, BILL_DATE) %>%
  summarise(across(c("financial_assist",
                     "net_after_assistance", "bill_before_assistance", "discount_assistance",
                     "crisis_voucher_amount"),
                   .fns=~sum(., na.rm=TRUE))) %>%
  ungroup() %>%
  mutate(across(c("financial_assist"),
                .fns=~.>0))

portland_panel_quarter <- portland_panel %>%
  filter(SOURCE_CODE=="")

portland_panel_month <- portland_panel %>%
  filter(SOURCE_CODE!="") %>%
  select(-financial_assist,
         -net_after_assistance, -bill_before_assistance, -discount_assistance,
         -crisis_voucher_amount) %>%
  left_join(portland_panel_sub, by=c("ACCOUNT_NO", "BILL_DATE"))

portland_panel_estimation <- rbind(portland_panel_quarter, portland_panel_month) %>%
  arrange(ACCOUNT_NO, BILL_DATE)

# Replace values as needed
portland_panel_estimation <- portland_panel_estimation %>%
  group_by(ACCOUNT_NO) %>%
  mutate(source_num=substr(SOURCE_CODE, 3, 3) %>% as.numeric(),
         across(c("source_num", "BILL_TP", "previous_bill", "total_payments"),
                .fns=list(lag=~lag(.))),
         across(c("source_num", "BILL_TP", "previous_bill", "total_payments"),
                .fns=list(lag_lag=~lag(., n=2))),
         across(c("source_num", "BILL_TP", "previous_bill"),
                .fns=list(lead=~lead(.))),
         across(c("source_num", "BILL_TP", "previous_bill"),
                .fns=list(lead_lead=~lead(., n=2)))) %>%
  ungroup()

portland_panel_estimation <- portland_panel_estimation %>%
  mutate(previous_bill=
           case_when(BILL_TP=="FINAL" & source_num_lag==2 ~
                       previous_bill_lag,
                     source_num_lag_lag==2 ~
                       previous_bill_lag_lag,
                     .default=previous_bill),
         total_payments=
           case_when(BILL_TP=="FINAL" & source_num_lag==2 ~
                       total_payments+total_payments_lag,
                     source_num_lag_lag==2 ~
                       total_payments+total_payments_lag+total_payments_lag_lag,
                     .default=total_payments),
         leftover_debt=case_when(BILL_TP=="FINAL" & source_num_lag==2 ~
                                   previous_bill-total_payments,
                                 .default=leftover_debt),
         current_bill=case_when(source_num==1 & source_num_lead==2 ~
                                  previous_bill_lead,
                                source_num==1 & BILL_TP_lead=="FINAL" ~
                                  previous_bill_lead,
                                .default=current_bill))

portland_panel_estimation <- portland_panel_estimation %>%
  filter(BILL_TP!="MSTMT" |
           (BILL_TP=="MSTMT" & is.na(BILL_TP_lead)) |
           (source_num==2 & source_num_lead==3 & is.na(BILL_TP_lead_lead))) %>%
  mutate(agg=case_when((BILL_TP=="FINAL" & source_num_lag==2) |
                         (source_num_lag==3 & source_num_lag_lag==2) ~ "AGG",
                       (BILL_TP=="MSTMT" & is.na(BILL_TP_lead)) |
                         (BILL_TP=="MSTMT" & is.na(BILL_TP_lead_lead)) ~ "CHOP",
                       .default="")) %>%
  select(-contains("lag"), -contains("lead"), -source_num) %>%
  mutate(delinquent=leftover_debt>0)

portland_panel_2024q1 <- portland_panel_estimation %>%
  filter(year(BILL_DATE)==2024 & quarter(BILL_DATE)==1)

portland_panel_2024q1 %>%
  group_by(ACCOUNT_NO) %>%
  summarise(delinquent=sum(delinquent)) %>%
  ungroup() %>%
  filter(delinquent>0) %>%
  count()

portland_panel_2024q1 %>%
  summarise(n=n_distinct(ACCOUNT_NO))
  
portland_panel_2024q1 %>%
  group_by(ACCOUNT_NO, LINC_TIER_TYPE, senior_disabilities) %>%
  summarise(delinquent=sum(delinquent)) %>%
  ungroup() %>%
  mutate(delinquent=delinquent>0) %>%
  group_by(LINC_TIER_TYPE, delinquent, senior_disabilities) %>%
  summarise(n=n_distinct(ACCOUNT_NO))

portland_panel_2024q1 %>%
  group_by(LINC_TIER_TYPE, senior_disabilities) %>%
  summarise(n=n_distinct(ACCOUNT_NO))
