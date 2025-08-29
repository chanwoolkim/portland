#=========================================================================#
# bill_breakdown_table_financial.R
# 
# Breakdown of total billed amount by income and credit quartiles (in 2024Q4)
# - Water Bills
#
# July 10, 2025
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Helper Functions
#---------+---------+---------+---------+---------+---------+
default_zero <- function(df) {
  if (!("liens" %in% names(df))) df$liens <- 0
  return(df)
}

fill_zeros <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ ifelse(is.na(.), 0, .)))
}

accumulate_debt <- function(df, group_var) {
  for (g in unique(df[[group_var]])) {
    idx <- which(df[[group_var]] == g)
    for (i in seq_along(idx)) {
      if (i == 1) {
        carryover <- 0
      } else {
        prev <- idx[i - 1]
        carryover <- df$leftover_debt[prev] - df$final_leftover[prev] - df$liens[prev]
      }
      cur <- idx[i]
      df$leftover_debt[cur] <- df$bill[cur] + carryover -
        df$discount[cur] - df$cleanriver_discount[cur] -
        df$final_discount[cur] - df$payment[cur] +
        df$total_outstanding[cur]
    }
  }
  return(df)
}

export_stat_tex <- function(value, label, unit = "million", percent = FALSE) {
  out <- if (percent) paste0(value, "\\%") else paste0("\\$", value, " ", unit)
  export_tex(out, label)
}

summarize_by_group <- function(df, group_vars, sum_vars) {
  df %>% 
    group_by(across(all_of(group_vars))) %>% 
    summarise(across(all_of(sum_vars), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
    arrange(across(all_of(group_vars)))
}


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
load(paste0(working_data_dir, "/transunion/analysis/estimation_dataset_all.RData"))
code_info <- read_csv(paste0(working_data_dir, "/transunion/analysis/code_info.csv"))
transaction_aggregate <- read_csv(paste0(working_data_dir, "/transunion/analysis/2024q4_financial/transaction_aggregate.csv"))
payment_plan_aggregate <- read_csv(paste0(working_data_dir, "/transunion/analysis/2024q4_financial/payment_plan_aggregate.csv"))
collection_aggregate <- read_csv(paste0(working_data_dir, "/transunion/analysis/2024q4_financial/collection_aggregate.csv"))
final_aggregate <- read_csv(paste0(working_data_dir, "/transunion/analysis/2024q4_financial/final_aggregate.csv"))
account_count <- read_csv(paste0(working_data_dir, "/transunion/analysis/2024q4_financial/account_count.csv"))

income_credit <- estimation_dataset_all %>%
  arrange(id, bill_date) %>%
  group_by(id) %>%
  fill(tu_income, .direction="downup") %>%
  ungroup() %>%
  filter(year(bill_date)==2024) %>%
  select(id, tu_income) %>%
  distinct()

ufh_threshold <- round(as.numeric(quantile(income_credit$tu_income, 0.15, na.rm=TRUE)*1.037*1000), 0)
ufh_threshold <- paste0("\\$",
                        prettyNum(ufh_threshold, big.mark=",", scientific=FALSE))
export_tex(ufh_threshold, "ufh_threshold")

account_count_median <- account_count %>%
  group_by(median_status) %>%
  summarise(n=sum(n, na.rm=TRUE)) %>%
  ungroup()

account_count <- account_count %>% 
  mutate(income_quartile=case_when(
    ufh_status & fa_status ~ 1,
    !ufh_status & fa_status ~ 2,
    !ufh_status & !fa_status ~ 3,
    .default=NA)) %>%
  filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
  group_by(income_quartile, credit_quartile) %>%
  summarise(n=sum(n, na.rm=TRUE)) %>%
  arrange(income_quartile, credit_quartile)

account_count_income <- account_count %>% 
  group_by(income_quartile) %>% 
  summarise(n=sum(n, na.rm=TRUE)) %>% 
  ungroup()

account_count_credit <- account_count %>%
  group_by(credit_quartile) %>%
  summarise(n=sum(n, na.rm=TRUE)) %>%
  ungroup()

if (!("liens" %in% names(final_aggregate))) {
  final_aggregate$liens <- 0
}

transaction_aggregate_summary_median <- transaction_aggregate %>% 
  filter(!is.na(median_status)) %>%
  left_join(code_info %>% select(transaction_type, category),
            by="transaction_type") %>%
  filter(!grepl("COMMIT", transaction_type)) %>%
  group_by(year, quarter, median_status, category) %>%
  summarise(total_amount=sum(total_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  spread(category, total_amount) %>%
  left_join(payment_plan_aggregate %>%
              mutate_all(as.numeric) %>%
              filter(!is.na(median_status)) %>%
              group_by(year, quarter, median_status) %>%
              summarise(total_payment_arrange=sum(total_amount_sum, na.rm=TRUE),
                        total_outstanding=sum(total_outstanding_amount_sum, na.rm=TRUE),
                        total_payment_made=sum(difference_sum, na.rm=TRUE),
                        total_delayed=sum(remaining_amount_sum, na.rm=TRUE)) %>%
              ungroup(),
            by=c("year", "quarter", "median_status")) %>%
  left_join(final_aggregate %>%
              mutate_all(as.numeric) %>%
              filter(!is.na(median_status)) %>%
              group_by(year, quarter, median_status) %>%
              summarise(final_discount=sum(discount_amount, na.rm=TRUE),
                        final_payment=sum(payment_amount, na.rm=TRUE),
                        final_bill=sum(bill_amount, na.rm=TRUE),
                        final_liens=sum(liens_amount, na.rm=TRUE),
                        final_leftover=sum(remaining_amount, na.rm=TRUE)) %>%
              ungroup(),
            by=c("year", "quarter", "median_status")) %>%
  mutate(final_discount=ifelse(is.na(final_discount), 0, final_discount),
         final_payment=ifelse(is.na(final_payment), 0, final_payment),
         final_bill=ifelse(is.na(final_bill), 0, final_bill),
         final_liens=ifelse(is.na(final_liens), 0, final_liens),
         final_leftover=ifelse(is.na(final_leftover), 0, final_leftover))

if (!("liens" %in% names(transaction_aggregate_summary_median))) {
  transaction_aggregate_summary_median$liens <- 0
}

transaction_aggregate_summary_median <- transaction_aggregate_summary_median %>%
  mutate(liens=ifelse(is.na(liens), 0, liens),
         liens=-(liens+final_liens)) %>% 
  left_join(collection_aggregate %>%
              mutate_all(as.numeric) %>%
              filter(!is.na(median_status)) %>%
              group_by(year, quarter, median_status) %>%
              summarise(collection_amount=sum(total_amount, na.rm=TRUE),
                        collection_paid=sum(total_payment, na.rm=TRUE)) %>%
              ungroup(),
            by=c("year", "quarter", "median_status")) %>%
  mutate(collection_amount=ifelse(is.na(collection_amount), 0, collection_amount),
         collection_paid=ifelse(is.na(collection_paid), 0, collection_paid))

transaction_aggregate_summary_median <- na.fill(transaction_aggregate_summary_median, 0) %>%
  as.data.frame()

# Non-categorised discounts are rare
if (!("discount" %in% names(transaction_aggregate_summary_median))) {
  transaction_aggregate_summary_median$discount <- 0
}

transaction_aggregate_summary_median <- transaction_aggregate_summary_median %>%
  mutate(bill=bill+fees+transfer+final_bill,
         discount=-discount-linc_discount-rct_discount,
         cleanriver_discount=-cleanriver_discount,
         final_discount=-writeoff-final_discount,
         payment=-payment-final_payment-refund) %>%
  filter(year<2025) %>%
  arrange(median_status, year, quarter)

for (j in 1:2) {
  for (i in 1:24) {
    carryover_debt <- ifelse(i==1, 
                             0, 
                             transaction_aggregate_summary_median$leftover_debt[(j-1)*24+i-1]-
                               transaction_aggregate_summary_median$final_leftover[(j-1)*24+i-1]-
                               transaction_aggregate_summary_median$liens[(j-1)*24+i-1])
    transaction_aggregate_summary_median$leftover_debt[(j-1)*24+i] <- 
      transaction_aggregate_summary_median$bill[(j-1)*24+i]+
      carryover_debt-
      transaction_aggregate_summary_median$discount[(j-1)*24+i]-
      transaction_aggregate_summary_median$cleanriver_discount[(j-1)*24+i]-
      transaction_aggregate_summary_median$final_discount[(j-1)*24+i]-
      transaction_aggregate_summary_median$payment[(j-1)*24+i]+
      transaction_aggregate_summary_median$total_outstanding[(j-1)*24+i]
  }
}

transaction_aggregate_summary_median <- transaction_aggregate_summary_median %>% 
  group_by(median_status) %>%
  mutate(payment=payment+collection_paid,
         total_delayed=total_delayed+collection_amount-collection_paid+cumsum(liens)) %>%
  ungroup() %>%
  filter(year==2024, quarter==4) %>%
  select(median_status, leftover_debt, discount, cleanriver_discount, final_discount, total_delayed)

transaction_aggregate_summary <- transaction_aggregate %>% 
  mutate(income_quartile=case_when(
    ufh_status & fa_status ~ 1,
    !ufh_status & fa_status ~ 2,
    !ufh_status & !fa_status ~ 3,
    .default=NA)) %>%
  filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
  left_join(code_info %>% select(transaction_type, category),
            by="transaction_type") %>%
  filter(!grepl("COMMIT", transaction_type)) %>%
  group_by(year, quarter, income_quartile, credit_quartile, category) %>%
  summarise(total_amount=sum(total_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  spread(category, total_amount) %>%
  left_join(payment_plan_aggregate %>%
              mutate(income_quartile=case_when(
                ufh_status & fa_status ~ 1,
                !ufh_status & fa_status ~ 2,
                !ufh_status & !fa_status ~ 3,
                .default=NA)) %>%
              mutate_all(as.numeric) %>%
              filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
              group_by(year, quarter, income_quartile, credit_quartile) %>%
              summarise(total_payment_arrange=sum(total_amount_sum, na.rm=TRUE),
                        total_outstanding=sum(total_outstanding_amount_sum, na.rm=TRUE),
                        total_payment_made=sum(difference_sum, na.rm=TRUE),
                        total_delayed=sum(remaining_amount_sum, na.rm=TRUE)) %>%
              ungroup(),
            by=c("year", "quarter", "income_quartile", "credit_quartile")) %>%
  mutate(total_payment_arrange=ifelse(is.na(total_payment_arrange), 0, total_payment_arrange),
         total_outstanding=ifelse(is.na(total_outstanding), 0, total_outstanding),
         total_payment_made=ifelse(is.na(total_payment_made), 0, total_payment_made),
         total_delayed=ifelse(is.na(total_delayed), 0, total_delayed)) %>%
  left_join(final_aggregate %>%
              mutate(income_quartile=case_when(
                ufh_status & fa_status ~ 1,
                !ufh_status & fa_status ~ 2,
                !ufh_status & !fa_status ~ 3,
                .default=NA)) %>%
              mutate_all(as.numeric) %>%
              filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
              group_by(year, quarter, income_quartile, credit_quartile) %>%
              summarise(final_discount=sum(discount_amount, na.rm=TRUE),
                        final_payment=sum(payment_amount, na.rm=TRUE),
                        final_bill=sum(bill_amount, na.rm=TRUE),
                        final_liens=sum(liens_amount, na.rm=TRUE),
                        final_leftover=sum(remaining_amount, na.rm=TRUE)) %>%
              ungroup(),
            by=c("year", "quarter", "income_quartile", "credit_quartile")) %>%
  mutate(final_discount=ifelse(is.na(final_discount), 0, final_discount),
         final_payment=ifelse(is.na(final_payment), 0, final_payment),
         final_bill=ifelse(is.na(final_bill), 0, final_bill),
         final_liens=ifelse(is.na(final_liens), 0, final_liens),
         final_leftover=ifelse(is.na(final_leftover), 0, final_leftover))

if (!("liens" %in% names(transaction_aggregate_summary))) {
  transaction_aggregate_summary$liens <- 0
}

transaction_aggregate_summary <- transaction_aggregate_summary %>%
  mutate(writeoff=ifelse(is.na(writeoff), 0, writeoff),
         liens=ifelse(is.na(liens), 0, liens),
         liens=-(liens+final_liens)) %>% 
  left_join(collection_aggregate %>%
              mutate(income_quartile=case_when(
                ufh_status & fa_status ~ 1,
                !ufh_status & fa_status ~ 2,
                !ufh_status & !fa_status ~ 3,
                .default=NA)) %>%
              mutate_all(as.numeric) %>%
              filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
              group_by(year, quarter, income_quartile, credit_quartile) %>%
              summarise(collection_amount=sum(total_amount, na.rm=TRUE),
                        collection_paid=sum(total_payment, na.rm=TRUE)) %>%
              ungroup(),
            by=c("year", "quarter", "income_quartile", "credit_quartile")) %>%
  mutate(collection_amount=ifelse(is.na(collection_amount), 0, collection_amount),
         collection_paid=ifelse(is.na(collection_paid), 0, collection_paid))

transaction_aggregate_summary <- na.fill(transaction_aggregate_summary, 0) %>%
  as.data.frame()

if (!("discount" %in% names(transaction_aggregate_summary))) {
  transaction_aggregate_summary$discount <- 0
}

transaction_aggregate_summary <- transaction_aggregate_summary %>%
  mutate(bill=bill+fees+transfer+final_bill,
         discount=-discount-linc_discount-rct_discount,
         cleanriver_discount=-cleanriver_discount,
         final_discount=-writeoff-final_discount,
         payment=-payment-final_payment-refund) %>%
  arrange(year, quarter, income_quartile, credit_quartile)

# Aggregate over income quartiles
transaction_aggregate_summary_income <- transaction_aggregate_summary %>%
  select(-credit_quartile) %>%
  group_by(income_quartile, year, quarter) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  arrange(income_quartile, year, quarter) %>%
  filter(year<2025)

for (j in 1:3) {
  for (i in 1:24) {
    carryover_debt <- ifelse(i==1, 
                             0, 
                             transaction_aggregate_summary_income$leftover_debt[(j-1)*24+i-1]-
                               transaction_aggregate_summary_income$final_leftover[(j-1)*24+i-1]-
                               transaction_aggregate_summary_income$liens[(j-1)*24+i-1])
    transaction_aggregate_summary_income$leftover_debt[(j-1)*24+i] <- 
      transaction_aggregate_summary_income$bill[(j-1)*24+i]+
      carryover_debt-
      transaction_aggregate_summary_income$discount[(j-1)*24+i]-
      transaction_aggregate_summary_income$cleanriver_discount[(j-1)*24+i]-
      transaction_aggregate_summary_income$final_discount[(j-1)*24+i]-
      transaction_aggregate_summary_income$payment[(j-1)*24+i]+
      transaction_aggregate_summary_income$total_outstanding[(j-1)*24+i]
  }
}

transaction_aggregate_summary_income <- transaction_aggregate_summary_income %>% 
  group_by(income_quartile) %>%
  mutate(payment=payment+collection_paid,
         total_delayed=total_delayed+collection_amount-collection_paid+cumsum(liens)) %>%
  ungroup() %>%
  filter(year==2024, quarter==4) %>%
  select(income_quartile, leftover_debt, discount, cleanriver_discount, final_discount, total_delayed) %>%
  mutate_at(vars(leftover_debt, discount, cleanriver_discount, final_discount, total_delayed),
            ~ifelse(.<0, 0, .))

# Aggregate over credit quartiles
transaction_aggregate_summary_credit <- transaction_aggregate_summary %>%
  select(-income_quartile) %>%
  group_by(credit_quartile, year, quarter) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  arrange(credit_quartile, year, quarter) %>%
  filter(year<2025)

for (j in 1:4) {
  for (i in 1:24) {
    carryover_debt <- ifelse(i==1, 
                             0, 
                             transaction_aggregate_summary_credit$leftover_debt[(j-1)*24+i-1]-
                               transaction_aggregate_summary_credit$final_leftover[(j-1)*24+i-1]-
                               transaction_aggregate_summary_credit$liens[(j-1)*24+i-1])
    transaction_aggregate_summary_credit$leftover_debt[(j-1)*24+i] <- 
      transaction_aggregate_summary_credit$bill[(j-1)*24+i]+
      carryover_debt-
      transaction_aggregate_summary_credit$discount[(j-1)*24+i]-
      transaction_aggregate_summary_credit$cleanriver_discount[(j-1)*24+i]-
      transaction_aggregate_summary_credit$final_discount[(j-1)*24+i]-
      transaction_aggregate_summary_credit$payment[(j-1)*24+i]+
      transaction_aggregate_summary_credit$total_outstanding[(j-1)*24+i]
  }
}

transaction_aggregate_summary_credit <- transaction_aggregate_summary_credit %>% 
  group_by(credit_quartile) %>%
  mutate(payment=payment+collection_paid,
         total_delayed=total_delayed+collection_amount-collection_paid+cumsum(liens)) %>%
  ungroup() %>%
  filter(year==2024, quarter==4) %>%
  select(credit_quartile, leftover_debt, discount, cleanriver_discount, final_discount, total_delayed) %>%
  mutate_at(vars(leftover_debt, discount, cleanriver_discount, final_discount, total_delayed),
            ~ifelse(.<0, 0, .))

# Aggregate over both quartiles
transaction_aggregate_summary <- transaction_aggregate_summary %>%
  filter(year<2025) %>%
  arrange(credit_quartile, income_quartile, year, quarter)

for (k in 1:4) {
  for (j in 1:3) {
    for (i in 1:24) {
      carryover_debt <- ifelse(i==1, 
                               0, 
                               transaction_aggregate_summary$leftover_debt[(k-1)*72+(j-1)*24+i-1]-
                                 transaction_aggregate_summary$final_leftover[(k-1)*72+(j-1)*24+i-1]-
                                 transaction_aggregate_summary$liens[(k-1)*72+(j-1)*24+i-1])
      transaction_aggregate_summary$leftover_debt[(k-1)*72+(j-1)*24+i] <-
        transaction_aggregate_summary$bill[(k-1)*72+(j-1)*24+i]+
        carryover_debt-
        transaction_aggregate_summary$discount[(k-1)*72+(j-1)*24+i]-
        transaction_aggregate_summary$cleanriver_discount[(k-1)*72+(j-1)*24+i]-
        transaction_aggregate_summary$final_discount[(k-1)*72+(j-1)*24+i]-
        transaction_aggregate_summary$payment[(k-1)*72+(j-1)*24+i]+
        transaction_aggregate_summary$total_outstanding[(k-1)*72+(j-1)*24+i]
    }
  }
}

transaction_aggregate_summary <- transaction_aggregate_summary %>%
  mutate_at(vars(leftover_debt, discount, cleanriver_discount, final_discount, total_delayed),
            ~ifelse(.<0, 0, .))

income_summary <- transaction_aggregate_summary_income %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         cleanriver_discount_percent=cleanriver_discount/sum(cleanriver_discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

income_summary <- income_summary %>%
  bind_rows(income_summary %>% summarise_all(sum) %>%
              mutate(income_quartile=0)) %>%
  mutate(leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         cleanriver_discount=cleanriver_discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000)

credit_summary <- transaction_aggregate_summary_credit %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         cleanriver_discount_percent=cleanriver_discount/sum(cleanriver_discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

credit_summary <- credit_summary %>%
  bind_rows(credit_summary %>% summarise_all(sum) %>%
              mutate(credit_quartile=0)) %>%
  mutate(leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         cleanriver_discount=cleanriver_discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000)

income_credit_summary <- transaction_aggregate_summary %>%
  group_by(income_quartile, credit_quartile) %>%
  mutate(payment=payment+collection_paid,
         total_delayed=total_delayed+collection_amount-collection_paid+cumsum(liens),
         revenue=leftover_debt+payment+discount+final_discount+total_delayed) %>%
  ungroup() %>%
  filter(year==2024, quarter==4) %>%
  select(income_quartile, credit_quartile, 
         leftover_debt, discount, cleanriver_discount, final_discount, total_delayed, revenue) %>%
  left_join(transaction_aggregate_summary_income, by="income_quartile", suffix=c("", "_income")) %>%
  left_join(transaction_aggregate_summary_credit, by="credit_quartile", suffix=c("", "_credit")) %>%
  mutate(leftover_debt_percent_income=leftover_debt/leftover_debt_income*100,
         discount_percent_income=discount/discount_income*100,
         cleanriver_discount_percent_income=cleanriver_discount/cleanriver_discount_income*100,
         final_discount_percent_income=final_discount/final_discount_income*100,
         total_delayed_percent_income=total_delayed/total_delayed_income*100,
         leftover_debt_percent_credit=leftover_debt/leftover_debt_credit*100,
         discount_percent_credit=discount/discount_credit*100,
         cleanriver_discount_percent_credit=cleanriver_discount/cleanriver_discount_credit*100,
         final_discount_percent_credit=final_discount/final_discount_credit*100,
         total_delayed_percent_credit=total_delayed/total_delayed_credit*100,
         leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         cleanriver_discount=cleanriver_discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000) %>%
  arrange(income_quartile, credit_quartile)

above_median_summary <- transaction_aggregate_summary_median %>%
  filter(median_status==0) %>%
  mutate(leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         cleanriver_discount=cleanriver_discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000,
         leftover_debt_percent=leftover_debt/(income_summary %>% 
                                                filter(income_quartile==0) %>% 
                                                pull(leftover_debt))*100,
         discount_percent=discount/(income_summary %>%
                                      filter(income_quartile==0) %>%
                                      pull(discount))*100,
         cleanriver_discount_percent=cleanriver_discount/(income_summary %>%
                                                            filter(income_quartile==0) %>%
                                                            pull(cleanriver_discount))*100,
         final_discount_percent=final_discount/(income_summary %>%
                                                  filter(income_quartile==0) %>%
                                                  pull(final_discount))*100,
         total_delayed_percent=total_delayed/(income_summary %>%
                                                filter(income_quartile==0) %>%
                                                pull(total_delayed))*100)

tab <- TexRow(c("", "Leftover Debt", "Discount", "Environment Discount", "Write-Off", "Delayed", ""),
              cspan=c(1, 2, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(2, 3), c(4, 5), c(6, 7), c(8, 9), c(10, 11))) +
  TexRow(c("Income Group", rep(c("Amount", "Share"), 5), "n")) +
  TexMidrule() +
  TexRow("UFH and Below") / TexRow(c(income_summary[1, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                     account_count_income[1, 2]) %>% as.numeric(),
                                   dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("UFH to Means-Tested Cap") / TexRow(c(income_summary[2, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                               account_count_income[2, 2]) %>% as.numeric(),
                                             dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Above Means-Tested Cap") / TexRow(c(income_summary[3, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                              account_count_income[3, 2]) %>% as.numeric(),
                                            dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexMidrule(list(c(1, 1), c(2, 3), c(4, 5), c(6, 7), c(8, 9), c(10, 11), c(12, 12))) +
  TexRow("(Above Median Income)") / TexRow(c(above_median_summary[1, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                             account_count_median[1, 2]) %>% as.numeric(),
                                           dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexMidrule() +
  TexRow("Total") / TexRow(c(income_summary[4, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             sum(account_count_income$n)) %>% as.numeric(),
                           dec=rep(0, 11), percentage=c(rep(c(F, T), 5), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/ufh_summary"),
        positions=c("l", rep("c", 11)))

total_uncollected <- round(sum(income_summary[4, 2:6])/1000, 1)
export_tex(paste0("\\$",
                  total_uncollected,
                  " million"),
           "total_uncollected")

ufh_uncollected <- round(sum(income_summary[1, 2:6])/1000, 1)
ufh_uncollected_share <- round(sum(income_summary[1, 2:6])/sum(income_summary[4, 2:6])*100, 0)
export_tex(paste0("\\$",
                  ufh_uncollected,
                  " million (",
                  ufh_uncollected_share,
                  "\\%)"),
           "ufh_uncollected")

above_means_uncollected <- round(sum(income_summary[3, 2:6])/1000, 1)
above_means_uncollected_share <- round(sum(income_summary[3, 2:6])/sum(income_summary[4, 2:6])*100, 0)
export_tex(paste0("\\$",
                  above_means_uncollected,
                  " million (",
                  above_means_uncollected_share,
                  "\\%)"),
           "above_means_uncollected")

above_median_uncollected <- round(sum(above_median_summary[1, 2:6])/1000, 1)
above_median_uncollected_share <- round(sum(above_median_summary[1, 2:6])/sum(income_summary[4, 2:6])*100, 0)
export_tex(paste0("\\$",
                  above_median_uncollected,
                  " million (",
                  above_median_uncollected_share,
                  "\\%)"),
           "above_median_uncollected")

above_median_leftover_debt <- round(as.numeric(above_median_summary[1, 2])/1000, 1)
above_median_leftover_debt_share <- round(as.numeric(above_median_summary[1, 7]), 0)
export_tex(paste0("\\$", 
                  above_median_leftover_debt,
                  " million (",
                  above_median_leftover_debt_share,
                  "\\%)"),
           "above_median_leftover_debt")

ufh_credit_summary <- income_credit_summary %>%
  filter(income_quartile==1) %>%
  select(-income_quartile, -revenue) %>%
  select(!ends_with("_income") & !ends_with("_credit")) %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         cleanriver_discount_percent=cleanriver_discount/sum(cleanriver_discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

ufh_credit_summary <- ufh_credit_summary %>%
  bind_rows(ufh_credit_summary %>% summarise_all(sum) %>%
              mutate(credit_quartile=0))

tab <- TexRow(c("UFH and Below", "Leftover Debt", "Discount", "Environment Discount", "Write-Off", "Delayed", ""),
              cspan=c(1, 2, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(1, 1), c(2, 3), c(4, 5), c(6, 7), c(8, 9), c(10, 11))) +
  TexRow(c("Credit Quartile", rep(c("Amount", "Share"), 5), "n")) +
  TexMidrule() +
  TexRow("Sub-Prime") / TexRow(c(ufh_credit_summary[1, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                 account_count[1, 3]) %>% as.numeric(),
                               dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Near-Prime") / TexRow(c(ufh_credit_summary[2, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                  account_count[2, 3]) %>% as.numeric(),
                                dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Prime") / TexRow(c(ufh_credit_summary[3, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             account_count[3, 3]) %>% as.numeric(),
                           dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Super-Prime") / TexRow(c(ufh_credit_summary[4, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                   account_count[4, 3]) %>% as.numeric(),
                                 dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexMidrule() +
  TexRow("Total") / TexRow(c(ufh_credit_summary[5, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             account_count_income[1, 2]) %>% as.numeric(),
                           dec=rep(0, 11), percentage=c(rep(c(F, T), 5), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/ufh_credit_summary"),
        positions=c("l", rep("c", 11)))

ufh_low_credit_share <- round(as.numeric(account_count[1, 3])/
                                as.numeric(account_count_income[1, 2])*100, 0)
export_tex(paste0(ufh_low_credit_share, "\\%"),
           "ufh_low_credit_share")

ufh_low_credit_unpaid_debt_share <- round(as.numeric(ufh_credit_summary[1, 7]), 0)
export_tex(paste0(ufh_low_credit_unpaid_debt_share, "\\%"),
           "ufh_low_credit_unpaid_debt_share")

below_credit_summary <- income_credit_summary %>%
  filter(income_quartile==2) %>%
  select(-income_quartile, -revenue) %>%
  select(!ends_with("_income") & !ends_with("_credit")) %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         cleanriver_discount_percent=cleanriver_discount/sum(cleanriver_discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

below_credit_summary <- below_credit_summary %>%
  bind_rows(below_credit_summary %>% summarise_all(sum) %>%
              mutate(credit_quartile=0))

tab <- TexRow(c("UFH to Means-Tested Cap", "Leftover Debt", "Discount", "Environment Discount", "Write-Off", "Delayed", ""),
              cspan=c(1, 2, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(1, 1), c(2, 3), c(4, 5), c(6, 7), c(8, 9), c(10, 11))) +
  TexRow(c("Credit Quartile", rep(c("Amount", "Share"), 5), "n")) +
  TexMidrule() +
  TexRow("Sub-Prime") / TexRow(c(below_credit_summary[1, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                 account_count[5, 3]) %>% as.numeric(),
                               dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Near-Prime") / TexRow(c(below_credit_summary[2, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                  account_count[6, 3]) %>% as.numeric(),
                                dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Prime") / TexRow(c(below_credit_summary[3, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             account_count[7, 3]) %>% as.numeric(),
                           dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Super-Prime") / TexRow(c(below_credit_summary[4, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                   account_count[8, 3]) %>% as.numeric(),
                                 dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexMidrule() +
  TexRow("Total") / TexRow(c(below_credit_summary[5, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             account_count_income[2, 2]) %>% as.numeric(),
                           dec=rep(0, 11), percentage=c(rep(c(F, T), 5), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/below_credit_summary"),
        positions=c("l", rep("c", 11)))

below_low_credit_share <- round(as.numeric(account_count[5, 3])/
                                  as.numeric(account_count_income[2, 2])*100, 0)
export_tex(paste0(below_low_credit_share, "\\%"),
           "below_low_credit_share")

below_low_credit_unpaid_debt_share <- round(as.numeric(below_credit_summary[1, 7]), 0)
export_tex(paste0(below_low_credit_unpaid_debt_share, "\\%"),
           "below_low_credit_unpaid_debt_share")

above_credit_summary <- income_credit_summary %>%
  filter(income_quartile==3) %>%
  select(-income_quartile, -revenue) %>%
  select(!ends_with("_income") & !ends_with("_credit")) %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         cleanriver_discount_percent=cleanriver_discount/sum(cleanriver_discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

above_credit_summary <- above_credit_summary %>%
  bind_rows(above_credit_summary %>% summarise_all(sum) %>%
              mutate(credit_quartile=0))

tab <- TexRow(c("Above Means-Tested Cap", "Leftover Debt", "Discount", "Environment Discount", "Write-Off", "Delayed", ""),
              cspan=c(1, 2, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(1, 1), c(2, 3), c(4, 5), c(6, 7), c(8, 9), c(10, 11))) +
  TexRow(c("Credit Quartile", rep(c("Amount", "Share"), 5), "n")) +
  TexMidrule() +
  TexRow("Sub-Prime") / TexRow(c(above_credit_summary[1, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                 account_count[9, 3]) %>% as.numeric(),
                               dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Near-Prime") / TexRow(c(above_credit_summary[2, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                  account_count[10, 3]) %>% as.numeric(),
                                dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Prime") / TexRow(c(above_credit_summary[3, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             account_count[11, 3]) %>% as.numeric(),
                           dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexRow("Super-Prime") / TexRow(c(above_credit_summary[4, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                                   account_count[12, 3]) %>% as.numeric(),
                                 dec=c(rep(c(0, 1), 5), 0), percentage=c(rep(c(F, T), 5), F)) +
  TexMidrule() +
  TexRow("Total") / TexRow(c(above_credit_summary[5, c(2, 7, 3, 8, 4, 9, 5, 10, 6, 11)],
                             account_count_income[3, 2]) %>% as.numeric(),
                           dec=rep(0, 11), percentage=c(rep(c(F, T), 5), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/above_credit_summary"),
        positions=c("l", rep("c", 11)))
