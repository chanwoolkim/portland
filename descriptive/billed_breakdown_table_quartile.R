# Debt decomposition ####
transaction_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/2024q4/transaction_aggregate.csv"))
payment_plan_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/2024q4/payment_plan_aggregate.csv"))
collection_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/2024q4/collection_aggregate.csv"))
final_aggregate <- read_csv(paste0(working_data_dir, "/servus_query/2024q4/final_aggregate.csv"))
account_count <- read_csv(paste0(working_data_dir, "/servus_query/2024q4/account_count.csv"))

account_count <- account_count %>% 
  filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
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

transaction_aggregate_summary <- transaction_aggregate %>% 
  filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
  left_join(code_info %>% select(transaction_type, category),
            by="transaction_type") %>%
  filter(!grepl("COMMIT", transaction_type)) %>%
  group_by(year, quarter, income_quartile, credit_quartile, category) %>%
  summarise(total_amount=sum(total_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  spread(category, total_amount) %>%
  left_join(payment_plan_aggregate %>%
              filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
              mutate_all(as.numeric) %>%
              select(year, quarter, income_quartile, credit_quartile,
                     total_payment_arrange=total_amount_sum,
                     total_outstanding=total_outstanding_amount_sum,
                     total_payment_made=difference_sum,
                     total_delayed=remaining_amount_sum),
            by=c("year", "quarter", "income_quartile", "credit_quartile")) %>%
  left_join(final_aggregate %>%
              filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
              mutate_all(as.numeric) %>%
              select(year, quarter, income_quartile, credit_quartile,
                     final_discount=discount_amount,
                     final_payment=payment_amount,
                     final_bill=bill_amount,
                     final_liens=liens_amount,
                     final_leftover=remaining_amount),
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
              filter(!is.na(income_quartile), !is.na(credit_quartile)) %>%
              mutate_all(as.numeric) %>%
              rename(collection_amount=total_amount,
                     collection_paid=total_payment),
            by=c("year", "quarter", "income_quartile", "credit_quartile")) %>%
  mutate(collection_amount=ifelse(is.na(collection_amount), 0, collection_amount),
         collection_paid=ifelse(is.na(collection_paid), 0, collection_paid),
         bill=bill+final_bill,
         discount=-discount,
         final_discount=-writeoff-final_discount,
         payment=-(payment+final_payment)) %>%
  arrange(year, quarter, income_quartile, credit_quartile)

# Aggregate over income quartiles
transaction_aggregate_summary_income <- transaction_aggregate_summary %>%
  select(-credit_quartile) %>%
  group_by(income_quartile, year, quarter) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  arrange(income_quartile, year, quarter) %>%
  filter(year<2025)

for (j in 1:4) {
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
  filter(year==2024, quarter==2) %>%
  select(income_quartile, leftover_debt, discount, final_discount, total_delayed)

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
  filter(year==2024, quarter==2) %>%
  select(credit_quartile, leftover_debt, discount, final_discount, total_delayed)

# Aggregate over both quartiles
transaction_aggregate_summary <- transaction_aggregate_summary %>%
  filter(year<2025) %>%
  arrange(credit_quartile, income_quartile, year, quarter)

for (k in 1:4) {
  for (j in 1:4) {
    for (i in 1:24) {
      carryover_debt <- ifelse(i==1, 
                               0, 
                               transaction_aggregate_summary$leftover_debt[(k-1)*96+(j-1)*24+i-1]-
                                 transaction_aggregate_summary$final_leftover[(k-1)*96+(j-1)*24+i-1]-
                                 transaction_aggregate_summary$liens[(k-1)*96+(j-1)*24+i-1])
      transaction_aggregate_summary$leftover_debt[(k-1)*96+(j-1)*24+i] <-
        transaction_aggregate_summary$bill[(k-1)*96+(j-1)*24+i]+
        carryover_debt-
        transaction_aggregate_summary$discount[(k-1)*96+(j-1)*24+i]-
        transaction_aggregate_summary$final_discount[(k-1)*96+(j-1)*24+i]-
        transaction_aggregate_summary$payment[(k-1)*96+(j-1)*24+i]+
        transaction_aggregate_summary$total_outstanding[(k-1)*96+(j-1)*24+i]
    }
  }
}

income_summary <- transaction_aggregate_summary_income %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

income_summary <- income_summary %>%
  bind_rows(income_summary %>% summarise_all(sum) %>%
              mutate(income_quartile=0)) %>%
  mutate(leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000)

credit_summary <- transaction_aggregate_summary_credit %>%
  mutate(leftover_debt_percent=leftover_debt/sum(leftover_debt)*100,
         discount_percent=discount/sum(discount)*100,
         final_discount_percent=final_discount/sum(final_discount)*100,
         total_delayed_percent=total_delayed/sum(total_delayed)*100)

credit_summary <- credit_summary %>%
  bind_rows(credit_summary %>% summarise_all(sum) %>%
              mutate(credit_quartile=0)) %>%
  mutate(leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000)

income_credit_summary <- transaction_aggregate_summary %>%
  group_by(income_quartile, credit_quartile) %>%
  mutate(payment=payment+collection_paid,
         total_delayed=total_delayed+collection_amount-collection_paid+cumsum(liens),
         revenue=leftover_debt+payment+discount+final_discount+total_delayed) %>%
  ungroup() %>%
  filter(year==2024, quarter==2) %>%
  select(income_quartile, credit_quartile, 
         leftover_debt, discount, final_discount, total_delayed, revenue) %>%
  left_join(transaction_aggregate_summary_income, by="income_quartile", suffix=c("", "_income")) %>%
  left_join(transaction_aggregate_summary_credit, by="credit_quartile", suffix=c("", "_credit")) %>%
  mutate(leftover_debt_percent_income=leftover_debt/leftover_debt_income*100,
         discount_percent_income=discount/discount_income*100,
         final_discount_percent_income=final_discount/final_discount_income*100,
         total_delayed_percent_income=total_delayed/total_delayed_income*100,
         leftover_debt_percent_credit=leftover_debt/leftover_debt_credit*100,
         discount_percent_credit=discount/discount_credit*100,
         final_discount_percent_credit=final_discount/final_discount_credit*100,
         total_delayed_percent_credit=total_delayed/total_delayed_credit*100,
         leftover_debt=leftover_debt/1000,
         discount=discount/1000,
         final_discount=final_discount/1000,
         total_delayed=total_delayed/1000) %>%
  arrange(income_quartile, credit_quartile)

tab <- TexRow(c("", "Leftover Debt", "Discount", "Write-Off", "Delayed", ""),
              cspan=c(1, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(2, 3), c(4, 5), c(6, 7), c(8, 9))) +
  TexRow(c("Income Quartile", rep(c("Amount", "Share"), 4), "n")) +
  TexMidrule() +
  TexRow(c(income_summary[1, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_income[1, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow(c(income_summary[2, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_income[2, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow(c(income_summary[3, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_income[3, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow(c(income_summary[4, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_income[4, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexMidrule() +
  TexRow("Total") / TexRow(c(income_summary[5, c(2, 6, 3, 7, 4, 8, 5, 9)],
                             sum(account_count_income$n)) %>% as.numeric(),
                           dec=rep(0, 9), percentage=c(rep(c(F, T), 4), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/income_summary"),
        positions=rep("c", 10))

tab <- TexRow(c("", "Leftover Debt", "Discount", "Write-Off", "Delayed", ""),
              cspan=c(1, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(2, 3), c(4, 5), c(6, 7), c(8, 9))) +
  TexRow(c("Credit Quartile", rep(c("Amount", "Share"), 4), "n")) +
  TexMidrule() +
  TexRow(c(credit_summary[1, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_credit[1, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow(c(credit_summary[2, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_credit[2, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow(c(credit_summary[3, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_credit[3, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow(c(credit_summary[4, c(1, 2, 6, 3, 7, 4, 8, 5, 9)],
           account_count_credit[4, 2]) %>% as.numeric(),
         dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexMidrule() +
  TexRow("Total") / TexRow(c(credit_summary[5, c(2, 6, 3, 7, 4, 8, 5, 9)],
                             sum(account_count_credit$n)) %>% as.numeric(),
                           dec=rep(0, 9), percentage=c(rep(c(F, T), 4), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/credit_summary"),
        positions=rep("c", 10))

tab <- TexRow(c("Quartile", "Leftover Debt", "Discount", "Write-Off", "Delayed", ""),
              cspan=c(2, 2, 2, 2, 2, 1)) +
  TexMidrule(list(c(1, 2), c(3, 4), c(5, 6), c(7, 8), c(9, 10))) +
  TexRow(c("Income", "Credit", rep(c("Amount", "Share"), 4), "n")) +
  TexMidrule() +
  TexRow(c(income_credit_summary[1, c(1:2, 3, 16, 4, 17, 5, 18, 6, 19)],
           account_count[1, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(c(0, 1), 4), 0), percentage=c(F, F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[2, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[2, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[3, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[3, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[4, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[4, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexMidrule(list(c(2, 11))) +
  TexRow(c("", "Total")) / TexRow(c(income_summary[1, c(2, 6, 3, 7, 4, 8, 5, 9)],
                                    account_count_income[1, 2]) %>% as.numeric(),
                                  dec=c(rep(c(0, 1), 4), 0), percentage=c(rep(c(F, T), 4), F)) +
  TexMidrule() +
  TexRow(c(income_credit_summary[5, c(1:2, 3, 16, 4, 17, 5, 18, 6, 19)],
           account_count[5, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(c(0, 1), 4), 0), percentage=c(F, F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[6, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[6, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[7, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[7, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[8, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[8, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexMidrule(list(c(2, 11))) +
  TexRow(c("", "Total")) / TexRow(c(income_summary[2, c(2, 6, 3, 7, 4, 8, 5, 9)],
                                    account_count_income[2, 2]) %>% as.numeric(),
                                  dec=c(rep(c(0, 1), 4), 0), percentage=c(rep(c(F, T), 4), F)) +
  TexMidrule() +
  TexRow(c(income_credit_summary[9, c(1:2, 3, 16, 4, 17, 5, 18, 6, 19)],
           account_count[9, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(c(0, 1), 4), 0), percentage=c(F, F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[10, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[10, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[11, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[11, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[12, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[12, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexMidrule(list(c(2, 11))) +
  TexRow(c("", "Total")) / TexRow(c(income_summary[3, c(2, 6, 3, 7, 4, 8, 5, 9)],
                                    account_count_income[3, 2]) %>% as.numeric(),
                                  dec=c(rep(c(0, 1), 4), 0), percentage=c(rep(c(F, T), 4), F)) +
  TexMidrule() +
  TexRow(c(income_credit_summary[13, c(1:2, 3, 16, 4, 17, 5, 18, 6, 19)],
           account_count[13, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(c(0, 1), 4), 0), percentage=c(F, F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[14, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[14, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[15, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[15, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexRow("") / TexRow(c(income_credit_summary[16, c(2, 3, 16, 4, 17, 5, 18, 6, 19)],
                        account_count[16, 3]) %>% as.numeric(),
                      dec=c(0, rep(c(0, 1), 4), 0), percentage=c(F, rep(c(F, T), 4), F)) +
  TexMidrule(list(c(2, 11))) +
  TexRow(c("", "Total")) / TexRow(c(income_summary[4, c(2, 6, 3, 7, 4, 8, 5, 9)],
                                    account_count_income[4, 2]) %>% as.numeric(),
                                  dec=c(rep(c(0, 1), 4), 0), percentage=c(rep(c(F, T), 4), F)) +
  TexMidrule() +
  TexRow(c("Total", "")) / TexRow(c(income_summary[5, c(2, 6, 3, 7, 4, 8, 5, 9)],
                                    sum(account_count$n)) %>% as.numeric(),
                                  dec=rep(0, 9), percentage=c(rep(c(F, T), 4), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/income_credit_summary"),
        positions=rep("c", 11))

# Do it by average per household
income_credit_summary <- income_credit_summary %>%
  left_join(account_count, 
            by=c("income_quartile", "credit_quartile")) %>%
  mutate(revenue=revenue/n,
         leftover_debt=leftover_debt/n*1000,
         discount=discount/n*1000,
         final_discount=final_discount/n*1000,
         total_delayed=total_delayed/n*1000)

income_credit_summary_average <- income_credit_summary %>%
  group_by(income_quartile) %>%
  summarise(revenue=weighted.mean(revenue, n),
            leftover_debt=weighted.mean(leftover_debt, n),
            discount=weighted.mean(discount, n),
            final_discount=weighted.mean(final_discount, n),
            total_delayed=weighted.mean(total_delayed, n)) %>%
  ungroup()

tab <- TexRow(c("Income", "Credit", 
                "Bill", "Leftover Debt", "Discount", "Write-Off", "Delayed", "n")) +
  TexMidrule() +
  TexRow(c(income_credit_summary[1, c(1:2, 7, 3:6)], 
           account_count[1, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(1, 5), 0), dollar=c(F, F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[2, c(2, 7, 3:6)], 
                        account_count[2, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[3, c(2, 7, 3:6)], 
                        account_count[3, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[4, c(2, 7, 3:6)], 
                        account_count[4, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexMidrule(list(c(2, 8))) +
  TexRow(c("", "Average")) / TexRow(c(income_credit_summary_average[1, 2:6],
                                      sum(account_count$n[1:4])) %>% as.numeric(),
                                    dec=c(rep(1, 5), 0), dollar=c(rep(T, 5), F)) +
  TexMidrule() +
  TexRow(c(income_credit_summary[5, c(1:2, 7, 3:6)], 
           account_count[5, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(1, 5), 0), dollar=c(F, F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[6, c(2, 7, 3:6)], 
                        account_count[6, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[7, c(2, 7, 3:6)], 
                        account_count[7, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[8, c(2, 7, 3:6)], 
                        account_count[8, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexMidrule(list(c(2, 8))) +
  TexRow(c("", "Average")) / TexRow(c(income_credit_summary_average[2, 2:6],
                                      sum(account_count$n[5:8])) %>% as.numeric(),
                                    dec=c(rep(1, 5), 0), dollar=c(rep(T, 5), F)) +
  TexMidrule() +
  TexRow(c(income_credit_summary[9, c(1:2, 7, 3:6)], 
           account_count[9, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(1, 5), 0), dollar=c(F, F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[10, c(2, 7, 3:6)], 
                        account_count[10, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[11, c(2, 7, 3:6)], 
                        account_count[11, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[12, c(2, 7, 3:6)], 
                        account_count[12, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexMidrule(list(c(2, 8))) +
  TexRow(c("", "Average")) / TexRow(c(income_credit_summary_average[3, 2:6],
                                      sum(account_count$n[9:12])) %>% as.numeric(),
                                    dec=c(rep(1, 5), 0), dollar=c(rep(T, 5), F)) +
  TexMidrule() +
  TexRow(c(income_credit_summary[13, c(1:2, 7, 3:6)], 
           account_count[13, 3]) %>% as.numeric(),
         dec=c(0, 0, rep(1, 5), 0), dollar=c(F, F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[14, c(2, 7, 3:6)], 
                        account_count[14, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[15, c(2, 7, 3:6)], 
                        account_count[15, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexRow("") / TexRow(c(income_credit_summary[16, c(2, 7, 3:6)], 
                        account_count[16, 3]) %>% as.numeric(),
                      dec=c(0, rep(1, 5), 0), dollar=c(F, rep(T, 5), F)) +
  TexMidrule(list(c(2, 8))) +
  TexRow(c("", "Average")) / TexRow(c(income_credit_summary_average[4, 2:6],
                                      sum(account_count$n[13:16])) %>% as.numeric(),
                                    dec=c(rep(1, 5), 0), dollar=c(rep(T, 5), F)) +
  TexMidrule() +
  TexRow(c("Average", "")) / TexRow(c(weighted.mean(income_credit_summary$revenue,
                                                    income_credit_summary$n),
                                      weighted.mean(income_credit_summary$leftover_debt,
                                                    income_credit_summary$n),
                                      weighted.mean(income_credit_summary$discount,
                                                    income_credit_summary$n),
                                      weighted.mean(income_credit_summary$final_discount,
                                                    income_credit_summary$n),
                                      weighted.mean(income_credit_summary$total_delayed,
                                                    income_credit_summary$n),
                                      sum(account_count$n)) %>% as.numeric(),
                                    dec=c(rep(1, 5), 0), dollar=c(rep(T, 5), F))

TexSave(tab,
        filename=paste0(output_dir, "/tables/income_credit_summary_average"),
        positions=rep("c", 8))
