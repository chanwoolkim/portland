hazard_bill <- read_csv(paste0(working_data_dir, "/servus_query/hazard_rate_bill_dataset.csv"))
hazard_transaction <- read_csv(paste0(working_data_dir, "/servus_query/hazard_rate_estimation_dataset.csv"))
payment_plan <- read_csv(paste0(working_data_dir, "/servus_query/payment_plan.csv"))
income_credit <- read_csv(paste0(working_data_dir, "/servus_query/income_credit.csv"))

hazard_bill <- hazard_bill %>%
  mutate(initial_amount=round(as.numeric(initial_amount), 2)) %>%
  arrange(account_number, bill_date) %>%
  group_by(account_number) %>%
  slice(1) %>%
  ungroup()

hazard_transaction <- hazard_transaction %>%
  mutate(transaction_amount=round(as.numeric(amount), 2))

hazard_payment <- hazard_transaction %>%
  filter(transaction_category %in% c("payment", "fees", 
                                     "discount", "adjustment",
                                     "refund", "writeoff", "liens")) %>%
  group_by(account_number, transaction_date) %>%
  summarise(transaction_amount=sum(transaction_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(account_number, transaction_date, transaction_amount) %>%
  group_by(account_number) %>%
  mutate(cumulative_paid=cumsum(transaction_amount),
         cumulative_paid=round(cumulative_paid, 2)) %>%
  ungroup() %>%
  left_join(hazard_bill %>% 
              select(account_number, initial_amount, bill_date),
            by="account_number")

hazard_payment <- hazard_payment %>%
  filter(initial_amount>0) %>%
  mutate(initial_unpaid=initial_amount+cumulative_paid,
         initial_unpaid=round(initial_unpaid, 2)) %>%
  group_by(account_number) %>%
  summarise(bill_date=first(bill_date),
            first_payment_date=min(transaction_date[initial_unpaid<=0])) %>%
  ungroup()

hazard_payment <- hazard_payment %>%
  mutate(first_payment_date=if_else(is.infinite(as.numeric(first_payment_date)), 
                                    as.Date("2025-04-01"), 
                                    first_payment_date),
         first_payment_weeks=as.numeric(difftime(first_payment_date, 
                                                 bill_date, 
                                                 units="weeks"))) %>%
  filter(first_payment_weeks>3) %>%
  mutate(first_payment_weeks=round(first_payment_weeks-3, 0))

hazard_payment_summary <- hazard_payment %>%
  group_by(first_payment_weeks) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(first_payment_weeks) %>%
  mutate(cumulative_n=cumsum(n),
         cumulative_proportion=cumulative_n/nrow(hazard_payment)) %>%
  filter(first_payment_weeks>0) %>%
  bind_rows(data.frame(first_payment_weeks=0, 
                       n=0, 
                       cumulative_n=0, 
                       cumulative_proportion=0)) %>%
  filter(first_payment_weeks<=90)

gg <- ggplot(hazard_payment_summary, 
             aes(x=first_payment_weeks, y=cumulative_proportion)) +
  geom_line() +
  geom_point() +
  labs(title="Hazard Rate of Paying Off Q1 2023 Bill",
       x="Weeks Unpaid After Due Date",
       y="Cumulative Proportion of Accounts") +
  fte_theme() +
  scale_x_continuous(breaks=seq(0, 90, 5),
                     minor_breaks=seq(0, 90, 1)) +
  scale_y_continuous(limits=c(0, 1),
                     breaks=seq(0, 1, 0.1)) +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  geom_vline(xintercept=10.5, 
             colour="darkred") +
  annotate("label", x=6, y=0.9, label="Next Bill Issued",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=13.5, 
             colour="darkred") +
  annotate("label", x=17, y=0.8, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=26.5, 
             colour="darkred") +
  annotate("label", x=26.5, y=0.3, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=39.5, 
             colour="darkred") +
  annotate("label", x=39.5, y=0.3, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=52.5, 
             colour="darkred") +
  annotate("label", x=52.5, y=0.3, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=65.5, 
             colour="darkred") +
  annotate("label", x=65.5, y=0.3, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=78.5, 
             colour="darkred") +
  annotate("label", x=78.5, y=0.3, label="Next Bill Due",
             colour="darkred", size=5, family="serif")
gg
ggsave(gg,
       file=paste0(output_dir, "/figures/hazard_rate_bill.png"),
       width=12, height=8)

hazard_delinquency <- hazard_transaction %>%
  group_by(account_number, transaction_date) %>%
  summarise(transaction_amount=sum(transaction_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(hazard_bill %>% 
              select(account_number, initial_amount, bill_date),
            by="account_number") %>%
  arrange(account_number, transaction_date, transaction_amount) %>%
  group_by(account_number) %>%
  mutate(row_number=row_number(),
         transaction_amount=if_else(row_number==1, 
                                    transaction_amount+initial_amount,
                                    transaction_amount)) %>%
  mutate(cumulative_unpaid=cumsum(transaction_amount),
         cumulative_unpaid=round(cumulative_unpaid, 2)) %>%
  ungroup()

hazard_delinquency <- hazard_delinquency %>%
  filter(initial_amount>0) %>%
  group_by(account_number) %>%
  summarise(bill_date=first(bill_date),
            first_payment_date=min(transaction_date[cumulative_unpaid<=0])) %>%
  ungroup()

hazard_delinquency <- hazard_delinquency %>%
  mutate(first_payment_date=if_else(is.infinite(as.numeric(first_payment_date)), 
                                    as.Date("2025-04-01"), 
                                    first_payment_date),
         first_payment_weeks=as.numeric(difftime(first_payment_date, 
                                                 bill_date, 
                                                 units="weeks"))) %>%
  filter(first_payment_weeks>3) %>%
  mutate(first_payment_weeks=round(first_payment_weeks-3, 0))

hazard_delinquency_summary <- hazard_delinquency %>%
  group_by(first_payment_weeks) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(first_payment_weeks) %>%
  mutate(cumulative_n=cumsum(n),
         cumulative_proportion=cumulative_n/nrow(hazard_delinquency)) %>%
  filter(first_payment_weeks>0) %>%
  bind_rows(data.frame(first_payment_weeks=0, 
                       n=0, 
                       cumulative_n=0, 
                       cumulative_proportion=0)) %>%
  filter(first_payment_weeks<=90)

gg <- ggplot(hazard_delinquency_summary, 
             aes(x=first_payment_weeks, y=cumulative_proportion)) +
  geom_line() +
  geom_point() +
  labs(title="Hazard Rate of Exiting Delinquency Status",
       x="Weeks Unpaid After Due Date",
       y="Cumulative Proportion of Accounts") +
  fte_theme() +
  scale_x_continuous(breaks=seq(0, 90, 5),
                     minor_breaks=seq(0, 90, 1)) +
  scale_y_continuous(limits=c(0, 0.8),
                     breaks=seq(0, 0.8, 0.1)) +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  geom_vline(xintercept=10.5, 
             colour="darkred") +
  annotate("label", x=6, y=0.8, label="Next Bill Issued",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=13.5, 
             colour="darkred") +
  annotate("label", x=17, y=0.7, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=26.5, 
             colour="darkred") +
  annotate("label", x=26.5, y=0.2, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=39.5, 
             colour="darkred") +
  annotate("label", x=39.5, y=0.2, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=52.5, 
             colour="darkred") +
  annotate("label", x=52.5, y=0.2, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=65.5, 
             colour="darkred") +
  annotate("label", x=65.5, y=0.2, label="Next Bill Due",
             colour="darkred", size=5, family="serif") +
  geom_vline(xintercept=78.5, 
             colour="darkred") +
  annotate("label", x=78.5, y=0.2, label="Next Bill Due",
             colour="darkred", size=5, family="serif")
gg
ggsave(gg,
       file=paste0(output_dir, "/figures/hazard_rate_delinquency.png"),
       width=12, height=8)

# How are they doing after delinquency?
payment_plan_delinquency <- payment_plan %>%
  select(-initial_amount) %>%
  inner_join(hazard_bill %>% 
               select(account_number, initial_amount, bill_date),
             by="account_number") %>%
  filter(start_date>=bill_date) %>%
  left_join(hazard_delinquency %>% 
              select(account_number, first_payment_date),
            by="account_number") %>%
  mutate(end_date=if_else(is.na(end_date), 
                          as.Date("2025-04-01"), 
                          end_date)) %>%
  filter(end_date<=first_payment_date) %>%
  mutate(exit_delinquency_duration=difftime(first_payment_date, bill_date, units="weeks") %>% as.numeric(),
         payment_plan_duration=difftime(end_date, start_date, units="weeks") %>% as.numeric()) %>%
  group_by(account_number) %>%
  summarise(exit_delinquency_duration=first(exit_delinquency_duration),
            initial_amount=first(initial_amount),
            payment_plan_duration=sum(payment_plan_duration, na.rm=TRUE),
            n_plans=n()) %>%
  ungroup() %>%
  mutate(share_on_plan=payment_plan_duration/exit_delinquency_duration)
         

# Present discounted payments ####
hazard_off_delinquency <- hazard_transaction %>%
  filter(transaction_category %in% c("payment", "fees", 
                                     "discount", "adjustment",
                                     "refund", "writeoff", "liens")) %>%
  select(account_number, transaction_date, transaction_amount) %>%
  arrange(account_number, transaction_date, transaction_amount) %>%
  group_by(account_number) %>%
  mutate(cumulative_paid=cumsum(transaction_amount)) %>%
  ungroup() %>%
  left_join(hazard_bill %>% 
              select(account_number, initial_amount, bill_date),
            by="account_number")

hazard_off_delinquency <- hazard_off_delinquency %>%
  filter(initial_amount>0) %>%
  mutate(initial_unpaid=initial_amount+cumulative_paid) %>%
  group_by(account_number) %>%
  summarise(bill_date=first(bill_date),
            first_payment_date=min(transaction_date[initial_unpaid<=0])) %>%
  ungroup()

hazard_off_delinquency <- hazard_off_delinquency %>%
  mutate(first_payment_date=if_else(is.infinite(as.numeric(first_payment_date)), 
                                    as.Date("2025-04-01"), 
                                    first_payment_date))

hazard_off_delinquency <- hazard_off_delinquency %>%
  select(account_number, first_payment_date) %>%
  right_join(hazard_transaction, by="account_number") %>%
  filter(transaction_category %in% c("payment", "fees", 
                                     "discount", "adjustment",
                                     "refund", "writeoff", "liens"),
         transaction_date<=first_payment_date)

payment_plan <- payment_plan %>%
  filter(start_date<as.Date("2025-04-01")) %>%
  mutate(end_date=if_else(is.na(end_date), 
                          as.Date("2025-04-01"), 
                          end_date))

hazard_off_delinquency <- hazard_off_delinquency %>%
  mutate(transaction_id=row_number())

hazard_off_delinquency_payment_plan <- hazard_off_delinquency %>%
  left_join(payment_plan, by="account_number") %>%
  filter(transaction_date>=start_date,
         transaction_date<=end_date) %>%
  mutate(in_payment_plan=TRUE)

hazard_off_delinquency <- hazard_off_delinquency %>%
  filter(!transaction_id %in% hazard_off_delinquency_payment_plan$transaction_id) %>%
  mutate(in_payment_plan=FALSE) %>%
  bind_rows(hazard_off_delinquency_payment_plan %>%
              select(-payment_plan_id, -status_code, 
                     -start_date, -end_date, 
                     -total_amount, -initial_amount, -updated)) %>%
  select(-transaction_id) %>%
  arrange(account_number, transaction_date, transaction_amount)

hazard_off_delinquency <- hazard_off_delinquency %>%
  left_join(hazard_bill %>% 
              select(account_number, initial_amount, bill_date),
            by="account_number")

hazard_off_delinquency <- hazard_off_delinquency %>%
  filter(initial_amount>0) %>%
  arrange(account_number, transaction_date, transaction_amount) %>%
  group_by(account_number) %>%
  mutate(cumulative_paid=cumsum(transaction_amount)) %>%
  ungroup() %>%
  mutate(initial_unpaid=initial_amount+cumulative_paid,
         transaction_amount=ifelse(initial_unpaid<0, -lag(initial_unpaid), transaction_amount)) %>%
  select(-cumulative_paid, -initial_unpaid)

hazard_initial_amount <- hazard_off_delinquency %>%
  select(account_number, initial_amount) %>%
  distinct() %>%
  summarise(initial_amount=sum(initial_amount, na.rm=TRUE)) %>%
  ungroup()

hazard_payment_summary <- hazard_off_delinquency %>%  
  mutate(transaction_category=ifelse(transaction_category=="refund", 
                                     "payment", 
                                     transaction_category)) %>%
  mutate(year=year(transaction_date),
         quarter=quarter(transaction_date)) %>%
  group_by(year, quarter, transaction_category, in_payment_plan) %>%
  summarise(total_amount=sum(transaction_amount, na.rm=TRUE)) %>%
  ungroup()

hazard_payment_summary <- hazard_payment_summary %>%
  mutate(transaction_category=
           ifelse(transaction_category=="payment" & in_payment_plan, 
                  "payment_plan", 
                  transaction_category)) %>%
  group_by(year, quarter, transaction_category) %>%
  summarise(total_amount=sum(total_amount, na.rm=TRUE)) %>%
  ungroup()

discount_rate <- data.frame(year=rep(2023:2025, each=4),
                            quarter=rep(1:4, 3)) %>%
  mutate(discount_rate=((1+0.05)^(1/4))^(row_number()-1))

hazard_payment_summary <- hazard_payment_summary %>%
  ungroup() %>%
  select(year, quarter, transaction_category, total_amount) %>%
  spread(transaction_category, total_amount) %>%
  cbind(hazard_initial_amount) %>%
  na.fill(fill=0) %>%
  as.data.frame()

hazard_payment_summary$unpaid_amount[1] <- 
  hazard_payment_summary$initial_amount[1]+
  hazard_payment_summary$fees[1]+
  hazard_payment_summary$discount[1]-
  hazard_payment_summary$liens[1]+
  hazard_payment_summary$payment[1]+
  hazard_payment_summary$payment_plan[1]+
  hazard_payment_summary$adjustment[1]+
  hazard_payment_summary$writeoff[1]

for (i in 2:nrow(hazard_payment_summary)) {
  hazard_payment_summary$unpaid_amount[i] <- 
    hazard_payment_summary$unpaid_amount[i-1] +
    hazard_payment_summary$fees[i]+
    hazard_payment_summary$discount[i]-
    hazard_payment_summary$liens[i]+
    hazard_payment_summary$payment[i]+
    hazard_payment_summary$payment_plan[i]+
    hazard_payment_summary$adjustment[i]+
    hazard_payment_summary$writeoff[i]
}

hazard_payment_summary <- hazard_payment_summary %>%
  left_join(discount_rate, by=c("year", "quarter")) %>%
  mutate_at(c("payment", "payment_plan", 
              "adjustment", "fees", "discount", 
              "writeoff", "liens", "unpaid_amount"), 
            ~./discount_rate)

hazard_payment_summary <- hazard_payment_summary %>%
  mutate_at(c("payment", "payment_plan", 
              "adjustment", "fees", "discount", 
              "writeoff", "liens"), 
            ~cumsum(.))

hazard_payment_summary <- hazard_payment_summary %>%
  mutate(total_amount=initial_amount+fees,
         loss_discount=total_amount+discount-liens+payment+payment_plan+
           adjustment+writeoff-unpaid_amount,
         unpaid_amount=unpaid_amount-fees,
         payment=-payment/1000,
         payment_plan=-payment_plan/1000,
         adjustment=-adjustment/1000,
         fees=fees/1000,
         discount=-discount/1000,
         writeoff=-writeoff/1000,
         liens=liens/1000,
         unpaid_amount=unpaid_amount/1000,
         loss_discount=loss_discount/1000,
         initial_amount=initial_amount/1000,
         total_amount=total_amount/1000,
         accum_payment_plan=payment+payment_plan,
         accum_adjustment=accum_payment_plan+adjustment,
         accum_discount=accum_adjustment+discount,
         accum_writeoff=accum_discount+writeoff+liens,
         accum_unpaid_amount=accum_writeoff+unpaid_amount,
         accum_fees=accum_unpaid_amount+fees,
         accum_loss_discount=accum_fees+loss_discount,
         date=as.Date(paste0(year, "-", quarter*3-2, "-01")))

quarter_labels <- function(x) {
  paste0(year(x), "Q", quarter(x))
}

gg <- ggplot(data=hazard_payment_summary,
             aes(x=date, y=total_amount)) +
  geom_path(colour="black", size=1) + 
  geom_path(aes(x=date, y=payment), colour=colours_set[1], size=1) +
  geom_path(aes(x=date, y=accum_payment_plan), colour=colours_set[2], size=1) +
  geom_path(aes(x=date, y=accum_adjustment), colour=colours_set[3], size=1) +
  geom_path(aes(x=date, y=accum_discount), colour=colours_set[4], size=1) +
  geom_path(aes(x=date, y=accum_writeoff), colour=colours_set[5], size=1) +
  geom_path(aes(x=date, y=accum_unpaid_amount), colour=colours_set[6], size=1) +
  geom_path(aes(x=date, y=accum_fees), colour=colours_set[7], size=1) +
  geom_path(aes(x=date, y=accum_loss_discount), colour=colours_set[8], size=1) +
  geom_ribbon(aes(ymin=0, ymax=payment, fill="Payments"), alpha=0.8) +
  geom_ribbon(aes(ymin=payment, ymax=accum_payment_plan, fill="Payments Under Payment Plan"), alpha=0.8) +
  geom_ribbon(aes(ymin=accum_payment_plan, ymax=accum_adjustment, fill="Adjustments"), alpha=0.8) +
  geom_ribbon(aes(ymin=accum_adjustment, ymax=accum_discount, fill="Discounts"), alpha=0.8) +
  geom_ribbon(aes(ymin=accum_discount, ymax=accum_writeoff, fill="Write-Offs"), alpha=0.8) +
  geom_ribbon(aes(ymin=accum_writeoff, ymax=accum_unpaid_amount, fill="Remaining Unpaid"), alpha=0.8) +
  geom_ribbon(aes(ymin=accum_unpaid_amount, ymax=accum_fees, fill="Fees"), alpha=0.8) +
  geom_ribbon(aes(ymin=accum_fees, ymax=accum_loss_discount, fill="Loss From Delay"), alpha=0.8) +
  scale_x_continuous(breaks=seq(as.Date("2023-01-01"), as.Date("2025-03-31"), by="3 month"),
                     minor_breaks=seq(as.Date("2023-01-01"), as.Date("2025-03-31"), by="3 month"),
                     labels=quarter_labels) +
  scale_y_continuous(breaks=scales::pretty_breaks(n=5),
                     minor_breaks=seq(0, 4000, 500),
                     labels=scales::comma) +
  labs(x="Date (Year/Quarter)",
       y="Revnue ($ thousands)",
       title="Breakdown of Bill Collection for 2023Q1 Delinquents") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  scale_fill_manual(values=c("Payments"=colours_set[1],
                             "Payments Under Payment Plan"=colours_set[2],
                             "Adjustments"=colours_set[3],
                             "Discounts"=colours_set[4],
                             "Write-Offs"=colours_set[5],
                             "Remaining Unpaid"=colours_set[6],
                             "Fees"=colours_set[7],
                             "Loss From Delay"=colours_set[8]),
                    breaks=c("Payments", "Payments Under Payment Plan", 
                             "Adjustments", "Discounts", "Write-Offs",
                             "Remaining Unpaid", "Fees", "Loss From Delay"),
                    labels=c("Payments", "Payments Under Payment Plan",
                             "Adjustments", "Discounts", "Write-Offs", 
                             "Remaining Unpaid", "Fees", "Loss From Delay"),
                    name="Category")
gg
ggsave(gg,
       file="~/Downloads/bill_collection.png",
       width=12, height=8)

# Who are these people?
hazard_accounts <- hazard_off_delinquency %>%
  group_by(account_number) %>%
  summarise(payment_plan=any(in_payment_plan)) %>%
  ungroup() %>%
  left_join(income_credit %>% filter(year==2023, quarter==1), 
            by="account_number")

hazard_accounts <- hazard_accounts %>%
  filter(!is.na(etie)) %>%
  mutate(income=ifelse(etie>300, 300, etie),
         credit_score=ifelse(credit_score<400, 400, credit_score))

gg <- ggplot(hazard_accounts, 
             aes(x=income, fill=payment_plan)) +
  geom_density(alpha=0.5) +
  labs(title="Income Distribution of 2023Q1 Delinquents",
       x="Income ($ thousands)",
       y="Density") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  scale_fill_manual(values=c("FALSE"=colours_set[2],
                             "TRUE"=colours_set[1]),
                    breaks=c("TRUE", "FALSE"),
                    labels=c("Payment Plan", "No Payment Plan"),
                    name="")
gg  
ggsave(gg,
       file="~/Downloads/income_distribution.png",
       width=12, height=8)

gg <- ggplot(hazard_accounts, 
             aes(x=credit_score, fill=payment_plan)) +
  geom_density(alpha=0.5) +
  labs(title="Credit Score Distribution of 2023Q1 Delinquents",
       x="Credit Score",
       y="Density") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  scale_fill_manual(values=c("FALSE"=colours_set[2],
                             "TRUE"=colours_set[1]),
                    breaks=c("TRUE", "FALSE"),
                    labels=c("Payment Plan", "No Payment Plan"),
                    name="")
gg  
ggsave(gg,
       file="~/Downloads/credit_score_distribution.png",
       width=12, height=8)

hazard_accounts <- hazard_off_delinquency %>%
  filter(first_payment_date>"2024-03-31") %>%
  group_by(account_number) %>%
  summarise(payment_plan=any(in_payment_plan)) %>%
  ungroup() %>%
  left_join(income_credit %>% filter(year==2023, quarter==1), 
            by="account_number")

hazard_accounts <- hazard_accounts %>%
  filter(!is.na(etie)) %>%
  mutate(income=ifelse(etie>300, 300, etie),
         credit_score=ifelse(credit_score<400, 400, credit_score))

gg <- ggplot(hazard_accounts, 
             aes(x=income, fill=payment_plan)) +
  geom_density(alpha=0.5) +
  labs(title="Income Distribution of 2023Q1 Delinquents (Long Delinquency)",
       x="Income ($ thousands)",
       y="Density") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  scale_fill_manual(values=c("FALSE"=colours_set[2],
                             "TRUE"=colours_set[1]),
                    breaks=c("TRUE", "FALSE"),
                    labels=c("Payment Plan", "No Payment Plan"),
                    name="")
gg  
ggsave(gg,
       file="~/Downloads/income_distribution_long.png",
       width=12, height=8)

gg <- ggplot(hazard_accounts, 
             aes(x=credit_score, fill=payment_plan)) +
  geom_density(alpha=0.5) +
  labs(title="Credit Score Distribution of 2023Q1 Delinquents (Long Delinquency)",
       x="Credit Score",
       y="Density") +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom") +
  scale_fill_manual(values=c("FALSE"=colours_set[2],
                             "TRUE"=colours_set[1]),
                    breaks=c("TRUE", "FALSE"),
                    labels=c("Payment Plan", "No Payment Plan"),
                    name="")
gg  
ggsave(gg,
       file="~/Downloads/credit_score_distribution_long.png",
       width=12, height=8)
