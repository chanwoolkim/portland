#=========================================================================#
# Descriptive_statistics_payment_overview.R
#
# Create graphs for basic descriptive statistics (pie charts)
#
# Chanwool Kim, April 29, 2023
#
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# Additional analysis on payment arrangement
#---------+---------+---------+---------+---------+---------+

# LOAD DATA
load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))
load(file=paste0(working_data_dir, "/delinquency_status.RData"))


# Payment plan terminated pie chart ####
# Attach financial info onto payment arrangement
financial_assist_account <- financial_assist %>%
  mutate(financial_assist_start=mdy(EFFECTIVE_DT),
         financial_assist_end=mdy(EXPIRY_DT)) %>%
  filter(year(financial_assist_start)>=2019 |
           year(financial_assist_end)>=2019) %>%
  select(ACCOUNT_NO, financial_assist_start, financial_assist_end)

# Payment arrangement amount
payment_arrange_amount <- payment_arrangement_info %>%
  mutate(amount_paid=AMOUNT_DUE-OUTSTANDING_AMT) %>%
  group_by(PAY_ARRANGEMENT_REF) %>%
  summarise(amount_due=sum(AMOUNT_DUE, na.rm=TRUE),
            amount_paid=sum(amount_paid, na.rm=TRUE),
            amount_outstanding=sum(OUTSTANDING_AMT, na.rm=TRUE))

payment_arrange_amount <- payment_arrangement %>%
  filter(!grepl("^[0-9]", STATUS_CD)) %>%
  mutate(STATUS_CD=trimws(STATUS_CD),
         STATUS_CD=ifelse(STATUS_CD=="T", "T", "P"),
         payment_arrange_start=mdy(START_DT),
         payment_arrange_end=mdy(END_DT),
         payment_arrange_start_year=year(payment_arrange_start),
         payment_arrange_end_year=year(payment_arrange_end),
         ARRANGEMENT_AMT=as.numeric(ARRANGEMENT_AMT)) %>%
  right_join(payment_arrange_amount, by="PAY_ARRANGEMENT_REF") %>%
  filter(payment_arrange_start_year>=2019 |
           payment_arrange_end_year>=2019)

# Only consider single family
account_info_subset <- account_info_merge %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST")) %>%
  select(ACCOUNT_NO) %>%
  mutate(account=TRUE)

payment_arrange_account <-
  left_join(payment_arrange_amount,
            account_info_subset,
            by=c("SS_ACCOUNT_NO"="ACCOUNT_NO")) %>%
  filter(account) %>%
  select(PAY_ARRANGEMENT_REF, SS_ACCOUNT_NO, STATUS_CD,
         payment_arrange_start, payment_arrange_end) %>%
  left_join(financial_assist_account %>%
              mutate(ACCOUNT_NO=as.character(ACCOUNT_NO)),
            by=c("SS_ACCOUNT_NO"="ACCOUNT_NO")) %>%
  mutate(financial_assist=
           (payment_arrange_start <= financial_assist_end) &
           (payment_arrange_end >= financial_assist_start),
         financial_assist=ifelse(is.na(financial_assist),
                                 FALSE,
                                 financial_assist)) %>%
  group_by(PAY_ARRANGEMENT_REF, SS_ACCOUNT_NO, STATUS_CD) %>%
  summarise(financial_assist=sum(financial_assist, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(financial_assist=ifelse(financial_assist>0,
                                 TRUE,
                                 FALSE))

payment_arrange_pie <- payment_arrange_account %>%
  group_by(STATUS_CD, financial_assist) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  mutate(Type=c("Arrangement Not Terminated,\nNo Financial Assistance",
                "Arrangement Not Terminated,\nReceived Financial Assistance",
                "Arrangement Terminated,\nNo Financial Assistance",
                "Arrangement Terminated,\nReceived Financial Assistance"),
         Type=factor(Type,
                     levels=c("Arrangement Not Terminated,\nNo Financial Assistance",
                              "Arrangement Not Terminated,\nReceived Financial Assistance",
                              "Arrangement Terminated,\nNo Financial Assistance",
                              "Arrangement Terminated,\nReceived Financial Assistance")))

payment_arrange_pie_annotate <- payment_arrange_pie %>% 
  mutate(csum=rev(cumsum(rev(count))), 
         pos=count/2+lead(csum, 1),
         pos=if_else(is.na(pos), count/2, pos),
         count_percent=count/sum(count))

gg <- ggplot(payment_arrange_pie,
             aes(x="", y=count, fill=Type)) + 
  geom_bar(stat="identity", width=1) +
  geom_label_repel(data=payment_arrange_pie_annotate,
                   aes(y=pos,
                       label=paste0(count, " (", round(count_percent*100, 1), "%)")),
                   size=3, nudge_x=0.1, family="serif", show.legend=FALSE,
                   colour=brewer.pal("Greys", n=9)[7],
                   segment.colour=brewer.pal("Greys", n=9)[7],
                   label.size=0.1) +
  coord_polar("y", start=0) +
  pie_theme() +
  scale_fill_manual(values=colours_set) +
  labs(fill="Payment Status") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/payment_arrange_pie.png"),
       width=6, height=4)


# Account that received financial assistance ####
# Only consider single family
account_info_merge <- account_info_merge %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST"))

account_count <- account_info_merge %>%
  mutate(crisis_voucher=crisis_voucher>0,
         crisis_voucher=ifelse(is.na(crisis_voucher), FALSE, crisis_voucher),
         delinquent=delinquent>0) %>%
  group_by(payment_arrange, financial_assist, crisis_voucher) %>%
  summarise(count=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            cutoff=sum(cutoff, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(payment_arrange, financial_assist, crisis_voucher) %>%
  mutate(proportion=count/sum(count)*100,
         payment_arrange=ifelse(payment_arrange, "$\\checkmark$", ""),
         financial_assist=ifelse(financial_assist, "$\\checkmark$", ""),
         crisis_voucher=ifelse(crisis_voucher, "$\\checkmark$", ""),
         delinquent_proportion=delinquent/count*100,
         cutoff_proportion=cutoff/count*100) %>%
  select(payment_arrange, financial_assist, crisis_voucher,
         count, proportion,
         delinquent, delinquent_proportion,
         cutoff, cutoff_proportion)

tab_row <- function(row) {
  add_percent <- function(row_tab) {
    row_tab$row_list[[1]][row_tab$ncol] <-
      str_c(row_tab$row_list[[1]][row_tab$ncol], "\\%")
    return(row_tab)
  }
  out <- TR(account_count[row, 1:3] %>% as.character()) %:%
    TR(account_count[row, 4:5] %>% as.numeric(), dec=c(5, 2))
  out <- add_percent(out)
  return(out)
}

tab <- TR(c("Payment Arrangement", "Financial Assistance", "Crisis Voucher",
            "Count", "Proportion")) +
  midrule() +
  tab_row(1) + tab_row(2) + tab_row(3) +
  tab_row(4) + tab_row(5) + tab_row(6)

tab <- fix_0(tab)
TS(tab, file="assistance_count", header=c("c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)

tab_row <- function(row) {
  add_percent <- function(row_tab) {
    row_tab$row_list[[1]][5] <-
      str_c(row_tab$row_list[[1]][5], "\\%")
    row_tab$row_list[[1]][7] <-
      str_c(row_tab$row_list[[1]][7], "\\%")
    row_tab$row_list[[1]][9] <-
      str_c(row_tab$row_list[[1]][9], "\\%")
    return(row_tab)
  }
  out <- TR(account_count[row, 1:3] %>% as.character()) %:%
    TR(account_count[row, 4:9] %>% as.numeric(), dec=c(5, 2, 5, 2, 5, 2))
  out <- add_percent(out)
  return(out)
}

tab <- TR(c("Payment", "Financial", "Crisis",
            "", "", "Delinquent", "Delinquent", "Shutoff", "Shutoff")) +
  TR(c("Arrangement", "Assistance", "Voucher",
       "Count", "Proportion", "Count", "Proportion", "Count", "Proportion")) +
  midrule() +
  tab_row(1) + tab_row(2) + tab_row(3) +
  tab_row(4) + tab_row(5) + tab_row(6)

tab <- fix_0(tab)
TS(tab, file="assistance_count_expand", header=c("c|c|c|c|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)


# Information on payment plans ####
payment_arrange_info <-
  left_join(payment_arrange_amount,
            account_info_subset,
            by=c("SS_ACCOUNT_NO"="ACCOUNT_NO")) %>%
  filter(account) %>%
  mutate(amount=ARRANGEMENT_AMT,
         status=ifelse(STATUS_CD=="P", "Good", "Terminated"),
         status=factor(status, levels=c("Good", "Terminated")))

gg <- ggplot(payment_arrange_info %>%
               group_by() %>%
               mutate(q95=quantile(amount, 0.95)) %>%
               filter(amount<=q95),
             aes(x=amount, fill=status)) + 
  geom_histogram() +
  fte_theme() +
  xlab("Payment Arrangement Amount") + ylab("Count") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/payment_arrange_amount_hist.png"),
       width=6, height=4)

gg <- ggplot(payment_arrange_info %>%
               group_by() %>%
               mutate(q95=quantile(amount, 0.95),
                      q999=quantile(amount, 0.999)) %>%
               filter(amount>q95, amount<=q999),
             aes(x=amount, fill=status)) + 
  geom_histogram() +
  fte_theme() +
  xlab("Payment Arrangement Amount") + ylab("Count") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/payment_arrange_amount_large_hist.png"),
       width=6, height=4)

payment_arrange_bill_info <-
  payment_arrangement_info %>%
  filter(PAY_ARRANGEMENT_REF %in% payment_arrange_info$PAY_ARRANGEMENT_REF) %>%
  left_join(payment_arrange_info %>%
              select(PAY_ARRANGEMENT_REF,
                     STATUS_CD),
            by="PAY_ARRANGEMENT_REF") %>%
  group_by(PAY_ARRANGEMENT_REF) %>%
  arrange(mdy(DUE_DT)) %>%
  mutate(prev_date=lag(DUE_DT)) %>%
  ungroup() %>%
  mutate(duration=round(interval(mdy(prev_date), mdy(DUE_DT))/months(1)),
         duration=ifelse(is.na(duration) | duration==0, 1, duration),
         amount=AMOUNT_DUE/duration,
         status=ifelse(STATUS_CD=="P", "Good", "Terminated"),
         status=factor(status, levels=c("Good", "Terminated")))

gg <- ggplot(payment_arrange_bill_info %>%
               group_by() %>%
               mutate(q95=quantile(amount, 0.95)) %>%
               filter(amount<=q95),
             aes(x=amount, fill=status)) + 
  geom_histogram() +
  fte_theme() +
  xlab("Payment Arrangement Bill Amount") + ylab("Count") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/payment_arrange_bill_amount_hist.png"),
       width=6, height=4)

gg <- ggplot(payment_arrange_bill_info %>%
               group_by() %>%
               mutate(q95=quantile(amount, 0.95),
                      q999=quantile(amount, 0.999)) %>%
               filter(amount>q95, amount<=q999),
             aes(x=amount, fill=status)) + 
  geom_histogram() +
  fte_theme() +
  xlab("Payment Arrangement Bill Amount") + ylab("Count") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/payment_arrange_bill_amount_large_hist.png"),
       width=6, height=4)

delinquency_status <- delinquency_status %>%
  arrange(ACCOUNT_NO, DUE_DT) %>%
  mutate(period_month=round(interval(PERIOD_FROM_DT, PERIOD_TO_DT)/months(1)),
         period_month=ifelse(period_month==0, 1, period_month),
         delinquent_amount=delinquent_amount/period_month)

delinquency_payment_arrange <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(payment_arrange)) %>%
  ungroup() %>%
  mutate(amount_owed=ifelse(is.na(AR_DUE_BEFORE_BILL), 0, AR_DUE_BEFORE_BILL))

delinquency_payment_arrange <- delinquency_payment_arrange %>%
  arrange(ACCOUNT_NO, DUE_DT) %>%
  group_by(ACCOUNT_NO) %>%
  mutate(row_number=row_number(),
         change=payment_arrange!=lag(payment_arrange)) %>%
  ungroup() %>%
  mutate(change=ifelse(row_number==1, TRUE, change),
         first_arrange=payment_arrange & change)

delinquency_payment_arrange_initial <- delinquency_payment_arrange %>%
  filter(first_arrange) %>%
  select(ACCOUNT_NO, DUE_DT, AR_DUE_BEFORE_BILL, payment_arrange_status) %>%
  mutate(after_6=DUE_DT+months(6),
         after_12=DUE_DT+months(12))

delinquency_payment_arrange_initial <- delinquency_payment_arrange_initial %>%
  mutate(SS_ACCOUNT_NO=ACCOUNT_NO) %>%
  select(-ACCOUNT_NO) %>%
  rowwise() %>%
  mutate(after_6_amount=
           subset(delinquency_payment_arrange,
                  DUE_DT>=after_6 & ACCOUNT_NO==SS_ACCOUNT_NO)$AR_DUE_BEFORE_BILL[1],
         after_12_amount=
           subset(delinquency_payment_arrange,
                  DUE_DT>=after_12 & ACCOUNT_NO==SS_ACCOUNT_NO)$AR_DUE_BEFORE_BILL[1])

delinquency_payment_arrange_change <- delinquency_payment_arrange_initial %>%
  filter(!(is.na(after_6_amount) | is.na(after_12_amount))) %>%
  mutate(more_6=after_6_amount>AR_DUE_BEFORE_BILL,
         more_12=after_12_amount>AR_DUE_BEFORE_BILL,
         change_6=(after_6_amount-AR_DUE_BEFORE_BILL)/AR_DUE_BEFORE_BILL,
         change_12=(after_12_amount-AR_DUE_BEFORE_BILL)/AR_DUE_BEFORE_BILL)

delinquency_payment_arrange_change_pie <- delinquency_payment_arrange_change %>%
  group_by() %>%
  summarise(count=n(),
            terminated=sum(payment_arrange_status, na.rm=TRUE),
            more_6=sum(more_6[!payment_arrange_status], na.rm=TRUE),
            more_12=sum(more_12[!payment_arrange_status], na.rm=TRUE))

pie_6 <- data.frame(type=c("Terminated", "Owed More", "Owed Equal or Less"),
                    value=c(delinquency_payment_arrange_change_pie$terminated,
                            delinquency_payment_arrange_change_pie$more_6,
                            delinquency_payment_arrange_change_pie$count-
                              delinquency_payment_arrange_change_pie$terminated-
                              delinquency_payment_arrange_change_pie$more_6))

gg <- ggplot(pie_6,
             aes(x="", y=value, fill=type)) + 
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label=paste0(value,
                             " (",
                             round(value/sum(value)*100,2),
                             "%)")),
            position=position_stack(vjust=0.5),
            color="white", size=4, family="serif") +
  coord_polar("y", start=0) +
  pie_theme() +
  labs(fill="Payment Arrangement - After 6 Months") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/after_6_months_pie.png"),
       width=6, height=4)

pie_12 <- data.frame(type=c("Terminated", "Owed More", "Owed Equal or Less"),
                     value=c(delinquency_payment_arrange_change_pie$terminated,
                             delinquency_payment_arrange_change_pie$more_12,
                             delinquency_payment_arrange_change_pie$count-
                               delinquency_payment_arrange_change_pie$terminated-
                               delinquency_payment_arrange_change_pie$more_12))

gg <- ggplot(pie_12,
             aes(x="", y=value, fill=type)) + 
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label=paste0(value,
                             " (",
                             round(value/sum(value)*100,2),
                             "%)")),
            position=position_stack(vjust=0.5),
            color="white", size=4, family="serif") +
  coord_polar("y", start=0) +
  pie_theme() +
  labs(fill="Payment Arrangement - After 12 Months") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/after_12_months_pie.png"),
       width=6, height=4)
