# Additional analysis on payment arrangement

load(file=paste0(working_data_dir, "/analysis_info.RData"))
load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/financial_assistance_info.RData"))


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
account_info_subset <- account_info %>%
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
  mutate(Type=c("Arrangement Not Terminated, Not LINC",
                "Arrangement Not Terminated, LINC",
                "Arrangement Terminated, Not LINC",
                "Arrangement Terminated, LINC"),
         Type=factor(Type,
                     levels=c("Arrangement Not Terminated, Not LINC",
                              "Arrangement Not Terminated, LINC",
                              "Arrangement Terminated, Not LINC",
                              "Arrangement Terminated, LINC")))

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
                   size=3, nudge_x=0.65, family="serif", show.legend=FALSE,
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
         crisis_voucher=ifelse(is.na(crisis_voucher), FALSE, crisis_voucher)) %>%
  group_by(payment_arrange, financial_assist, crisis_voucher) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(payment_arrange, financial_assist, crisis_voucher) %>%
  mutate(proportion=count/sum(count)*100,
         payment_arrange=ifelse(payment_arrange, "$\\checkmark$", ""),
         financial_assist=ifelse(financial_assist, "$\\checkmark$", ""),
         crisis_voucher=ifelse(crisis_voucher, "$\\checkmark$", ""))

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

# Average payment arrangement amount
bill_info <- bill_info %>%
  mutate(PERIOD_FROM_DT=mdy(PERIOD_FROM_DT),
         PERIOD_TO_DT=mdy(PERIOD_TO_DT),
         DUE_DT=mdy(DUE_DT))

bill_info_filtered <- bill_info %>% 
  filter(!CANCELED_BILL_YN,
         !is.na(PERIOD_FROM_DT), 
         !is.na(PERIOD_TO_DT),
         !is.na(DUE_DT),
         !ERROR_YN,
         AUDIT_OR_LIVE=="L",
         BILL_TP %in% c("REGLR", "MSTMT"),
         SOURCE_CD %in% c("", "QB1", "QB2", "QB3"),
         !CORRECTED_BILL_YN)

# Those who are not on a payment plan
no_plan_bill <- bill_info_filtered %>%
  filter(SOURCE_CD=="") %>%
  mutate(delinquent=PREV_BILL_AMT+TOTAL_PAYMENTS>0,
         delinquent=ifelse(is.na(delinquent), FALSE, delinquent),
         delinquent_amount=ifelse(PREV_BILL_AMT+TOTAL_PAYMENTS>0,
                                  PREV_BILL_AMT+TOTAL_PAYMENTS,
                                  0),
         delinquent_amount=ifelse(is.na(delinquent_amount),
                                  0,
                                  delinquent_amount),
         due_year=year(DUE_DT)) %>%
  select(ACCOUNT_NO, DUE_DT,
         due_year, delinquent, delinquent_amount,
         PREV_BILL_AMT, AR_DUE_BEFORE_BILL, SOURCE_CD,
         PERIOD_FROM_DT, PERIOD_TO_DT)

# Those on a payment plan
plan_bill <- bill_info_filtered %>%
  filter(SOURCE_CD!="") %>%
  mutate(delinquent=AR_DUE_BEFORE_BILL>0,
         delinquent=ifelse(is.na(delinquent), FALSE, delinquent),
         delinquent_amount=ifelse(AR_DUE_BEFORE_BILL>0,
                                  AR_DUE_BEFORE_BILL,
                                  0),
         delinquent_amount=ifelse(is.na(delinquent_amount),
                                  0,
                                  delinquent_amount),
         due_year=year(DUE_DT)) %>%
  select(ACCOUNT_NO, DUE_DT,
         due_year, delinquent, delinquent_amount,
         PREV_BILL_AMT, AR_DUE_BEFORE_BILL, SOURCE_CD,
         PERIOD_FROM_DT, PERIOD_TO_DT)

delinquency_status <- rbind(no_plan_bill, plan_bill) %>%
  mutate(ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
  left_join(account_info_subset,
            by="ACCOUNT_NO") %>%
  filter(account)

# Payment arrangement
payment_arrange_time <- payment_arrange_amount %>%
  group_by(SS_ACCOUNT_NO) %>%
  arrange(payment_arrange_start, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(payment_arrange_start)) >
                            cummax(as.numeric(payment_arrange_end)))[-n()])) %>%
  group_by(SS_ACCOUNT_NO, indx) %>%
  summarise(terminated=any(STATUS_CD=="T"),
            payment_arrange_start=min(payment_arrange_start), 
            payment_arrange_end=max(payment_arrange_end)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(payment_arrange_start=ifelse(is.na(payment_arrange_start),
                                      mdy("12/31/2099"),
                                      payment_arrange_start),
         payment_arrange_end=ifelse(is.na(payment_arrange_end),
                                    mdy("12/31/2099"),
                                    payment_arrange_end))

payment_arrange_time_count <- payment_arrange_time %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(count=n())

payment_arrange_time_count_1 <- payment_arrange_time_count %>%
  filter(count==1)

payment_arrange_time_count_above_1 <- payment_arrange_time_count %>%
  filter(count>1)

payment_arrange_time_above_1 <- payment_arrange_time %>%
  filter(SS_ACCOUNT_NO %in% payment_arrange_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_none <- delinquency_status %>%
  filter(!(ACCOUNT_NO %in% c(payment_arrange_time_count_above_1$SS_ACCOUNT_NO,
                             payment_arrange_time_count_1$SS_ACCOUNT_NO))) %>%
  mutate(payment_arrange=FALSE,
         payment_arrange_status=NA)

delinquency_status_sub <- delinquency_status %>%
  filter(ACCOUNT_NO %in% payment_arrange_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_rest <- delinquency_status %>%
  filter(ACCOUNT_NO %in% payment_arrange_time_count_1$SS_ACCOUNT_NO) %>%
  left_join(payment_arrange_time %>%
              filter(SS_ACCOUNT_NO %in% payment_arrange_time_count_1$SS_ACCOUNT_NO),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO")) %>%
  rowwise() %>%
  mutate(payment_arrange=between(DUE_DT, payment_arrange_start, payment_arrange_end),
         payment_arrange_status=ifelse(payment_arrange, terminated, NA)) %>%
  ungroup() %>%
  mutate(payment_arrange=ifelse(is.na(payment_arrange), FALSE, payment_arrange)) %>%
  select(-payment_arrange_start, -payment_arrange_end, -terminated)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(payment_arrange=any(DUE_DT %between%
                               list(subset(payment_arrange_time_above_1,
                                           SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_start,
                                    subset(payment_arrange_time_above_1,
                                           SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_end)),
         payment_arrange_status=
           ifelse(payment_arrange,
                  subset(payment_arrange_time_above_1,
                         SS_ACCOUNT_NO==ACCOUNT_NO)$terminated[
                           which(DUE_DT %between%
                                   list(subset(payment_arrange_time_above_1,
                                               SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_start,
                                        subset(payment_arrange_time_above_1,
                                               SS_ACCOUNT_NO==ACCOUNT_NO)$payment_arrange_end))],
                  NA)) %>%
  ungroup()

delinquency_status <- rbind(delinquency_status_sub,
                            delinquency_status_rest,
                            delinquency_status_none)

# Financial assistance
financial_assist_time <- financial_assist_account %>%
  mutate(SS_ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
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
  mutate(financial_assist=between(DUE_DT, financial_assist_start, financial_assist_end)) %>%
  ungroup() %>%
  mutate(financial_assist=ifelse(is.na(financial_assist), FALSE, financial_assist)) %>%
  select(-financial_assist_start, -financial_assist_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(financial_assist=any(DUE_DT %between%
                                list(subset(financial_assist_time_above_1,
                                            SS_ACCOUNT_NO==ACCOUNT_NO)$financial_assist_start,
                                     subset(financial_assist_time_above_1,
                                            SS_ACCOUNT_NO==ACCOUNT_NO)$financial_assist_end))) %>%
  ungroup()

delinquency_status <- rbind(delinquency_status_sub,
                            delinquency_status_rest,
                            delinquency_status_none)

# Cutoff/reconnect
cutoff_reconnect <-
  left_join(cutoff_info %>%
              mutate(CUTOFF_DATE=mdy(EFFECTIVE_DT)) %>%
              arrange(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE) %>%
              group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO) %>%
              mutate(id=row_number()) %>%
              ungroup() %>%
              select(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE, id),
            reconnect_info %>%
              mutate(RECONNECT_DATE=mdy(EFFECTIVE_DT)) %>%
              arrange(ACCOUNT_NO, PERSON_NO, LOCATION_NO, RECONNECT_DATE) %>%
              group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO) %>%
              mutate(id=row_number()) %>%
              ungroup() %>%
              select(ACCOUNT_NO, PERSON_NO, LOCATION_NO, RECONNECT_DATE, id),
            by=c("ACCOUNT_NO", "PERSON_NO", "LOCATION_NO", "id")) %>%
  select(-id) %>%
  arrange(ACCOUNT_NO, PERSON_NO, LOCATION_NO, CUTOFF_DATE)

cutoff_reconnect$CUTOFF_DATE[is.na(cutoff_reconnect$CUTOFF_DATE)] <-
  "2000-01-01"
cutoff_reconnect$RECONNECT_DATE[is.na(cutoff_reconnect$RECONNECT_DATE)] <-
  "2099-12-31"

cutoff_time <- cutoff_reconnect %>%
  mutate(SS_ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
  group_by(SS_ACCOUNT_NO) %>%
  arrange(CUTOFF_DATE, by_group=TRUE) %>% 
  mutate(indx=c(0, cumsum(as.numeric(lead(CUTOFF_DATE)) >
                            cummax(as.numeric(RECONNECT_DATE)))[-n()])) %>%
  group_by(SS_ACCOUNT_NO, indx) %>%
  summarise(cutoff_start=min(CUTOFF_DATE), 
            cutoff_end=max(RECONNECT_DATE)) %>%
  select(-indx) %>%
  ungroup() %>%
  mutate(cutoff_start=ifelse(is.na(cutoff_start),
                             mdy("12/31/2099"),
                             cutoff_start),
         cutoff_end=ifelse(is.na(cutoff_end),
                           mdy("12/31/2099"),
                           cutoff_end))

cutoff_time_count <- cutoff_time %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(count=n())

cutoff_time_count_1 <- cutoff_time_count %>%
  filter(count==1)

cutoff_time_count_above_1 <- cutoff_time_count %>%
  filter(count>1)

cutoff_time_above_1 <- cutoff_time %>%
  filter(SS_ACCOUNT_NO %in% cutoff_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_none <- delinquency_status %>%
  filter(!(ACCOUNT_NO %in% c(cutoff_time_count_above_1$SS_ACCOUNT_NO,
                             cutoff_time_count_1$SS_ACCOUNT_NO))) %>%
  mutate(cutoff=FALSE)

delinquency_status_sub <- delinquency_status %>%
  filter(ACCOUNT_NO %in% cutoff_time_count_above_1$SS_ACCOUNT_NO)

delinquency_status_rest <- delinquency_status %>%
  filter(ACCOUNT_NO %in% cutoff_time_count_1$SS_ACCOUNT_NO) %>%
  left_join(cutoff_time %>%
              filter(SS_ACCOUNT_NO %in% cutoff_time_count_1$SS_ACCOUNT_NO),
            by=c("ACCOUNT_NO"="SS_ACCOUNT_NO")) %>%
  rowwise() %>%
  mutate(cutoff=between(DUE_DT, cutoff_start, cutoff_end)) %>%
  ungroup() %>%
  mutate(cutoff=ifelse(is.na(cutoff), FALSE, cutoff)) %>%
  select(-cutoff_start, -cutoff_end)

delinquency_status_sub <- delinquency_status_sub %>%
  rowwise() %>%
  mutate(cutoff=any(DUE_DT %between%
                      list(subset(cutoff_time_above_1,
                                  SS_ACCOUNT_NO==ACCOUNT_NO)$cutoff_start,
                           subset(cutoff_time_above_1,
                                  SS_ACCOUNT_NO==ACCOUNT_NO)$cutoff_end))) %>%
  ungroup()

delinquency_status <- rbind(delinquency_status_sub,
                            delinquency_status_rest,
                            delinquency_status_none)


# Bills of someone who initiated a payment plan ####
delinquency_status <- delinquency_status %>%
  arrange(ACCOUNT_NO, DUE_DT) %>%
  mutate(period_month=round(interval(PERIOD_FROM_DT, PERIOD_TO_DT)/months(1)),
         period_month=ifelse(period_month==0, 1, period_month),
         delinquent_amount=delinquent_amount/period_month)

delinquency_payment_arrange <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(payment_arrange)) %>%
  group_by(ACCOUNT_NO, payment_arrange) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill) %>%
  group_by(payment_arrange) %>%
  summarise(nrow=n(),
            delinquency_rate_mean=mean(delinquency_rate, na.rm=TRUE),
            delinquency_rate_sd=sd(delinquency_rate, na.rm=TRUE),
            delinquent_amount_mean=mean(delinquent_amount, na.rm=TRUE),
            delinquent_amount_sd=sd(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate_se=delinquency_rate_sd/sqrt(nrow),
         delinquency_rate_lower_ci=
           delinquency_rate_mean-qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquency_rate_upper_ci=
           delinquency_rate_mean+qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquent_amount_se=delinquent_amount_sd/sqrt(nrow),
         delinquent_amount_lower_ci=
           delinquent_amount_mean-qt(1-0.05/2, nrow-1)*delinquent_amount_se,
         delinquent_amount_upper_ci=
           delinquent_amount_mean+qt(1-0.05/2, nrow-1)*delinquent_amount_se)

delinquency_financial_assist <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(financial_assist)) %>%
  group_by(ACCOUNT_NO, financial_assist) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill) %>%
  group_by(financial_assist) %>%
  summarise(nrow=n(),
            delinquency_rate_mean=mean(delinquency_rate, na.rm=TRUE),
            delinquency_rate_sd=sd(delinquency_rate, na.rm=TRUE),
            delinquent_amount_mean=mean(delinquent_amount, na.rm=TRUE),
            delinquent_amount_sd=sd(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate_se=delinquency_rate_sd/sqrt(nrow),
         delinquency_rate_lower_ci=
           delinquency_rate_mean-qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquency_rate_upper_ci=
           delinquency_rate_mean+qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquent_amount_se=delinquent_amount_sd/sqrt(nrow),
         delinquent_amount_lower_ci=
           delinquent_amount_mean-qt(1-0.05/2, nrow-1)*delinquent_amount_se,
         delinquent_amount_upper_ci=
           delinquent_amount_mean+qt(1-0.05/2, nrow-1)*delinquent_amount_se)

delinquency_bar <-
  rbind(delinquency_financial_assist %>%
          mutate(type=financial_assist,
                 Variable="Financial Assistance") %>%
          select(type, Variable, starts_with("delin")),
        delinquency_payment_arrange %>%
          mutate(type=payment_arrange,
                 Variable="Payment Arrangement") %>%
          select(type, Variable, starts_with("delin"))) %>%
  mutate(Variable=factor(Variable, levels=c("Financial Assistance",
                                            "Payment Arrangement")),
         type=ifelse(type, "Enroled", "Not Enroled"),
         type=factor(as.character(type), levels=c("Enroled", "Not Enroled")))

gg <- ggplot(delinquency_bar,
             aes(x=Variable, y=delinquency_rate_mean, fill=type)) + 
  geom_bar(position=position_dodge(),
           stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=delinquency_rate_lower_ci,
                    ymax=delinquency_rate_upper_ci),
                position=position_dodge(0.9),
                width=0.2, colour="orange", alpha=0.7, size=0.3) +
  fte_theme() +
  xlab("") + ylab("") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/delinquency_rate_bar.png"),
       width=6, height=4)

gg <- ggplot(delinquency_bar,
             aes(x=Variable, y=delinquent_amount_mean, fill=type)) + 
  geom_bar(position=position_dodge(),
           stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=delinquent_amount_lower_ci,
                    ymax=delinquent_amount_upper_ci),
                position=position_dodge(0.9),
                width=0.2, colour="orange", alpha=0.7, size=0.3) +
  fte_theme() +
  xlab("") + ylab("") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/delinquent_amount_bar.png"),
       width=6, height=4)


# Run regressions ####
delinquency_payment_arrange <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(payment_arrange)) %>%
  group_by(ACCOUNT_NO, payment_arrange) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill)

delinquency_financial_assist <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(financial_assist)) %>%
  group_by(ACCOUNT_NO, financial_assist) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill)

model1 <- lm(delinquency_rate~payment_arrange,
             data=delinquency_payment_arrange)

model2 <- lm(delinquent_amount~payment_arrange,
             data=delinquency_payment_arrange)

model3 <- lm(delinquency_rate~financial_assist,
             data=delinquency_financial_assist)

model4 <- lm(delinquent_amount~financial_assist,
             data=delinquency_financial_assist)

tab_data <- data.frame(coef_arrange=c(summary(model1)$coefficients[2,1], NA,
                                      summary(model2)$coefficients[2,1], NA),
                       se_arrange=c(summary(model1)$coefficients[2,2], NA, 
                                    summary(model2)$coefficients[2,2], NA),
                       p_arrange=c(summary(model1)$coefficients[2,4], NA,
                                   summary(model2)$coefficients[2,4], NA),
                       coef_assist=c(NA, summary(model3)$coefficients[2,1],
                                     NA, summary(model4)$coefficients[2,1]),
                       se_assist=c(NA, summary(model3)$coefficients[2,2],
                                   NA, summary(model4)$coefficients[2,2]),
                       p_assist=c(NA, summary(model3)$coefficients[2,4],
                                  NA, summary(model4)$coefficients[2,4]),
                       coef_intercept=c(summary(model1)$coefficients[1,1],
                                        summary(model3)$coefficients[1,1],
                                        summary(model2)$coefficients[1,1],
                                        summary(model4)$coefficients[1,1]),
                       se_intercept=c(summary(model1)$coefficients[1,2],
                                      summary(model3)$coefficients[1,2],
                                      summary(model2)$coefficients[1,2],
                                      summary(model4)$coefficients[1,2]),
                       p_intercept=c(summary(model1)$coefficients[1,4],
                                     summary(model3)$coefficients[1,4],
                                     summary(model2)$coefficients[1,4],
                                     summary(model4)$coefficients[1,4]))

tab <- TR(c("", "Delinquency Rate", "Delinquent Amount"), cspan=c(1, 2, 2)) +
  midrulep(list(c(2, 3), c(4, 5))) +
  TR(c("", "(1)", "(2)", "(3)", "(4)")) +
  midrule() +
  TR("Payment Arrangement") %:% with(tab_data, TR(coef_arrange, dec=3, pvalues=p_arrange)) +
  TR("") %:% with(tab_data, TR(se_arrange, dec=3, se=T)) +
  TR("Financial Assistance") %:% with(tab_data, TR(coef_assist, dec=3, pvalues=p_assist)) +
  TR("") %:% with(tab_data, TR(se_assist, dec=3, se=T)) +
  TR("(Intercept)") %:% with(tab_data, TR(coef_intercept, dec=3, pvalues=p_intercept)) +
  TR("") %:% with(tab_data, TR(se_intercept, dec=3, se=T))

tab
TS(tab, file="delinquency_reg", header=c("lcccc"),
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
