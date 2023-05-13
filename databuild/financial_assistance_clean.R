# Financial assistance information

load(file=paste0(working_data_dir, "/analysis_info.RData"))


# Payment arrangement amount ####
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
           payment_arrange_end_year>=2019) %>%
  group_by(SS_ACCOUNT_NO) %>%
  summarise(arrange_amount_due=sum(amount_due, na.rm=TRUE),
            arrange_amount_paid=sum(amount_paid, na.rm=TRUE),
            arrange_amount_terminated=sum(amount_outstanding, na.rm=TRUE)) %>%
  ungroup()

# Payment arrangement by year
payment_arrange_by_year <- payment_arrangement %>%
  filter(grepl("/", START_DT),
         grepl("/", END_DT)) %>%
  mutate(ACCOUNT_NO=as.character(SS_ACCOUNT_NO),
         payment_arrange_start=mdy(START_DT),
         payment_arrange_end=mdy(END_DT)) %>%
  rowwise() %>%
  mutate(payment_arrange_2019=
           between(2019,
                   year(payment_arrange_start),
                   year(payment_arrange_end)),
         payment_arrange_2020=
           between(2020,
                   year(payment_arrange_start),
                   year(payment_arrange_end)),
         payment_arrange_2021=
           between(2021,
                   year(payment_arrange_start),
                   year(payment_arrange_end)),
         payment_arrange_2022=
           between(2022,
                   year(payment_arrange_start),
                   year(payment_arrange_end))) %>%
  group_by(ACCOUNT_NO) %>%
  summarise(payment_arrange_2019=sum(payment_arrange_2019),
            payment_arrange_2020=sum(payment_arrange_2020),
            payment_arrange_2021=sum(payment_arrange_2021),
            payment_arrange_2022=sum(payment_arrange_2022)) %>%
  ungroup() %>%
  mutate(payment_arrange_2019=payment_arrange_2019>0,
         payment_arrange_2020=payment_arrange_2020>0,
         payment_arrange_2021=payment_arrange_2021>0,
         payment_arrange_2022=payment_arrange_2022>0)


# Financial assistance by LINC tier ####
financial_assist_detail <- list.files(path=data_dir,
                                      pattern="Linc Data - *",
                                      full.names=TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

financial_assist_detail <- financial_assist_detail %>%
  filter(FINAL=="N") %>%
  mutate(bill_year=year(mdy(BILL_DT)))

linc_info <- financial_assist_detail %>%
  mutate(tier=as.numeric(gsub("Tier", "", LINC_TIER_TYPE))) %>%
  group_by(ACCOUNT_NO, bill_year) %>%
  summarise(tier=max(tier),
            discount_amount=(-1)*sum(LINC_DISCOUNT_AMT, na.rm=TRUE),
            crisis_voucher=(-1)*sum(CRISIS_VOUCHER_AMT, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols=ACCOUNT_NO, 
              names_from=bill_year, 
              values_from=c("tier",
                            "discount_amount",
                            "crisis_voucher"),
              values_fill=NA)

# Financial assistance by year
financial_assist_by_year <- financial_assist %>%
  mutate(ACCOUNT_NO=as.character(ACCOUNT_NO),
         financial_assist_start=mdy(EFFECTIVE_DT),
         financial_assist_end=mdy(EXPIRY_DT)) %>%
  rowwise() %>%
  mutate(financial_assist_2019=
           between(2019,
                   year(financial_assist_start),
                   year(financial_assist_end)),
         financial_assist_2020=
           between(2020,
                   year(financial_assist_start),
                   year(financial_assist_end)),
         financial_assist_2021=
           between(2021,
                   year(financial_assist_start),
                   year(financial_assist_end)),
         financial_assist_2022=
           between(2022,
                   year(financial_assist_start),
                   year(financial_assist_end))) %>%
  group_by(ACCOUNT_NO) %>%
  summarise(financial_assist_2019=sum(financial_assist_2019),
            financial_assist_2020=sum(financial_assist_2020),
            financial_assist_2021=sum(financial_assist_2021),
            financial_assist_2022=sum(financial_assist_2022)) %>%
  ungroup() %>%
  mutate(financial_assist_2019=financial_assist_2019>0,
         financial_assist_2020=financial_assist_2020>0,
         financial_assist_2021=financial_assist_2021>0,
         financial_assist_2022=financial_assist_2022>0)

save(payment_arrange_amount, payment_arrange_by_year,
     linc_info, financial_assist_detail, financial_assist_by_year,
     file=paste0(working_data_dir, "/financial_assistance_info.RData"))
