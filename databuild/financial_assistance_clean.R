# Financial assistance information

load(file=paste0(working_data_dir, "/analysis_info.RData"))


# Payment arrangement amount ####
payment_arrange_amount <- payment_arrangement_info %>%
  group_by(payment_plan_id) %>%
  summarise(amount_due=sum(amount, na.rm=TRUE),
            amount_outstanding=sum(outstanding_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(amount_paid=amount_due-amount_outstanding)

payment_arrange_amount <- payment_arrangement %>%
  filter(!grepl("^[0-9]", status_code)) %>%
  mutate(status_code=trimws(status_code),
         status_code=ifelse(status_code=="T", "T", "P"),
         payment_arrange_start=mdy(start_date),
         payment_arrange_end=mdy(end_date),
         payment_arrange_start_year=year(payment_arrange_start),
         payment_arrange_end_year=year(payment_arrange_end)) %>%
  right_join(payment_arrange_amount, by="payment_plan_id") %>%
  filter(payment_arrange_start_year>=2019 |
           payment_arrange_end_year>=2019) %>%
  group_by(account_number) %>%
  summarise(arrange_amount_due=sum(amount_due, na.rm=TRUE),
            arrange_amount_paid=sum(amount_paid, na.rm=TRUE),
            arrange_amount_terminated=sum(amount_outstanding, na.rm=TRUE)) %>%
  ungroup() %>%
  distinct()

# Payment arrangement by year
payment_arrange_by_year <- payment_arrangement %>%
  mutate(payment_arrange_start=mdy(start_date),
         payment_arrange_end=mdy(end_date)) %>%
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
                   year(payment_arrange_end)),
         payment_arrange_2023=
           between(2023,
                   year(payment_arrange_start),
                   year(payment_arrange_end)),
         payment_arrange_2024=
           between(2024,
                   year(payment_arrange_start),
                   year(payment_arrange_end))) %>%
  group_by(account_number) %>%
  summarise(payment_arrange_2019=sum(payment_arrange_2019),
            payment_arrange_2020=sum(payment_arrange_2020),
            payment_arrange_2021=sum(payment_arrange_2021),
            payment_arrange_2022=sum(payment_arrange_2022),
            payment_arrange_2023=sum(payment_arrange_2023),
            payment_arrange_2024=sum(payment_arrange_2024)) %>%
  ungroup() %>%
  mutate(payment_arrange_2019=payment_arrange_2019>0,
         payment_arrange_2020=payment_arrange_2020>0,
         payment_arrange_2021=payment_arrange_2021>0,
         payment_arrange_2022=payment_arrange_2022>0,
         payment_arrange_2023=payment_arrange_2023>0,
         payment_arrange_2024=payment_arrange_2024>0) %>%
  distinct()


# Financial assistance by LINC tier ####
financial_assist_csv <- financial_assist_detail %>%
  filter(grepl("/", bill_date)) %>%
  mutate_at(c("bill_date",
              "linc_effective_date", 
              "linc_expiry_date", 
              "date_last_updated"),
            mdy)

financial_assist_xlsx <- financial_assist_detail %>%
  filter(!grepl("/", bill_date)) %>%
  mutate_at(c("bill_date", 
              "linc_effective_date",
              "linc_expiry_date",
              "date_last_updated"),
            function(x) {as_date(as.numeric(x), origin="1900-01-01")})

financial_assist_detail <- bind_rows(financial_assist_csv, financial_assist_xlsx) %>%
  mutate(bill_year=year(bill_date),
         linc_expiry_year=year(linc_expiry_date),
         senior_disabilities=linc_expiry_year>2050,
         penalty_fees=ifelse(is.na(penalty_fees),
                             penalty_fee,
                             penalty_fees),
         penalty_fees_reversed=ifelse(is.na(penalty_fees_reversed),
                                      penalty_fee_reversed,
                                      penalty_fees_reversed)) %>%
  select(-penalty_fee, -penalty_fee_reversed) %>%
  mutate_at(c("location_number",
              "net_bill_amount", "billed_amount_before_discount", "linc_discount_amount",
              "water_consumption", "sewer_consumption",
              "penalty_fees", "penalty_fees_reversed",
              "crisis_voucher_amount"),
            as.numeric) %>%
  distinct()

linc_info <- financial_assist_detail %>%
  mutate(tier=as.numeric(gsub("Tier", "", linc_tier_type))) %>%
  group_by(location_number, bill_year) %>%
  summarise(tier=max(tier),
            discount_amount=(-1)*sum(linc_discount_amount, na.rm=TRUE),
            crisis_voucher=(-1)*sum(crisis_voucher_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols=location_number, 
              names_from=bill_year, 
              values_from=c("tier",
                            "discount_amount",
                            "crisis_voucher"),
              values_fill=NA) %>%
  distinct()

# Financial assistance by year
financial_assist_by_year <- financial_assist %>%
  mutate(financial_assist_start=mdy(effective_date),
         financial_assist_end=mdy(expiration_date)) %>%
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
                   year(financial_assist_end)),
         financial_assist_2023=
           between(2023,
                   year(financial_assist_start),
                   year(financial_assist_end)),
         financial_assist_2024=
           between(2024,
                   year(financial_assist_start),
                   year(financial_assist_end))) %>%
  group_by(account_number) %>%
  summarise(financial_assist_2019=sum(financial_assist_2019),
            financial_assist_2020=sum(financial_assist_2020),
            financial_assist_2021=sum(financial_assist_2021),
            financial_assist_2022=sum(financial_assist_2022),
            financial_assist_2023=sum(financial_assist_2023),
            financial_assist_2024=sum(financial_assist_2024)) %>%
  ungroup() %>%
  mutate(financial_assist_2019=financial_assist_2019>0,
         financial_assist_2020=financial_assist_2020>0,
         financial_assist_2021=financial_assist_2021>0,
         financial_assist_2022=financial_assist_2022>0,
         financial_assist_2023=financial_assist_2023>0,
         financial_assist_2024=financial_assist_2024>0) %>%
  distinct()

save(payment_arrange_amount, payment_arrange_by_year,
     linc_info, financial_assist_detail, financial_assist_by_year,
     file=paste0(working_data_dir, "/financial_assistance_info.RData"))
