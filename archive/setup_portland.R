# Load data ####
# Helper function
force_bind <- function(df1, df2) {
  colnames(df2) <- colnames(df1)
  df <- bind_rows(df1, df2)
  return(df)
}

# Location relation
location_relation <- read_csv(file=paste0(data_dir, "/UM00120T.csv"))

location_relation <- location_relation %>%
  rename(tract_id=TRACT_ID,
         tu_id=TU_ID,
         occupancy_status=REL_TP,
         occupancy_code=REL_SEQ,
         start_date=START_DATE,
         end_date=END_DATE,
         is_occupant=IS_OCCUPANT,
         is_owner=IS_OWNER,
         frc_connect_id=FRC_CONNECT_ID,
         date_created=CREATED,
         date_updated=UPDATED)

location_account_relation <- read_csv(file=paste0(data_dir, "/UM00250T.csv"))

location_account_relation <- location_account_relation %>%
  rename(tract_id=TRACT_ID,
         tu_id=TU_ID,
         date_created=CREATED,
         date_updated=UPDATED)

# Address info
address_info <- read_csv(file=paste0(data_dir, "/UM00100M.csv"))

address_info <- address_info %>%
  rename(tract_id=TRACT_ID,
         is_active=IS_ACTIVE,
         postal_code=POSTAL_CODE,
         date_created=CREATED,
         date_updated=UPDATED)

# Account info
account_info <- read_csv(file=paste0(data_dir, "/UM00200M.csv"))

account_info <- account_info %>%
  rename(tu_id=TU_ID,
         cycle_code=CYCLE_CODE,
         status_code=STATUS_CODE,
         status_start_date=STATUS_START_DATE,
         status_end_date=STATUS_END_DATE,
         billing_code=BILLING_CODE,
         is_closed=IS_CLOSED,
         bill_count=BILL_COUNT,
         occupancy_code=OCCUPANCY_CODE,
         is_overdue=IS_OVERDUE,
         last_bill_date=LAST_BILL_DATE,
         last_bill_timestamp=LAST_BILL_TIMESTAMP,
         last_bill_amount=LAST_BILL_AMOUNT,
         lien_status=LIEN_STATUS,
         lien_amount=LIEN_AMOUNT,
         bypass_writeoff=BYPASS_WRITE_OFF,
         no_bill_end_date=NO_BILL_END_DATE,
         no_bill_start_date=NO_BILL_START_DATE,
         is_hold_writeoff=IS_HOLD_WRITE_OFF,
         is_att_on_hold=IS_ATT_ON_HOLD,
         att_on_hold_end_date=ATT_ON_HOLD_END_DATE,
         bypass_writeoff_end_date=BYPASS_WRITE_OFF_END_DATE,
         date_created=CREATED,
         date_updated=UPDATED)

# Financial info
financial_info <-
  force_bind(read_csv((list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
                         str_subset("AR00200T"))[1]) %>%
               rename(transaction_number=TRANSACTION_ID,
                      subsystem_id=SUBSYSTEM_ID,
                      tu_id=TU_ID,
                      transaction_type=TRANSACTION_TYPE,
                      transaction_code=TRANSACTION_CODE,
                      summary=SUMMARY,
                      transaction_date=TRANSACTION_DATE,
                      matched_date=MATCHED_DATE,
                      last_matched_date=LAST_MATCHED_DATE,
                      amount=AMOUNT,
                      remaining_amount=REMAINING_AMOUNT,
                      status_code=STATUS_CODE,
                      source_reference=SOURCE_REFERENCE,
                      source_spec_2=SOURCE_SPEC2,
                      source_spec_3=SOURCE_SPEC3,
                      bill_reference=SS_BILL_REF,
                      bill_date_1=BILL_DATE_1,
                      bill_date_2=BILL_DATE_2,
                      bill_date_3=BILL_DATE_3,
                      bill_date=BILL_DATE,
                      taxable_amount=TAXABLE_AMOUNT,
                      is_cutoff=IS_CUTOFF,
                      is_subject_to_interest=IS_SUBJECT_TO_INT,
                      adjusted_amount=ADJUSTED_AMOUNT,
                      is_adjust_allowed=IS_ADJUST_ALLOWED,
                      due_date=DUE_DATE,
                      date_created=CREATED,
                      date_updated=UPDATED),
             (list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
                str_subset("AR00200T"))[-1] %>%
               lapply(read_csv, 
                      col_names=FALSE) %>%
               bind_rows)

# Bill info
bill_info <-
  force_bind(read_csv((list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
                         str_subset("UM00260T"))[1]) %>%
               rename(tu_id=TU_ID,
                      bill_date=BILL_DATE,
                      bill_timestamp=BILL_TIMESTAMP,
                      start_date=START_DATE,
                      end_date=END_DATE,
                      billing_days=BILLING_DAYS,
                      previous_bill_amount=PREVIOUS_BILL_AMOUNT,
                      due_date=DUE_DATE,
                      budget_bill_amount=BUDGET_BILL_AMOUNT,
                      discount_amount_due=DISCOUNT_AMOUNT_DUE,
                      discount_due_date=DISCOUNT_DUE_DATE,
                      discount_percentage=DISCOUNT_PERCENTAGE,
                      is_final=IS_FINAL,
                      is_error=IS_ERROR,
                      ar_due_before_bill=AR_DUE_BEFORE_BILL,
                      ar_unapplied_credit_before_bill=AR_UNAPPLIED_CR_BEFORE_BILL,
                      ar_due_after_bill=AR_DUE_AFTER_BILL,
                      ar_unapplied_credit_after_bill=AR_UNAPPLIED_CR_AFTER_BILL,
                      ar_net_after_bill=AR_NET_AFTER_BILL,
                      is_corrected=IS_CORRECTED,
                      is_voided=IS_VOIDED,
                      is_collection=IS_COLLECTION,
                      penalty_amount=PENALTY_AMOUNT,
                      type_code=TYPE_CODE,
                      is_canceled=IS_CANCELED,
                      is_rebill=IS_REBILL,
                      is_off_cycle=IS_OFF_CYCLE,
                      non_bill_generated_changes=NON_BILL_GENERAGED_CHANGES,
                      total_payments=TOTAL_PAYMENTS,
                      amount=AMOUNT,
                      is_bill_corrected=IS_BILL_CORRECTED,
                      discount_amont=DISCOUNT_AMONT,
                      source_code=SOURCE_CODE,
                      previous_amount=PREVIOUS_AMOUNT,
                      audit_or_live=AUDIT_OR_LIVE,
                      date_created=CREATED,
                      date_updated=UPDATED),
             (list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
                str_subset("UM00260T"))[-1] %>%
               lapply(read_csv, 
                      col_names=FALSE) %>%
               bind_rows)

# Financial assistance
financial_assist <- read_csv(file=paste0(data_dir, "/UM00232T.csv"))

financial_assist <- financial_assist %>%
  rename(transaction_id=TRANSACTION_ID,
         tu_id=TU_ID,
         effective_date=EFFECTIVE_DATE,
         review_date=REVIEW_DATE,
         expiration_date=EXPIRATION_DATE,
         status_code=STATUS_CODE,
         date_created=CREATED,
         date_updated=UPDATED)

financial_assist_detail <-
  list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
  str_subset("LINC") %>%
  lapply(read_csv) %>%
  bind_rows %>%
  rename(bill_date=BILL_DT,
         final=FINAL,
         net_bill_amount=NET_BILL_AMT,
         linc_tier_type=LINC_TIER_TYPE,
         billed_amount_before_discount=BILLED_AMT_BEFORE_DIS,
         linc_discount_amount=LINC_DISCOUNT_AMT,
         water_consumption=WATER_CONS,
         sewer_consumption=SEWER_CONS,
         penalty_fees=PENALTY_FEES,
         penalty_fees_reversed=PENALTY_FEES_REVERSED,
         crisis_voucher_amount=CRISIS_VOUCHER_AMT,
         day_of_service=DAY_OF_SERVICE,
         clean_river_rewards=CLEAN_RIVER_REWARDS,
         clean_river_percentage=CLEAN_RIVER_PERCENTAGE,
         linc_effective_date=LINC_EFFECTIVE_DATE,
         linc_expiry_date=LINC_EXPIRY_DATE,
         linc_total_count=LINC_TOTAL_COUNT,
         honored_citizen=HONORED_CITIZEN,
         monthly_statements=MONTHLY_STATEMENTS,
         monthly_statement_bill=MONTHLY_STATEMENT_BILL,
         cycle_code=CYCLE_CD,
         route_number=ROUTE_NO,
         date_last_updated=LAST_UPDATED,
         tu_id=TU_ID)

# Cutoff and reconnect
cutoff_info <- read_csv(file=paste0(data_dir, "/RS00200M.csv"))

cutoff_info <- cutoff_info %>%
  rename(action_id=ACTION_ID,
         request_number=REQUEST_NUMBER,
         request_type=REQUEST_TYPE,
         action_code=ACTION_CODE,
         action_type=ACTION_TYPE,
         source_code=SOURCE_CODE,
         tu_id=TU_ID,
         effective_date=EFFECTIVE_DATE,
         received_date=RECEIVED_DATE,
         scheduled_date=SCHEDULED_DATE,
         scheduled_timestamp=SCHEDULED_TIMESTAMP,
         resolution_code=RESOLUTION_CODE,
         resolution_date=RESOLUTION_DATE,
         status_code=STATUS_CODE,
         date_created=CREATED,
         date_updated=UPDATED)

# Payment arrangement
payment_arrangement <- read_csv(file=paste0(data_dir, "/CO00200M.csv"))

payment_arrangement <- payment_arrangement %>%
  rename(payment_plan_id=PAYMENT_PLAN_ID,
         tu_id=TU_ID,
         status_code=STATUS_CODE,
         start_date=START_DATE,
         end_date=END_DATE,
         payment_code=PAYMENT_CODE,
         payment_amount=PAYMENT_AMOUNT,
         is_penalty_waived=IS_PENALTY_WAIVED,
         is_variable_waived=IS_VARIABLE_WAIVED,
         is_broken_waived=IS_BROKEN_WAIVED,
         is_interest_waived=IS_INTEREST_WAIVED,
         stream_code=STREAM_CODE,
         total_amount=TOTAL_AMOUNT,
         initial_amount=INITIAL_AMOUNT,
         estimated_bill_amount=ESTIMATED_BILL_AMOUNT,
         adjustment_amount=ADJUSTMENT_AMOUNT,
         transaction_id=TRANSACTION_ID,
         date_created=CREATED,
         date_updated=UPDATED)

payment_arrangement_info <- read_csv(file=paste0(data_dir, "/CO00210T.csv"))

payment_arrangement_info <- payment_arrangement_info %>%
  rename(payment_plan_id=PAYMENT_PLAN_ID,
         due_date=DUE_DATE,
         amount=AMOUNT,
         transaction_id=TRANSACTION_ID,
         last_pay_date=LAST_PAY_DATE,
         outstanding_amount=OUTSTANDING_AMOUNT,
         date_created=CREATED,
         date_updated=UPDATED)

# Collections
collection_info <- read_csv(file=paste0(data_dir, "/CO00400T.csv"))

collection_info <- collection_info %>%
  rename(subsystem_id=SUBSYSTEM_ID,
         tu_id=TU_ID,
         sent_date=SENT_DATE,
         sent_time=SENT_TIME,
         status_code=STATUS_CODE,
         date_created=CREATED,
         date_updated=UPDATED)

collection_amount <- read_csv(file=paste0(data_dir, "/CO00450T.csv"))

collection_amount <- collection_amount %>%
  rename(subsystem_id=SUBSYSTEM_ID,
         tu_id=TU_ID,
         sent_date=SENT_DATE,
         sent_time=SENT_TIME,
         audit_or_live=AUDIT_OR_LIVE,
         cycle_code=CYCLE_CODE,
         final_notices=FINAL_NOTICES,
         last_payment_date=LAST_PAYMENT_DATE,
         last_bill_date=LAST_BILL_DATE,
         is_pending_cash=IS_PENDING_CASH,
         last_payment=LAST_PAYMENT,
         cut_on_date=CUT_ON_DATE,
         amount_due=AMOUNT_DUE,
         collection_amount=COLLECTION_AMOUNT,
         collection_date=COLLECTION_DATE,
         total_deposit_amount=TOTAL_DEPOSIT_AMOUNT,
         date_created=CREATED,
         date_updated=UPDATED)

# Usage info
usage_info <-
  force_bind(read_csv((list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
                         str_subset("UM00262T"))[1]) %>%
               rename(tu_id=TU_ID,
                      bill_run_date=BILL_RUN_DATE,
                      bill_run_time=BILL_RUN_TIME,
                      service_seq=SERVICE_SEQ,
                      detail_type=DETAIL_TYPE,
                      detail_seq=DETAIL_SEQ,
                      bill_code=BILL_CODE,
                      bc_detail_type=BC_DETAIL_TYPE,
                      bc_detail_seq=BC_DETAIL_SEQ,
                      code_1=CODE_1,
                      code_2=CODE_2,
                      code_3=CODE_3,
                      code_4=CODE_4,
                      num_1=NUM_1,
                      num_2=NUM_2,
                      num_3=NUM_3,
                      num_4=NUM_4,
                      num_5=NUM_5,
                      level_from=LEVEL_FROM,
                      level_to=LEVEL_TO,
                      cons_level_amount=CONS_LEVEL_AMOUNT,
                      bc_detail_rate=BC_DETAIL_RATE,
                      bc_detail_amount=BC_DETAIL_AMOUNT,
                      is_bc_detail_prorated=IS_BC_DETAIL_PRORATED,
                      bc_active_days=BC_ACTIVE_DAYS,
                      bc_standard_days=BC_STANDARD_DAYS,
                      item_number=ITEM_NUMBER,
                      report_context=REPORT_CONTEXT,
                      report_sub_context=REPORT_SUB_CONTEXT,
                      start_date=START_DATE,
                      end_date=END_DATE,
                      rate_active_date=RATE_ACTIVE_DATE,
                      start_1_date=START_1_DATE,
                      end_1_date=END_1_DATE,
                      start_2_date=START_2_DATE,
                      end_2_date=END_2_DATE,
                      date_created=CREATED,
                      date_updated=UPDATED),
             (list.files(data_dir, recursive=TRUE, full.names=TRUE) %>% 
                str_subset("UM00262T"))[-1] %>%
               lapply(read_csv, 
                      col_names=FALSE) %>%
               bind_rows)

# Code info
code_info <- read_csv(file=paste0(data_dir, "/AR50100C.csv"))

code_info <- code_info %>%
  rename(subsystem_id=SUBSYSTEM_ID,
         transaction_type=TRANSACTION_TYPE,
         description=DESCRIPTION,
         short_description=SHORT_DESCRIPTION,
         default_status=DEFAULT_STATUS,
         default_category=DEFAULT_CATEGORY,
         default_summary=DEFAULT_SUMMARY,
         default_amount=DEFAULT_AMOUNT,
         is_as_payment=IS_AS_PAYMENT,
         date_created=CREATED,
         date_updated=UPDATED)


# Save ####
save(account_info, address_info,
     location_relation, location_account_relation,
     bill_info, financial_assist, financial_assist_detail,
     cutoff_info,
     payment_arrangement, payment_arrangement_info,
     collection_info, collection_amount,
     code_info,
     file=paste0(working_data_dir, "/analysis_info.RData"))

save(financial_info, usage_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info_large.RData.gz")))

save(code_info,
     file=paste0(working_data_dir, "/code_info.RData"))
