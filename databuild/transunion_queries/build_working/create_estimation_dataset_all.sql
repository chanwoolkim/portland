CREATE OR REPLACE TABLE `servus-291816.portland_working.estimation_dataset_all` AS
WITH ranked_usage AS (
  SELECT
    account_number,
    item_number,
    bill_run_date,
    location_id,
    bill_code,
    cons_level_amount,
    bc_detail_rate,
    bc_detail_amount,
    is_bc_detail_prorated,
    bc_active_days,
    bc_standard_days,
    report_context,
    report_sub_context,
    start_date,
    end_date,
    updated,
    ROW_NUMBER() OVER (PARTITION BY item_number ORDER BY updated DESC) AS row_num
  FROM `servus-291816.portlandWater.usage`
),
usage_info AS (
  SELECT
    account_number,
    bill_run_date,
    location_id,
    bill_code,
    cons_level_amount,
    bc_detail_rate,
    bc_detail_amount,
    is_bc_detail_prorated,
    bc_active_days,
    bc_standard_days,
    report_context,
    report_sub_context,
    start_date,
    end_date,
    updated
  FROM ranked_usage
  WHERE row_num = 1
  ORDER BY account_number DESC
),
water_usage_info AS (
  SELECT DISTINCT
    account_number,
    bill_run_date,
    AVG(CASE WHEN report_context = 'WCONS' THEN cons_level_amount END) AS water_consumption,
    SUM(bc_detail_amount) AS usage_bill
  FROM usage_info
  GROUP BY account_number, bill_run_date
),
bill_info AS (
  SELECT
    account_number,
    bill_date,
    EXTRACT(YEAR FROM bill_date) AS year,
    EXTRACT(QUARTER FROM bill_date) AS quarter,
    CASE 
      WHEN EXTRACT(YEAR FROM bill_date) > 2023 THEN 2023
      ELSE EXTRACT(YEAR FROM bill_date)
    END AS census_year,
    next_bill_date,
    due_date,
    start_date,
    end_date,
    previous_bill_amount,
    previous_unpaid_amount,
    current_amount,
    amount_billed,
    amount_trans_billed,
    fees,
    amount_paid,
    cleanriver_discount,
    rct_discount,
    linc_discount,
    discount,
    liens,
    writeoff,
    refund,
    transfer,
    running_owed
  FROM `servus-291816.portland_working.billed_paid`
),
on_payment_plan AS (
  SELECT DISTINCT
    payment_plan.account_number,
    bill_date,
    TRUE AS payment_plan
  FROM `servus-291816.portland_working.payment_plan` AS payment_plan
  INNER JOIN bill_info
  ON bill_info.account_number = payment_plan.account_number
    AND payment_plan.start_date <= bill_info.next_bill_date
    AND payment_plan.end_date >= bill_info.bill_date
),
payment_plan_info AS (
  SELECT
    billed_paid.account_number,
    billed_paid.bill_date,
    SUM(payment_plan_transaction.amount) AS payment_plan_amount
  FROM `servus-291816.portland_working.billed_paid` AS billed_paid
  INNER JOIN `servus-291816.portland_working.payment_plan_transaction` AS payment_plan_transaction
  ON billed_paid.account_number = payment_plan_transaction.account_number
  WHERE payment_plan_transaction.action_date >= billed_paid.next_bill_date
    AND payment_plan_transaction.end_date >= billed_paid.next_bill_date
    AND payment_plan_transaction.start_date < billed_paid.next_bill_date
  GROUP BY billed_paid.account_number, billed_paid.bill_date
),
account_consolidated AS (
  SELECT
    account.account_number,
    account.person_id,
    account.location_id,
    account.cycle_code,
    account.state_id,
    account.county_id,
    account.tract_id,
    account.block_group_id,
    account.occupancy_status,
    account.age,
    account.estimated_current_home_value,
    account.income,
    account.household_size,
    account.adult_count,
    account.child_count,
    account.service_water,
    account.service_sewer,
    account.service_storm,
    account_financial.ufh_status,
    account_financial.fa_status,
    account_financial.median_status,
    account_financial.credit_quartile,
    census_data.*
    EXCEPT(census_year, state_id, county_id, tract_id, block_group_id),
    CAST(census_data.census_year AS int64) AS census_year
  FROM `servus-291816.portland_working.account` AS account
  LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
    ON account.account_number = account_financial.account_number
  LEFT JOIN `servus-291816.census_data.census_data` AS census_data
    ON account.state_id = census_data.state_id
    AND account.county_id = census_data.county_id
    AND account.tract_id = census_data.tract_id
    AND account.block_group_id = census_data.block_group_id
    AND census_data.census_year = '2023'
)
SELECT DISTINCT
  bill_info.*
  EXCEPT(census_year),
  cutoff_action.cutoff_date,
  reconnect_action.reconnect_date,
  COALESCE(bill_to_join.is_rebill, FALSE) AS is_rebill,
  COALESCE(bill_to_join.type_code, 'REGLR') AS type_code,
  water_usage_info.water_consumption,
  water_usage_info.usage_bill,
  account_consolidated.*
  EXCEPT(account_number),
  account_income.income AS tu_income,
  account_income.credit_score AS tu_credit_score,
  on_payment_plan.payment_plan,
  COALESCE(payment_plan_amount, 0) AS payment_plan_amount,
  COALESCE((bill_to_join.source_code = 'QB1'), FALSE) AS monthly_payment
  FROM bill_info
  LEFT JOIN water_usage_info
    ON bill_info.bill_date = water_usage_info.bill_run_date
    AND bill_info.account_number = water_usage_info.account_number
  LEFT JOIN account_consolidated
    ON bill_info.account_number = account_consolidated.account_number
  LEFT JOIN on_payment_plan
    ON bill_info.account_number = on_payment_plan.account_number
    AND bill_info.bill_date = on_payment_plan.bill_date
  LEFT JOIN payment_plan_info
    ON bill_info.account_number = payment_plan_info.account_number
    AND bill_info.bill_date = payment_plan_info.bill_date
  LEFT JOIN (
    SELECT 
      account_number,
      bill_date,
      source_code,
      type_code,
      is_rebill,
      ROW_NUMBER() OVER (PARTITION BY account_number, bill_date ORDER BY updated DESC) AS bill_num
    FROM `servus-291816.portlandWater.bill` AS bill
    WHERE (bill.type_code = 'REGLR' OR bill.type_code = 'FINAL')
    AND bill.is_canceled = FALSE
    AND bill.is_error = FALSE
    AND bill.is_voided = FALSE
    AND bill.is_corrected = FALSE
    AND bill.audit_or_live = 'L'
    AND bill.start_date IS NOT NULL
    AND bill.end_date IS NOT NULL
    AND bill.due_date IS NOT NULL
  ) AS bill_to_join
    ON bill_info.account_number = bill_to_join.account_number 
    AND bill_info.bill_date = bill_to_join.bill_date
    AND bill_to_join.bill_num = 1
  LEFT JOIN `servus-291816.portland_working.account_income` AS account_income
    ON bill_info.account_number = account_income.account_number
    AND bill_info.year = account_income.year
    AND bill_info.quarter = account_income.quarter
  LEFT JOIN (
    SELECT
      account_number,
      effective_date AS cutoff_date
    FROM `servus-291816.portland_working.action` AS action
    WHERE action_code = 'CUTOF'
  ) AS cutoff_action
    ON bill_info.account_number = cutoff_action.account_number
    AND bill_info.bill_date <= cutoff_action.cutoff_date
    AND bill_info.next_bill_date > cutoff_action.cutoff_date
  LEFT JOIN (
    SELECT
      account_number,
      effective_date AS reconnect_date
    FROM `servus-291816.portland_working.action` AS action
    WHERE action_code = 'RCNCT'
  ) AS reconnect_action
    ON bill_info.account_number = reconnect_action.account_number
    AND bill_info.bill_date <= reconnect_action.reconnect_date
    AND bill_info.next_bill_date > reconnect_action.reconnect_date;
