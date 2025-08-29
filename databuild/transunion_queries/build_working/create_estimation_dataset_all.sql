WITH ranked_usage AS (
  SELECT
    tu_id,
    item_number,
    bill_run_date,
    bill_code,
    cons_level_amount,
    bc_detail_rate,
    bc_detail_amount,
    is_bc_detail_prorated,
    bc_active_days,
    bc_standard_days,
    TRIM(report_context) AS report_context,
    report_sub_context,
    start_date,
    end_date,
    updated,
    ROW_NUMBER() OVER (PARTITION BY item_number ORDER BY updated DESC) AS row_num
  FROM usage
),
usage_info AS (
  SELECT
    tu_id,
    bill_run_date,
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
  ORDER BY tu_id DESC
),
water_usage_info AS (
  SELECT DISTINCT
    tu_id,
    bill_run_date,
    AVG(CASE WHEN report_context = 'WCONS' THEN cons_level_amount END) AS water_consumption,
    SUM(bc_detail_amount) AS usage_bill
  FROM usage_info
  GROUP BY tu_id, bill_run_date
),
bill_info AS (
  SELECT
    tu_id,
    bill_date,
    EXTRACT(YEAR FROM CAST(bill_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(bill_date AS DATE)) AS quarter,
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
    adjustment,
    liens,
    writeoff,
    refund,
    transfer,
    running_owed
  FROM billed_paid
),
on_payment_plan AS (
  SELECT DISTINCT
    payment_plan.tu_id,
    bill_date,
    TRUE AS payment_plan
  FROM payment_plan
  INNER JOIN bill_info
  ON bill_info.tu_id = payment_plan.tu_id
    AND payment_plan.start_date <= bill_info.next_bill_date
    AND payment_plan.end_date >= bill_info.bill_date
),
payment_plan_info AS (
  SELECT
    billed_paid.tu_id,
    billed_paid.bill_date,
    SUM(payment_plan_transaction.amount) AS payment_plan_amount
  FROM billed_paid
  INNER JOIN payment_plan_transaction
  ON billed_paid.tu_id = payment_plan_transaction.tu_id
  WHERE payment_plan_transaction.action_date >= billed_paid.next_bill_date
    AND payment_plan_transaction.end_date >= billed_paid.next_bill_date
    AND payment_plan_transaction.start_date < billed_paid.next_bill_date
  GROUP BY billed_paid.tu_id, billed_paid.bill_date
),
linc_info AS (
  SELECT
    billed_paid.tu_id,
    billed_paid.bill_date,
    linc.linc_tier_type AS linc_tier_type_at_bill,
    linc.is_honored_citizen
  FROM billed_paid
  INNER JOIN linc
  ON billed_paid.tu_id = linc.tu_id
  WHERE linc.bill_date >= linc.linc_effective_date
    AND linc.bill_date <= linc.linc_expiry_date
),
account_income AS (
  SELECT
    tu_id,
    EXTRACT(YEAR FROM CAST(credit_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(credit_date AS DATE)) AS quarter,
    AVG(etie) AS income,
    AVG(credit_score) AS credit_score
  FROM tu_data
  GROUP BY tu_id, year, quarter
),
account_consolidated AS (
  SELECT
    account.tu_id,
    account.cycle_code,
    account.service_water,
    account.service_sewer,
    account.service_storm,
    account_financial.*,
    census_data.*
  FROM account
  LEFT JOIN account_financial
    ON account.tu_id = account_financial.tu_id
  LEFT JOIN census_data
    ON account.tract_id = census_data.tract_id
    AND census_data.census_year = '2023'
)
SELECT DISTINCT
  bill_info.*,
  cutoff_action.cutoff_date,
  reconnect_action.reconnect_date,
  COALESCE((bill_to_join.is_rebill = 'T'), FALSE) AS is_rebill,
  COALESCE(bill_to_join.type_code, 'REGLR') AS type_code,
  water_usage_info.water_consumption,
  water_usage_info.usage_bill,
  account_consolidated.*,
  account_income.income AS tu_income,
  account_income.credit_score AS tu_credit_score,
  on_payment_plan.payment_plan,
  COALESCE(payment_plan_amount, 0) AS payment_plan_amount,
  COALESCE((bill_to_join.source_code = 'QB1'), FALSE) AS monthly_payment,
  linc_tier_type_at_bill,
  is_honored_citizen
  FROM bill_info
  LEFT JOIN water_usage_info
    ON bill_info.bill_date = water_usage_info.bill_run_date
    AND bill_info.tu_id = water_usage_info.tu_id
  LEFT JOIN account_consolidated
    ON bill_info.tu_id = account_consolidated.tu_id
  LEFT JOIN on_payment_plan
    ON bill_info.tu_id = on_payment_plan.tu_id
    AND bill_info.bill_date = on_payment_plan.bill_date
  LEFT JOIN payment_plan_info
    ON bill_info.tu_id = payment_plan_info.tu_id
    AND bill_info.bill_date = payment_plan_info.bill_date
  LEFT JOIN linc_info
    ON bill_info.tu_id = linc_info.tu_id
    AND bill_info.bill_date = linc_info.bill_date
  LEFT JOIN (
    SELECT 
      tu_id,
      bill_date,
      source_code,
      type_code,
      is_rebill,
      ROW_NUMBER() OVER (PARTITION BY tu_id, bill_date ORDER BY updated DESC) AS bill_num
    FROM bill
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
    ON bill_info.tu_id = bill_to_join.tu_id 
    AND bill_info.bill_date = bill_to_join.bill_date
    AND bill_to_join.bill_num = 1
  LEFT JOIN account_income
    ON bill_info.tu_id = account_income.tu_id
    AND bill_info.year = account_income.year
    AND bill_info.quarter = account_income.quarter
  LEFT JOIN (
    SELECT
      tu_id,
      effective_date AS cutoff_date
    FROM action
    WHERE action_code = 'CUTOF'
  ) AS cutoff_action
    ON bill_info.tu_id = cutoff_action.tu_id
    AND bill_info.bill_date <= cutoff_action.cutoff_date
    AND bill_info.next_bill_date > cutoff_action.cutoff_date
  LEFT JOIN (
    SELECT
      tu_id,
      effective_date AS reconnect_date
    FROM action
    WHERE action_code = 'RCNCT'
  ) AS reconnect_action
    ON bill_info.tu_id = reconnect_action.tu_id
    AND bill_info.bill_date <= reconnect_action.reconnect_date
    AND bill_info.next_bill_date > reconnect_action.reconnect_date;
