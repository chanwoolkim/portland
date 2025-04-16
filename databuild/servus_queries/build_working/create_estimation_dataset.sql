CREATE OR REPLACE TABLE `servus-291816.portland_working.estimation_dataset` AS
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
    AND account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`)
  ORDER BY account_number DESC
),
water_usage_info AS (
  SELECT DISTINCT
    account_number,
    bill_run_date,
    SUM(CASE WHEN report_context = 'WCONS' THEN cons_level_amount END) AS water_consumption,
    SUM(bc_detail_amount) AS usage_bill
  FROM usage_info
  GROUP BY account_number, bill_run_date
),
bill_info AS (
  SELECT
    account_number,
    bill_date,
    next_bill_date,
    due_date,
    start_date,
    end_date,
    previous_bill_amount,
    previous_unpaid_amount,
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
  WHERE account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`)
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
    account_rct.account_number,
    account_rct.discount_percentage,
    account_rct.cycle_code,
    account_rct.linc_tier_type,
    account_rct.delinquent,
    account_rct.etie,
    account_rct.credit_score,
    account_rct.exit_reason,
    account_financial.ufh_status,
    account_financial.fa_status,
    account_financial.median_status,
    account_financial.income_quartile,
    account_financial.credit_quartile,
    census_data.total_house_holds,
    census_data.percent_of_individuals_above_sixteen_in_labor_force,
    census_data.unemployment_rate_in_labor_force,
    census_data.median_house_hold_income,
    census_data.average_house_hold_income,
    census_data.average_house_hold_earnings,
    census_data.percent_of_house_holds_with_retirement_income,
    census_data.average_house_hold_retirement_income,
    census_data.percent_of_house_holds_with_ssi,
    census_data.percent_of_house_holds_with_cash_assistance,
    census_data.average_house_hold_cash_assistance,
    census_data.percent_of_house_holds_with_food_stamps,
    census_data.percent_of_house_holds_in_poverty,
    census_data.percent_of_population_with_no_health_insurance,
    census_data.average_house_hold_size,
    census_data.average_house_hold_size_owner,
    census_data.average_house_hold_size_renter,
    census_data.median_number_of_rooms,
    census_data.percent_of_house_hold_owning,
    census_data.percent_of_house_hold_renting,
    census_data.housing_cost_renter,
    census_data.housing_cost_owner,
    census_data.median_housing_market_value,
    census_data.percent_vehicles_owned_0,
    census_data.percent_vehicles_owned_1,
    census_data.percent_vehicles_owned_2,
    census_data.percent_vehicles_owned_more_than_3,
    census_data.median_age,
    census_data.percent_age_under_5,
    census_data.percent_age_5_to_9,
    census_data.percent_age_10_to_14,
    census_data.percent_age_15_to_19,
    census_data.percent_age_20_to_24,
    census_data.percent_age_25_to_34,
    census_data.percent_age_35_to_44,
    census_data.percent_age_45_to_54,
    census_data.percent_age_55_to_59,
    census_data.percent_age_60_to_64,
    census_data.percent_age_65_to_74,
    census_data.percent_age_75_to_84,
    census_data.percent_age_over_85,
    census_data.male_to_female_ratio,
    census_data.percent_of_population_includes_native_american,
    census_data.percent_of_population_includes_hispanic,
    census_data.percent_of_population_includes_asian,
    census_data.percent_of_population_includes_pacific_islander,
    census_data.percent_of_population_includes_black,
    census_data.percent_of_population_includes_white,
    census_data.percent_of_population_includes_other_race
  FROM `servus-291816.portland_working.account_rct` AS account_rct
  LEFT JOIN `servus-291816.portland_working.account` AS account
    ON account_rct.account_number = account.account_number
  LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
    ON account_rct.account_number = account_financial.account_number
  LEFT JOIN `servus-291816`.`portlandWater`.`census_data` AS census_data
    ON account.census_id = census_data.census_id
)
SELECT DISTINCT
  bill_info.account_number,
  bill_info.bill_date,
  bill_info.start_date,
  bill_info.end_date,
  bill_info.due_date,
  bill_info.previous_bill_amount,
  bill_info.previous_unpaid_amount,
  bill_info.amount_billed,
  bill_info.amount_trans_billed,
  bill_info.fees,
  bill_info.amount_paid,
  bill_info.cleanriver_discount,
  bill_info.rct_discount,
  bill_info.linc_discount,
  bill_info.discount,
  bill_info.liens,
  bill_info.writeoff,
  bill_info.refund,
  bill_info.transfer,
  bill_info.running_owed,
  cutoff_action.cutoff_date,
  reconnect_action.reconnect_date,
  COALESCE(bill_to_join.is_rebill, FALSE) AS is_rebill,
  COALESCE(bill_to_join.type_code, 'REGLR') AS type_code,
  water_usage_info.water_consumption,
  water_usage_info.usage_bill,
  discount_percentage,
  cycle_code,
  linc_tier_type,
  delinquent,
  etie,
  credit_score,
  exit_reason,
  ufh_status,
  fa_status,
  median_status,
  income_quartile,
  credit_quartile,
  on_payment_plan.payment_plan,
 COALESCE(payment_plan_amount, 0) AS payment_plan_amount,
  COALESCE((bill_to_join.source_code = 'QB1'), FALSE) AS monthly_payment,
  total_house_holds,
  percent_of_individuals_above_sixteen_in_labor_force,
  unemployment_rate_in_labor_force,
  median_house_hold_income,
  average_house_hold_income,
  average_house_hold_earnings,
  percent_of_house_holds_with_retirement_income,
  average_house_hold_retirement_income,
  percent_of_house_holds_with_ssi,
  percent_of_house_holds_with_cash_assistance,
  average_house_hold_cash_assistance,
  percent_of_house_holds_with_food_stamps,
  percent_of_house_holds_in_poverty,
  percent_of_population_with_no_health_insurance,
  average_house_hold_size,
  average_house_hold_size_owner,
  average_house_hold_size_renter,
  median_number_of_rooms,
  percent_of_house_hold_owning,
  percent_of_house_hold_renting,
  housing_cost_renter,
  housing_cost_owner,
  median_housing_market_value,
  percent_vehicles_owned_0,
  percent_vehicles_owned_1,
  percent_vehicles_owned_2,
  percent_vehicles_owned_more_than_3,
  median_age,
  percent_age_under_5,
  percent_age_5_to_9,
  percent_age_10_to_14,
  percent_age_15_to_19,
  percent_age_20_to_24,
  percent_age_25_to_34,
  percent_age_35_to_44,
  percent_age_45_to_54,
  percent_age_55_to_59,
  percent_age_60_to_64,
  percent_age_65_to_74,
  percent_age_75_to_84,
  percent_age_over_85,
  male_to_female_ratio,
  percent_of_population_includes_native_american,
  percent_of_population_includes_hispanic,
  percent_of_population_includes_asian,
  percent_of_population_includes_pacific_islander,
  percent_of_population_includes_black,
  percent_of_population_includes_white,
  percent_of_population_includes_other_race
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
