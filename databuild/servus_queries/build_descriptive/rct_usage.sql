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
FROM `servus-291816.portland_working.usage`
WHERE account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`)
  AND bill_run_date >= '2024-09-11'
ORDER BY account_number, bill_run_date;
