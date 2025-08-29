SELECT
  account_id,
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
FROM usage
WHERE account_id IN (SELECT account_id FROM account_rct)
  AND bill_run_date >= '2024-09-11'
ORDER BY account_id, bill_run_date;
