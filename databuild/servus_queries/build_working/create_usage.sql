SELECT 
  account_id,
  item_number,
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
FROM (
  SELECT 
    *,
    ROW_NUMBER() OVER (
      PARTITION BY 
        account_id,
        location_id,
        service_seq,
        detail_type,
        detail_seq,
        bill_code,
        bc_detail_type,
        bc_detail_seq,
        start_date,
        end_date
      ORDER BY updated
    ) AS row_num
  FROM usage
)
WHERE row_num = 1;
