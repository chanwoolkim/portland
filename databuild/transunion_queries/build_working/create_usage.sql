SELECT
  tu_id,
  service_seq,
  detail_type,
  detail_seq,
  bill_code,
  bc_detail_type,
  bc_detail_seq,
  start_date,
  end_date
  FROM (
    SELECT 
      *,
      ROW_NUMBER() OVER (
        PARTITION BY 
          tu_id,
          service_seq,
          detail_type,
          detail_seq,
          bill_code,
          bc_detail_type,
          bc_detail_seq,
          start_date,
          end_date
        ORDER BY tu_id
      ) AS row_num
    FROM usage
  )
WHERE row_num = 1;
