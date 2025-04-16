CREATE OR REPLACE TABLE `servus-291816.portland_working.usage` AS
  SELECT * EXCEPT(row_num)
  FROM (
    SELECT 
      *,
      ROW_NUMBER() OVER (
        PARTITION BY 
          account_number,
          location_id,
          service_seq,
          detail_type,
          detail_seq,
          bill_code,
          bc_detail_type,
          bc_detail_seq,
          start_date,
          end_date
        ORDER BY account_number
      ) AS row_num
    FROM `servus-291816.portlandWater.usage`
  )
  WHERE row_num = 1;
