SELECT 
  account_id,
  location_id,
  bill_date,
  is_final,
  net_bill_amount,
  linc_tier_type,
  linc_effective_date,
  linc_expiry_date,
  is_honored_citizen,
  updated
FROM (
  SELECT 
    *,
    ROW_NUMBER() OVER (
      PARTITION BY 
        account_id,
        location_id,
        bill_date
      ORDER BY updated
    ) AS row_num
  FROM linc
)
WHERE row_num = 1;
