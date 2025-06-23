SELECT DISTINCT
*,
  CASE
    WHEN default_category = 'ADJ' THEN 'discount'
    WHEN default_category = 'BILLB' THEN 'bill'
    WHEN default_category = 'BILLV' THEN 'bill'
    WHEN default_category = 'BNKRP' THEN 'writeoff'
    WHEN default_category = 'CONV' THEN 'bill'
    WHEN default_category = 'DONAT' THEN 'payment'
    WHEN default_category = 'DSCNT' AND description LIKE '%Clean River%' THEN 'cleanriver_discount'
    WHEN default_category = 'DSCNT' AND description LIKE '%Smart Discount%' THEN 'rct_discount'
    WHEN default_category = 'DSCNT' AND description LIKE '%Low Income%' THEN 'linc_discount'
    WHEN default_category = 'DSCNT' THEN 'discount'
    WHEN default_category = 'FEES' THEN 'fees'
    WHEN default_category = 'FEESP' THEN 'fees'
    WHEN default_category = 'LIENS' THEN 'liens'
    WHEN default_category = 'PYMNT' THEN 'payment'
    WHEN default_category = 'REFND' THEN 'refund'
    WHEN default_category = 'WRTOF' THEN 'writeoff'
    WHEN default_category = 'XFER' AND (default_summary LIKE '%PYMNT%' OR default_summary LIKE '%DONAT%') THEN 'payment' ELSE 'transfer'
  END AS category
FROM code;