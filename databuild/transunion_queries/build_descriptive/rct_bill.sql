SELECT
  *
FROM billed_paid
WHERE tu_id IN (SELECT tu_id FROM account_rct)
  AND bill_date >= '2024-09-11'
