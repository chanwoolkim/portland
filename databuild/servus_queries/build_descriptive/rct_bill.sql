SELECT
  *
FROM billed_paid
WHERE account_id IN (SELECT account_id FROM account_rct)
  AND bill_date >= '2024-09-11'
