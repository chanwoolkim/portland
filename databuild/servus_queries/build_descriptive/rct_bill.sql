SELECT
  *
FROM `servus-291816.portland_working.billed_paid`
WHERE account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`)
  AND bill_date >= '2024-09-11'
