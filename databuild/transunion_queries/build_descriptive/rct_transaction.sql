WITH rct_bill AS (
  SELECT
  *
  FROM `servus-291816.portland_working.billed_paid`
  WHERE account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`)
  AND bill_date BETWEEN '2024-12-12' AND '2025-03-14'
)
SELECT
  transaction.account_number,
  transaction_date,
  CASE
      WHEN transaction_code = 'ADJ' THEN 'discount'
      WHEN transaction_code = 'BILLV' AND source_reference LIKE '%ADJ%' THEN 'adjustment'
      ELSE transaction_category
    END AS transaction_category,
  transaction_type,
  amount
  FROM `servus-291816.portland_working.transaction` AS transaction
  INNER JOIN rct_bill
    ON transaction.account_number = rct_bill.account_number
    AND transaction_date > rct_bill.bill_date
    AND transaction_date < rct_bill.next_bill_date
    AND NOT transaction_type LIKE '%COMMIT%';