WITH rct_bill AS (
  SELECT
  *
  FROM billed_paid
  WHERE tu_id IN (SELECT tu_id FROM account_rct)
  AND bill_date BETWEEN '2024-12-12' AND '2025-03-14'
)
SELECT
  transaction.tu_id,
  transaction_date,
  CASE
      WHEN transaction_code = 'ADJ' THEN 'discount'
      WHEN transaction_code = 'BILLV' AND source_reference LIKE '%ADJ%' THEN 'adjustment'
      ELSE transaction_category
    END AS transaction_category,
  transaction_type,
  amount
  FROM transaction
  INNER JOIN rct_bill
    ON transaction.tu_id = rct_bill.tu_id
    AND transaction_date > rct_bill.bill_date
    AND transaction_date < rct_bill.next_bill_date
    AND NOT transaction_type LIKE '%COMMIT%';