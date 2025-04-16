WITH new_delinquent AS (
  SELECT DISTINCT 
    account_number,
    bill_date,
    amount_billed AS initial_amount
  FROM `servus-291816.portland_working.billed_paid` AS b1
  WHERE 
    bill_date BETWEEN '2023-01-01' AND '2023-03-31'
    AND previous_unpaid_amount <= 0
    AND amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer > 0
    AND NOT EXISTS (
      SELECT 1 
      FROM `servus-291816.portland_working.billed_paid` AS b2
      WHERE b2.account_number = b1.account_number
      AND b2.bill_date < b1.bill_date
      AND b2.previous_unpaid_amount > 0
    )
    AND type_code = 'REGLR'
), 
filtered_delinquent AS (
  SELECT
    new_delinquent.account_number,
    new_delinquent.bill_date,
    initial_amount
    FROM `servus-291816.portland_working.billed_paid` AS bill
  INNER JOIN new_delinquent
    ON bill.account_number = new_delinquent.account_number
    AND bill.bill_date = new_delinquent.bill_date
)
SELECT
  transaction.account_number,
  transaction_date,
  CASE
      WHEN transaction_code = 'ADJ' THEN 'discount'
      WHEN transaction_code = 'BILLV' AND source_reference LIKE '%ADJ%' THEN 'adjustment'
      ELSE transaction_category
    END AS transaction_category,
  amount
  FROM `servus-291816.portland_working.transaction` AS transaction
  INNER JOIN filtered_delinquent
    ON transaction.account_number = filtered_delinquent.account_number
    AND transaction_date > filtered_delinquent.bill_date
    AND NOT transaction_type LIKE '%COMMIT%';