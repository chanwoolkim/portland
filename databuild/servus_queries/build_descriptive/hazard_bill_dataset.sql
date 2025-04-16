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
)
SELECT
  *
  FROM `servus-291816.portland_working.billed_paid` AS bill
  INNER JOIN new_delinquent
    ON bill.account_number = new_delinquent.account_number
    AND bill.bill_date = new_delinquent.bill_date;