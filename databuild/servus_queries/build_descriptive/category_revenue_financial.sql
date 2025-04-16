WITH processed_transactions AS (
  SELECT
    EXTRACT(YEAR FROM transaction.transaction_date) AS year,
    EXTRACT(QUARTER FROM transaction.transaction_date) AS quarter,
    SIGN(transaction.amount) AS transaction_amount_positive,
    account_financial.ufh_status,
    account_financial.fa_status,
    account_financial.median_status,
    account_financial.credit_quartile,
    transaction_type,
    transaction.amount,
    transaction.account_number
  FROM `servus-291816.portland_working.transaction` AS transaction
  LEFT JOIN `servus-291816.portland_working.account` AS account
  ON account.account_number = transaction.account_number
  LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
  ON transaction.account_number = account_financial.account_number
  WHERE (transaction_date BETWEEN '2019-01-01' AND '2025-01-31')
  AND ((account.billing_code != 'FINAL') OR (account.billing_code = 'FINAL' AND transaction_date <= account.last_bill_date))
)
SELECT
  year,
  quarter,
  transaction_type,
  transaction_amount_positive,
  ufh_status,
  fa_status,
  median_status,
  credit_quartile,
  SUM(amount) AS total_amount,
  COUNT(DISTINCT account_number) AS n
FROM processed_transactions
WHERE transaction_amount_positive != 0
GROUP BY 1, 2, 3, 4, 5, 6, 7, 8
ORDER BY 1, 2, 3, 4, 5, 6, 7, 8;