WITH processed_transactions AS (
  SELECT
    EXTRACT(YEAR FROM CAST(transaction.transaction_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(transaction.transaction_date AS DATE)) AS quarter,
    SIGN(transaction.amount) AS transaction_amount_positive,
    account_financial.ufh_status,
    account_financial.fa_status,
    account_financial.median_status,
    account_financial.credit_quartile,
    transaction_type,
    transaction.amount,
    transaction.tu_id
  FROM transaction
  LEFT JOIN account
  ON account.tu_id = transaction.tu_id
  LEFT JOIN account_financial
  ON transaction.tu_id = account_financial.tu_id
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
  COUNT(DISTINCT tu_id) AS n
FROM processed_transactions
WHERE transaction_amount_positive != 0
GROUP BY 1, 2, 3, 4, 5, 6, 7, 8
ORDER BY 1, 2, 3, 4, 5, 6, 7, 8;