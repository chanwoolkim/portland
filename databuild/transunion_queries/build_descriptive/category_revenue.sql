WITH processed_transactions AS (
  SELECT
    EXTRACT(YEAR FROM CAST(transaction.transaction_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(transaction.transaction_date AS DATE)) AS quarter,
    SIGN(transaction.amount) AS transaction_amount_positive,
    transaction_type,
    transaction.amount,
    transaction.tu_id
  FROM transaction
  LEFT JOIN account
  ON account.tu_id = transaction.tu_id
  WHERE (transaction_date BETWEEN '2019-01-01' AND '2025-02-01')
  AND ((account.billing_code != 'FINAL') OR (account.billing_code = 'FINAL' AND transaction_date <= account.last_bill_date))
  -- OPTIONS_PLACEHOLDER
)
SELECT
  year,
  quarter,
  transaction_type,
  transaction_amount_positive,
  SUM(amount) AS total_amount,
  COUNT(DISTINCT tu_id) AS n
FROM processed_transactions
WHERE transaction_amount_positive != 0
GROUP BY 1, 2, 3, 4
ORDER BY 1, 2, 3, 4;