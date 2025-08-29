SELECT 
  account_financial.ufh_status,
  account_financial.fa_status,
  account_financial.median_status,
  account_financial.credit_quartile,
  count(distinct transaction.tu_id) AS n
FROM transaction
LEFT JOIN account_financial
  ON transaction.tu_id = account_financial.tu_id
WHERE (transaction_date BETWEEN '2024-10-01' AND '2024-12-31')
  -- OPTIONS_PLACEHOLDER
GROUP BY 1, 2, 3, 4
ORDER BY 1, 2, 3, 4 DESC;