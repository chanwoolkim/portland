SELECT 
  account_financial.ufh_status,
  account_financial.fa_status,
  account_financial.median_status,
  account_financial.credit_quartile,
  count(distinct transaction.account_number) AS n
FROM `servus-291816.portlandWater.transaction` AS transaction
LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
  ON transaction.account_number = account_financial.account_number
WHERE (transaction_date BETWEEN "2024-10-01" AND "2024-12-31")
  -- OPTIONS_PLACEHOLDER
GROUP BY 1, 2, 3, 4
ORDER BY 1, 2, 3, 4 DESC;