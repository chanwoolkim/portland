SELECT
  account_financial.income_quartile,
  account_financial.credit_quartile,
  count(distinct transaction.account_number) AS n
FROM `servus-291816.portlandWater.transaction` AS transaction
LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
  ON transaction.account_number = account_financial.account_number
WHERE (transaction_date BETWEEN "2024-10-01" AND "2024-12-31") 
GROUP BY 1, 2
ORDER BY 1, 2 DESC;