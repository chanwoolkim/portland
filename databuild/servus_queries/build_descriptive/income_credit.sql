SELECT DISTINCT
  year,
  quarter,
  account_number,
  etie,
  credit_score
FROM
  `servus-291816.portland_auxiliary.financial_income` AS financial_income
INNER JOIN
  `servus-291816.portland_working.transaction` AS transaction
ON
  financial_income.transaction_number = transaction.transaction_id;