CREATE OR REPLACE TABLE `servus-291816.portland_working.account_financial` AS
WITH financial_status AS (
  SELECT DISTINCT
    transaction.account_number,
    (financial_income.etie*1000*1.037 < 65331) AS ufh_status,
    (financial_income.etie*1000*1.037 <= 70800) AS fa_status,
    (financial_income.etie*1000*1.037 <= 116900) AS median_status,
  FROM
    `servus-291816.portlandWater.transaction` AS transaction
  LEFT JOIN `servus-291816.portland_auxiliary.financial_income` AS financial_income
  ON transaction.transaction_id = financial_income.transaction_number
  WHERE financial_income.etie IS NOT NULL
  AND transaction.transaction_date >= '2024-01-01'
)
SELECT
  financial_status.account_number,
  ufh_status,
  fa_status,
  median_status,
  account_quartile.income_quartile,
  account_quartile.credit_quartile
FROM financial_status
INNER JOIN `servus-291816.portland_working.account_quartile` AS account_quartile
ON financial_status.account_number = account_quartile.account_number
WHERE income_quartile != 'NA' AND credit_quartile != 'NA';