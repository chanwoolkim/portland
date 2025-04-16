CREATE OR REPLACE TABLE `servus-291816.portland_working.account_income` AS
SELECT DISTINCT
    transaction.account_number,
    etie*250 AS income,
    EXTRACT(QUARTER FROM transaction_date) AS quarter,
    EXTRACT(YEAR FROM transaction_date) AS year
  FROM
    `servus-291816.portlandWater.transaction` AS transaction
    INNER JOIN `servus-291816.portland_auxiliary.financial_income` AS financial_income
     ON transaction.transaction_id = financial_income.transaction_number;