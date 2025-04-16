CREATE OR REPLACE TABLE `servus-291816.portland_working.account_quartile` AS
SELECT DISTINCT
    transaction.account_number,
    financial_quartile.income_quartile,
    financial_quartile.credit_quartile
  FROM
    `servus-291816.portlandWater.transaction` AS transaction
    INNER JOIN `servus-291816.portland_auxiliary.financial_quartile` AS financial_quartile
     ON transaction.transaction_id = financial_quartile.transaction_number;