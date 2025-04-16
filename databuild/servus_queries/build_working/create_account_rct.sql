CREATE OR REPLACE TABLE `servus-291816.portland_working.account_rct` AS
WITH rct_accounts AS (
  SELECT
    account_number,
    discount_percentage,
  FROM
      `servus-291816.portlandWater.discount` AS discount
  UNION ALL
  SELECT
      account_number,
      discount_percentage,
  FROM
      `servus-291816.portlandWater.discount_addition` AS discount_addition
),
merged_transaction AS (
  SELECT
    transaction.account_number,
    transaction.transaction_date,
    ROUND(etie*1000*1.037, 0) AS etie,
    credit_score,
    reason
  FROM
    `servus-291816.portlandWater.transaction` AS TRANSACTION
  INNER JOIN
    `servus-291816.portland_auxiliary.financial_income` AS financial_income
  ON
    transaction.transaction_id = financial_income.transaction_number
  LEFT JOIN `servus-291816.portland_auxiliary.rct_exclusion` AS rct_exclusion
  ON
    transaction.transaction_id = rct_exclusion.transaction_number
),
rct_tu_info AS (
  SELECT
    account_number,
    etie,
    credit_score,
    reason,
    transaction_date
  FROM merged_transaction
  WHERE etie IS NOT NULL
  QUALIFY
    ROW_NUMBER() OVER (PARTITION BY account_number ORDER BY transaction_date DESC) = 1
),
rct_info AS (
  SELECT DISTINCT
    transaction.account_number,
    rct_info.linc_tier_type,
    rct_info.delinquent
  FROM
    `servus-291816.portlandWater.transaction` AS transaction
    INNER JOIN `servus-291816.portland_auxiliary.rct_info` AS rct_info
    ON transaction.transaction_id = rct_info.transaction_number
)
SELECT DISTINCT
  rct_accounts.account_number,
  discount_percentage,
  cycle_code,
  linc_tier_type,
  delinquent,
  etie,
  credit_score,
  account.billing_code,
  reason AS exit_reason
FROM rct_accounts
  LEFT JOIN rct_info
  ON rct_accounts.account_number = rct_info.account_number
  LEFT JOIN rct_tu_info
  ON rct_accounts.account_number = rct_tu_info.account_number
  LEFT JOIN `servus-291816.portland_working.account` AS account
  ON rct_accounts.account_number = account.account_number;
