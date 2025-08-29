WITH rct_accounts AS (
  SELECT
    account_id,
    discount_percentage,
  FROM discount
  UNION ALL
  SELECT
      account_id,
      discount_percentage,
  FROM discount_addition
),
linc_type AS (
  SELECT DISTINCT
    account_id,
    linc_tier_type
  FROM linc
  WHERE bill_date BETWEEN '2024-04-01' AND '2024-06-30'
),
delinquent_at_random AS (
  SELECT DISTINCT
    account_id
  FROM billed_paid
  WHERE bill_date BETWEEN '2024-04-01' AND '2024-06-30'
    AND previous_unpaid_amount > 0
)
SELECT DISTINCT
  rct_accounts.account_id,
  discount_percentage,
  cycle_code,
  linc_tier_type,
  CASE
    WHEN rct_accounts.account_id IN (SELECT account_id FROM delinquent_at_random) THEN 'Delinquent'
    ELSE 'Not Delinquent'
  END AS delinquent,
  reason AS exit_reason
FROM rct_accounts
  LEFT JOIN rct_exclusion
    ON rct_accounts.account_id = rct_exclusion.account_id
  LEFT JOIN linc_type
    ON rct_accounts.account_id = linc_type.account_id
  LEFT JOIN account
    ON rct_accounts.account_id = account.account_id;
