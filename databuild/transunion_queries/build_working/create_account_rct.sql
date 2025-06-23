WITH rct_accounts AS (
  SELECT
    tu_id,
    discount_percentage,
    linc_tier_type,
    delinquent
  FROM discount
),
latest_tu_info AS (
  SELECT
    tu_id,
    etie,
    credit_score
  FROM tu_data
  WHERE etie IS NOT NULL
  QUALIFY
    ROW_NUMBER() OVER (PARTITION BY tu_id ORDER BY credit_date DESC) = 1
)
SELECT DISTINCT
  rct_accounts.tu_id,
  discount_percentage,
  cycle_code,
  linc_tier_type,
  delinquent,
  etie,
  credit_score,
  billing_code,
  reason AS exit_reason
FROM rct_accounts
  LEFT JOIN account
  ON rct_accounts.tu_id = account.tu_id
  LEFT JOIN latest_tu_info
  ON rct_accounts.tu_id = latest_tu_info.tu_id
  LEFT JOIN rct_exclusions
  ON rct_accounts.tu_id = rct_exclusions.tu_id;
