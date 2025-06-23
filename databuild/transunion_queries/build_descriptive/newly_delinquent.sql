WITH bill_modified AS (
  SELECT
    *
  FROM `servus-291816.portland_working.billed_paid`
  WHERE bill_date >= '2019-01-01'
),
new_delinquent AS (
  SELECT DISTINCT 
    b1.account_number,
    bill_date,
    amount_billed AS initial_amount,
    EXTRACT(YEAR FROM bill_date) AS year,
    EXTRACT(QUARTER FROM bill_date) AS quarter,
    amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer AS amount_owed
  FROM bill_modified AS b1
  WHERE 
    -- OPTIONS_PLACEHOLDER
    AND type_code = 'REGLR'
    AND previous_unpaid_amount <= 0
    AND amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer > 0
    AND NOT EXISTS (
      SELECT 1 
      FROM bill_modified AS b2
      WHERE b2.account_number = b1.account_number
      AND b2.bill_date < b1.bill_date
      AND b2.previous_unpaid_amount > 0
    )
),
all_account_count AS (
  SELECT
    COUNT(DISTINCT account_number) AS all_account_count,
    EXTRACT(YEAR FROM bill_date) AS year,
    EXTRACT(QUARTER FROM bill_date) AS quarter,
    AVG(amount_billed) AS average_amount_billed
  FROM bill_modified AS bill
  WHERE 
    -- OPTIONS_PLACEHOLDER
    AND type_code = 'REGLR'
  GROUP BY year, quarter
),
all_delinquent AS (
  SELECT
    COUNT(DISTINCT account_number) AS all_delinquent_count,
    EXTRACT(YEAR FROM bill_date) AS year,
    EXTRACT(QUARTER FROM bill_date) AS quarter,
    AVG(amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer) AS average_amount_delinquent
  FROM bill_modified AS bill
  WHERE 
    -- OPTIONS_PLACEHOLDER
    AND amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer > 0
    AND type_code = 'REGLR'
  GROUP BY year, quarter
),
aggregate_delinquent AS (
  SELECT
    COUNT(DISTINCT bill.account_number) AS new_delinquent_count,
    EXTRACT(YEAR FROM bill.bill_date) AS year,
    EXTRACT(QUARTER FROM bill.bill_date) AS quarter,
    AVG(new_delinquent.amount_owed) AS average_amount_owed
  FROM bill_modified AS bill
  INNER JOIN new_delinquent
    ON bill.account_number = new_delinquent.account_number
    AND bill.bill_date = new_delinquent.bill_date
  GROUP BY year, quarter
)
SELECT
  all_account_count.all_account_count,
  aggregate_delinquent.new_delinquent_count,
  SAFE_DIVIDE(aggregate_delinquent.new_delinquent_count, all_account_count.all_account_count) AS delinquency_rate,
  all_delinquent.all_delinquent_count,
  SAFE_DIVIDE(all_delinquent.all_delinquent_count, all_account_count.all_account_count) AS any_delinquency_rate,
  all_delinquent.average_amount_delinquent,
  aggregate_delinquent.average_amount_owed,
  all_account_count.average_amount_billed,
  all_account_count.year,
  all_account_count.quarter
FROM all_account_count
INNER JOIN aggregate_delinquent
  ON all_account_count.year = aggregate_delinquent.year
  AND all_account_count.quarter = aggregate_delinquent.quarter
INNER JOIN all_delinquent
  ON all_account_count.year = all_delinquent.year
  AND all_account_count.quarter = all_delinquent.quarter;
  