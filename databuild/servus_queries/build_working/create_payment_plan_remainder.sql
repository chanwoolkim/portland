WITH aggregated_data AS (
    SELECT 
        payment_plan_id,
        EXTRACT(YEAR FROM due_date) AS year,
        EXTRACT(QUARTER FROM due_date) AS quarter,
        SUM(amount) AS total_amount,
        SUM(outstanding_amount) AS total_outstanding_amount
    FROM payment_plan_transaction
    GROUP BY payment_plan_id, year, quarter
),
cumulative_data AS (
    SELECT 
        a.payment_plan_id,
        a.year,
        a.quarter,
        a.total_amount,
        a.total_outstanding_amount,
        (SELECT COALESCE(SUM(b.total_amount), 0) 
         FROM aggregated_data b 
         WHERE b.payment_plan_id = a.payment_plan_id 
           AND (b.year > a.year OR (b.year = a.year AND b.quarter > a.quarter))
        ) AS remaining_amount,
        (SELECT COALESCE(SUM(b.total_outstanding_amount), 0) 
         FROM aggregated_data b 
         WHERE b.payment_plan_id = a.payment_plan_id 
           AND (b.year > a.year OR (b.year = a.year AND b.quarter > a.quarter))
        ) AS remaining_outstanding_amount
    FROM aggregated_data a
),
modified_data AS (
  SELECT 
      payment_plan_id,
      year,
      quarter,
      total_amount,
      total_outstanding_amount,
      remaining_amount,
      remaining_outstanding_amount
  FROM cumulative_data
),
payment_plan_data AS (
    SELECT
        account_id,
        payment_plan_id,
        start_date,
        EXTRACT(YEAR FROM start_date) AS year,
        EXTRACT(QUARTER FROM start_date) AS quarter,
        end_date,
        EXTRACT(YEAR FROM end_date) AS end_year,
        EXTRACT(QUARTER FROM end_date) AS end_quarter,
        status_code,
        total_amount
    FROM payment_plan
)
SELECT 
    m.*,
    p.account_id,
    p.status_code,
    p.start_date,
    p.end_date,
    p.total_amount AS payment_plan_total_amount
FROM modified_data m
LEFT JOIN payment_plan_data p
    ON m.payment_plan_id = p.payment_plan_id
    AND m.year = p.year
    AND m.quarter = p.quarter
WHERE (m.year < p.end_year) OR (m.year = p.end_year AND m.quarter < p.end_quarter) OR (m.year = p.end_year AND m.quarter = p.end_quarter AND status_code != 'T')
AND (status_code != 'N');