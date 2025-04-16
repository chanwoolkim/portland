CREATE OR REPLACE TABLE `servus-291816.portland_working.payment_plan` AS
WITH ranked_payment_plan AS (
  SELECT
    payment_plan_id,
    account_number,
    status_code,
    stream_code,
    start_date,
    end_date,
    total_amount,
    initial_amount,
    updated,
    ROW_NUMBER() OVER (PARTITION BY payment_plan_id ORDER BY updated DESC) AS row_num
  FROM `servus-291816.portlandWater.payment_plan`
)
SELECT
    payment_plan_id,
    account_number,
    status_code,
    stream_code,
    start_date,
    end_date,
    total_amount,
    initial_amount,
    updated
FROM ranked_payment_plan
WHERE row_num = 1 AND account_number IN (SELECT account_number FROM `servus-291816.portland_working.account`)
ORDER BY payment_plan_id DESC;