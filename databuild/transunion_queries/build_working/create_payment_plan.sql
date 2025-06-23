WITH ranked_payment_plan AS (
  SELECT
    payment_plan_id,
    tu_id,
    status_code,
    stream_code,
    start_date,
    end_date,
    total_amount,
    initial_amount,
    updated,
    ROW_NUMBER() OVER (PARTITION BY payment_plan_id ORDER BY updated DESC) AS row_num
  FROM payment_plan
)
SELECT
    payment_plan_id,
    tu_id,
    status_code,
    stream_code,
    start_date,
    end_date,
    total_amount,
    initial_amount,
    updated
FROM ranked_payment_plan
WHERE row_num = 1 AND tu_id IN (SELECT tu_id FROM account)
ORDER BY payment_plan_id DESC;