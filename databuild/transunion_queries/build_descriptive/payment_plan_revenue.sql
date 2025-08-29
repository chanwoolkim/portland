SELECT
  CAST(year AS INT) AS year,
  CAST(quarter AS INT) AS quarter,
  SUM(total_amount) AS total_amount_sum,
  SUM(total_outstanding_amount) AS total_outstanding_amount_sum,
  SUM(total_amount - total_outstanding_amount) AS difference_sum,
  SUM(remaining_amount) AS remaining_amount_sum
FROM payment_plan_remainder
WHERE (year >= 2019)
-- OPTIONS_PLACEHOLDER
GROUP BY 1, 2
ORDER BY 1, 2;