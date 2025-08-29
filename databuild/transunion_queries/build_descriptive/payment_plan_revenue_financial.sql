SELECT
  CAST(year AS INT) AS year,
  CAST(quarter AS INT) AS quarter,
  ufh_status,
  fa_status,
  median_status,
  credit_quartile,
  SUM(total_amount) AS total_amount_sum,
  SUM(total_outstanding_amount) AS total_outstanding_amount_sum,
  SUM(total_amount - total_outstanding_amount) AS difference_sum,
  SUM(remaining_amount) AS remaining_amount_sum
  FROM payment_plan_remainder
   LEFT JOIN account_financial
   ON payment_plan_remainder.tu_id = account_financial.tu_id
  WHERE (year >= 2019)
GROUP BY 1, 2, 3, 4, 5, 6;