SELECT
    year,
    quarter,
    sum(total_amount) AS total_amount_sum,
    sum(total_outstanding_amount) AS total_outstanding_amount_sum,
    sum(total_amount - total_outstanding_amount) AS difference_sum,
    sum(remaining_amount) AS remaining_amount_sum
  FROM
    `servus-291816.portland_working.payment_plan_remainder` AS payment_plan
  WHERE (year >= 2019)
  -- OPTIONS_PLACEHOLDER
GROUP BY 1, 2
ORDER BY 1, 2;