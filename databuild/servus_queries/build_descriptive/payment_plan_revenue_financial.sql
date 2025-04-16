SELECT
    year,
    quarter,
    ufh_status,
    fa_status,
    median_status,
    credit_quartile,
    sum(total_amount) AS total_amount_sum,
    sum(total_outstanding_amount) AS total_outstanding_amount_sum,
    sum(total_amount - total_outstanding_amount) AS difference_sum,
    sum(remaining_amount) AS remaining_amount_sum
  FROM
    `servus-291816.portland_working.payment_plan_remainder` AS payment_plan
   LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
   ON payment_plan.account_number = account_financial.account_number
  WHERE (year >= 2019)
GROUP BY 1, 2, 3, 4, 5, 6;