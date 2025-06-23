SELECT
    EXTRACT(YEAR FROM transaction.transaction_date) AS year,
    EXTRACT(QUARTER FROM transaction.transaction_date) AS quarter,
    count(distinct account_number) AS n
  FROM `servus-291816.portlandWater.transaction` AS transaction
  WHERE (transaction_date BETWEEN "2019-01-01" AND "2025-02-01")
  -- OPTIONS_PLACEHOLDER
  GROUP BY 1, 2
  ORDER BY 1, 2;