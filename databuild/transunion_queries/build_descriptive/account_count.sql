SELECT
    EXTRACT(YEAR FROM transaction.transaction_date) AS year,
    EXTRACT(QUARTER FROM transaction.transaction_date) AS quarter,
    count(distinct tu_id) AS n
  FROM transaction
  WHERE (transaction_date BETWEEN '2019-01-01' AND '2025-02-01')
  -- OPTIONS_PLACEHOLDER
  GROUP BY 1, 2
  ORDER BY 1, 2;