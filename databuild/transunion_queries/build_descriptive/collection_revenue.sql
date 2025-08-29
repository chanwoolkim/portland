SELECT
    EXTRACT(YEAR FROM CAST(collection_payment.sent_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(collection_payment.sent_date AS DATE)) AS quarter,
    sum(collection_payment.amount_due) AS total_amount,
    sum(collection_payment.collection_amount) AS total_payment,
    count(distinct collection_payment.tu_id) AS n
  FROM collection_payment
    LEFT JOIN account 
    ON collection_payment.tu_id = account.tu_id
  WHERE (account.billing_code <> 'FINAL') AND (sent_date BETWEEN '2019-01-01' AND '2025-02-01')
  -- OPTIONS_PLACEHOLDER
  GROUP BY 1, 2
  ORDER BY 1, 2;