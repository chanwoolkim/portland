SELECT
    EXTRACT(YEAR FROM collection_payment.sent_timestamp) AS year,
    EXTRACT(QUARTER FROM collection_payment.sent_timestamp) AS quarter,
    sum(collection_payment.amount_due) AS total_amount,
    sum(collection_payment.collection_amount) AS total_payment,
    count(distinct collection_payment.account_number) AS n
  FROM `servus-291816.portland_working.collection_payment` AS collection_payment
    LEFT JOIN `servus-291816.portland_working.account` AS account 
    ON collection_payment.account_number = account.account_number
  WHERE (account.billing_code <> 'FINAL') AND (sent_timestamp BETWEEN "2019-01-01" AND "2025-02-01")
  -- OPTIONS_PLACEHOLDER
  GROUP BY 1, 2
  ORDER BY 1, 2;