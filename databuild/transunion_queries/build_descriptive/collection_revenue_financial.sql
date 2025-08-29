SELECT
    EXTRACT(YEAR FROM CAST(collection_payment.sent_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(collection_payment.sent_date AS DATE)) AS quarter,
    account_financial.ufh_status,
    account_financial.fa_status,
    account_financial.median_status,
    account_financial.credit_quartile,
    sum(collection_payment.amount_due) AS total_amount,
    sum(collection_payment.collection_amount) AS total_payment,
    count(distinct collection_payment.tu_id) AS n
  FROM collection_payment
    LEFT JOIN account 
    ON collection_payment.tu_id = account.tu_id
    LEFT JOIN account_financial
    ON collection_payment.tu_id = account_financial.tu_id
  WHERE (account.billing_code <> 'FINAL') AND (sent_date BETWEEN '2019-01-01' AND '2025-02-15')
  GROUP BY 1, 2, 3, 4, 5, 6
  ORDER BY 1, 2, 3, 4, 5, 6;