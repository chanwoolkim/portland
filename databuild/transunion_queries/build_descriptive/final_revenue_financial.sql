WITH code_modified AS (
  SELECT
    transaction_type,
    CASE
      WHEN default_category LIKE '%XFER%' THEN 'transfer'
      ELSE category
    END AS category
  FROM code_info
),
collection AS (
  SELECT
    collection_payment.tu_id,
    SUM(collection_payment.collection_amount) AS collection_amount,
    FROM collection_payment
      LEFT JOIN account 
      ON collection_payment.tu_id = account.tu_id
    WHERE (account.billing_code = 'FINAL') AND (CAST(collection_payment.sent_date AS DATE) > account.last_bill_date)
    GROUP BY
      collection_payment.tu_id
),
final_net_bill AS (
  SELECT
    account.tu_id,
    last_bill_date,
    last_bill_amount,
    (last_bill_amount + COALESCE(SUM(CASE WHEN category = 'transfer' THEN transaction.amount ELSE 0 END), 0)) AS net_bill
  FROM account
  LEFT JOIN transaction
    ON account.tu_id = transaction.tu_id
    AND transaction.transaction_date > account.last_bill_date
    AND (NOT transaction.transaction_type LIKE '%COMMIT%')
  LEFT JOIN code_modified
    ON transaction.transaction_type = code_modified.transaction_type
  WHERE account.billing_code = 'FINAL'
    AND account.last_bill_date >= '2019-01-01'
  GROUP BY
    account.tu_id, last_bill_date, last_bill_amount
),
decomposed_final AS (
  SELECT
    final_net_bill.tu_id,
    final_net_bill.last_bill_date,
    final_net_bill.net_bill,
    account_financial.ufh_status,
    account_financial.fa_status,
    account_financial.median_status,
    account_financial.credit_quartile,
    COALESCE(collection.collection_amount, 0) AS collection_amount,
    EXTRACT(YEAR FROM CAST(final_net_bill.last_bill_date AS DATE)) AS year,
    EXTRACT(QUARTER FROM CAST(final_net_bill.last_bill_date AS DATE)) AS quarter,
    COALESCE(SUM(CASE WHEN category = 'discount' THEN transaction.amount ELSE 0 END), 0) AS discount_amount,
    COALESCE(SUM(CASE WHEN category = 'bill' THEN transaction.amount ELSE 0 END), 0) AS bill_amount,
    COALESCE(SUM(CASE WHEN category = 'payment' THEN transaction.amount ELSE 0 END), 0) AS payment_amount,
    COALESCE(SUM(CASE WHEN category = 'liens' THEN transaction.amount ELSE 0 END), 0) AS liens_amount
  FROM final_net_bill
  LEFT JOIN collection
    ON final_net_bill.tu_id = collection.tu_id
  LEFT JOIN transaction
    ON final_net_bill.tu_id = transaction.tu_id
    AND transaction.transaction_date > final_net_bill.last_bill_date
    AND (NOT transaction.transaction_type LIKE '%COMMIT%')
  LEFT JOIN code_modified
    ON transaction.transaction_type = code_modified.transaction_type
  LEFT JOIN account_financial
    ON final_net_bill.tu_id = account_financial.tu_id
  GROUP BY
    final_net_bill.tu_id, last_bill_date, collection_amount, net_bill, year, quarter, ufh_status, fa_status, median_status, credit_quartile
)
SELECT
  year,
  quarter,
  ufh_status, 
  fa_status,
  median_status,
  credit_quartile,
  SUM(net_bill) AS total_bill,
  SUM(discount_amount) AS discount_amount,
  SUM(payment_amount) AS payment_amount,
  SUM(bill_amount) AS bill_amount,
  SUM(liens_amount) AS liens_amount,
  SUM(collection_amount) AS collection_amount,
  SUM(net_bill+discount_amount+bill_amount+payment_amount+liens_amount) AS remaining_amount
FROM decomposed_final
GROUP BY
  year, quarter, ufh_status, fa_status, median_status, credit_quartile
ORDER BY
  year, quarter, ufh_status, fa_status, median_status, credit_quartile;