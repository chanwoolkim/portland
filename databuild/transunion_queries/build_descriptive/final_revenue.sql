WITH code_modified AS (
  SELECT
    transaction_type,
    CASE
      WHEN default_category LIKE '%XFER%' THEN 'transfer'
      ELSE category
    END AS category
  FROM `servus-291816.portland_working.code_info`
),
collection AS (
  SELECT
    collection_payment.account_number,
    sum(collection_payment.collection_amount) AS collection_amount,
    FROM `servus-291816.portland_working.collection_payment` AS collection_payment
      LEFT JOIN `servus-291816.portland_working.account` AS account 
      ON collection_payment.account_number = account.account_number
    WHERE (account.billing_code = 'FINAL') AND (DATE(collection_payment.sent_timestamp) > account.last_bill_date)
    GROUP BY
      collection_payment.account_number
),
final_net_bill AS (
  SELECT
    account.account_number,
    last_bill_date,
    last_bill_amount,
    (last_bill_amount + COALESCE(SUM(CASE WHEN category = 'transfer' THEN transaction.amount ELSE 0 END), 0)) AS net_bill
  FROM `servus-291816.portland_working.account` AS account
  LEFT JOIN `servus-291816.portland_working.transaction` AS transaction
    ON account.account_number = transaction.account_number
    AND transaction.transaction_date > account.last_bill_date
    AND (NOT transaction.transaction_type LIKE '%COMMIT%')
  LEFT JOIN code_modified
    ON transaction.transaction_type = code_modified.transaction_type
  WHERE account.billing_code = 'FINAL'
    AND account.last_bill_date >= '2019-01-01'
  GROUP BY
    account.account_number, last_bill_date, last_bill_amount
),
decomposed_final AS (
  SELECT
    final_net_bill.account_number,
    final_net_bill.last_bill_date,
    final_net_bill.net_bill,
    COALESCE(collection.collection_amount, 0) AS collection_amount,
    EXTRACT(YEAR FROM final_net_bill.last_bill_date) AS year,
    EXTRACT(QUARTER FROM final_net_bill.last_bill_date) AS quarter,
    COALESCE(SUM(CASE WHEN category = 'discount' THEN transaction.amount ELSE 0 END), 0) AS discount_amount,
    COALESCE(SUM(CASE WHEN category = 'bill' THEN transaction.amount ELSE 0 END), 0) AS bill_amount,
    COALESCE(SUM(CASE WHEN category = 'payment' THEN transaction.amount ELSE 0 END), 0) AS payment_amount,
    COALESCE(SUM(CASE WHEN category = 'liens' THEN transaction.amount ELSE 0 END), 0) AS liens_amount
  FROM final_net_bill
  LEFT JOIN collection
    ON final_net_bill.account_number = collection.account_number
  LEFT JOIN `servus-291816.portland_working.transaction` AS transaction
    ON final_net_bill.account_number = transaction.account_number
    AND transaction.transaction_date > final_net_bill.last_bill_date
    AND (NOT transaction.transaction_type LIKE '%COMMIT%')
  LEFT JOIN code_modified
    ON transaction.transaction_type = code_modified.transaction_type
  LEFT JOIN `servus-291816.portland_working.account_financial` AS account_financial
    ON final_net_bill.account_number = account_financial.account_number
  -- OPTIONS_PLACEHOLDER
  GROUP BY
    account_number, last_bill_date, collection_amount, net_bill, year, quarter
)
SELECT
  year,
  quarter,
  SUM(net_bill) AS total_bill,
  SUM(discount_amount) AS discount_amount,
  SUM(payment_amount) AS payment_amount,
  SUM(bill_amount) AS bill_amount,
  SUM(liens_amount) AS liens_amount,
  SUM(collection_amount) AS collection_amount,
  SUM(net_bill+discount_amount+bill_amount+payment_amount+liens_amount) AS remaining_amount
FROM decomposed_final
GROUP BY
  year, quarter
ORDER BY
  year, quarter;
