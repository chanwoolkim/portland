WITH ranked_collection_payment AS (
  SELECT
    account_id,
    sent_timestamp,
    amount_due,
    collection_amount,
    updated,
    ROW_NUMBER() OVER (PARTITION BY account_id, sent_timestamp ORDER BY updated DESC) AS row_num
  FROM collection_payment
)
SELECT
    account_id,
    sent_timestamp,
    amount_due,
    collection_amount,
    updated
FROM ranked_collection_payment
WHERE row_num = 1 AND account_id IN (SELECT account_id FROM account)
ORDER BY account_id, sent_timestamp DESC;