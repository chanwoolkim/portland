WITH ranked_collection_payment AS (
  SELECT
    tu_id,
    sent_time,
    amount_due,
    collection_amount,
    updated,
ROW_NUMBER() OVER (PARTITION BY tu_id, sent_time ORDER BY updated DESC) AS row_num
  FROM collection_payment
)
SELECT
    tu_id,
    sent_time,
    amount_due,
    collection_amount,
    updated
FROM ranked_collection_payment
WHERE row_num = 1 AND tu_id IN (SELECT tu_id FROM account)
ORDER BY tu_id, sent_time DESC;