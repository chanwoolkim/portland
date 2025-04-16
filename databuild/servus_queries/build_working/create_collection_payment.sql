CREATE OR REPLACE TABLE `servus-291816.portland_working.collection_payment` AS
WITH ranked_collection_payment AS (
  SELECT
    account_number,
    sent_timestamp,
    amount_due,
    collection_amount,
    updated,
ROW_NUMBER() OVER (PARTITION BY account_number, sent_timestamp ORDER BY updated DESC) AS row_num
  FROM `servus-291816.portlandWater.collection_payment`
)
SELECT
    account_number,
    sent_timestamp,
    amount_due,
    collection_amount,
    updated
FROM ranked_collection_payment
WHERE row_num = 1 AND account_number IN (SELECT account_number FROM `servus-291816.portland_working.account`)
ORDER BY account_number, sent_timestamp DESC;