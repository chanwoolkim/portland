CREATE OR REPLACE TABLE `servus-291816.portland_working.action` AS
WITH ranked_action AS (
  SELECT
    action_id,
    action_code,
    person_id,
    account_number,
    location_id,
    effective_date,
    updated,
    ROW_NUMBER() OVER (PARTITION BY action_id ORDER BY updated DESC) AS row_num
  FROM `servus-291816.portlandWater.action`
)
SELECT
    action_id,
    action_code,
    person_id,
    account_number,
    location_id,
    effective_date,
    updated
FROM ranked_action
WHERE row_num = 1 AND account_number IN (SELECT account_number FROM `servus-291816.portland_working.account`)
ORDER BY account_number, effective_date;