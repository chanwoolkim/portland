WITH ranked_action AS (
  SELECT
    action_id,
    action_code,
    person_id,
    account_id,
    location_id,
    effective_date,
    updated,
    ROW_NUMBER() OVER (PARTITION BY action_id ORDER BY updated DESC) AS row_num
  FROM action
)
SELECT
    action_id,
    action_code,
    person_id,
    account_id,
    location_id,
    effective_date,
    updated
FROM ranked_action
WHERE row_num = 1 AND account_id IN (SELECT account_id FROM account)
ORDER BY account_id, effective_date;