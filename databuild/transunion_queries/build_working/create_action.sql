WITH ranked_action AS (
  SELECT
    action_id,
    action_code,
    tu_id,
    effective_date,
    updated,
    ROW_NUMBER() OVER (PARTITION BY action_id ORDER BY updated DESC) AS row_num
  FROM action
)
SELECT
    action_id,
    action_code,
    tu_id,
    effective_date,
    updated
FROM ranked_action
WHERE row_num = 1 AND tu_id IN (SELECT tu_id FROM account)
ORDER BY tu_id, effective_date;