CREATE OR REPLACE TABLE `servus-291816.portland_working.cycle_bill_date` AS
WITH cycle_aggregate AS (
  SELECT
    COUNT(DISTINCT account_number) AS count,
    cycle_code,
    bill_date
  FROM `servus-291816.portland_working.billed_paid` AS billed_paid
  WHERE CAST(cycle_code AS int) BETWEEN 1 AND 64
  GROUP BY cycle_code, bill_date
  ORDER BY cycle_code, bill_date
)
SELECT
  cycle_code,
  bill_date
FROM cycle_aggregate
WHERE (CAST(cycle_code AS int64) < 64 AND count > 1000)
  OR (CAST(cycle_code AS int64) >= 64 AND count > 100)
ORDER BY cycle_code, bill_date;