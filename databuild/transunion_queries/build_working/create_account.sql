WITH latest_account AS (
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY tu_id ORDER BY updated DESC) AS rn
  FROM account
),
latest_location_census AS (
  SELECT 
    tu_id,
    tract_id,
    ROW_NUMBER() OVER (PARTITION BY tu_id ORDER BY created DESC) AS rn
  FROM location_census
),
service_offer AS (
  SELECT
  tu_id,
  CASE WHEN MAX(CASE WHEN service_type = 'WATER' THEN 1 ELSE 0 END) = 1 
   THEN TRUE ELSE FALSE END AS service_water,
  CASE WHEN MAX(CASE WHEN service_type = 'SEWER' THEN 1 ELSE 0 END) = 1 
   THEN TRUE ELSE FALSE END AS service_sewer,
  CASE WHEN MAX(CASE WHEN service_type = 'STORM' THEN 1 ELSE 0 END) = 1 
   THEN TRUE ELSE FALSE END AS service_storm
  FROM service
  GROUP BY tu_id
),
latest_bill AS (
  SELECT 
    tu_id, 
    type_code,
    bill_date,
    ROW_NUMBER() OVER(PARTITION BY tu_id ORDER BY bill_date DESC) AS rn
  FROM bill
)
SELECT 
  a.tu_id,
  a.cycle_code,
  a.billing_code,
  a.last_bill_date,
  a.last_bill_amount,
  so.service_water,
  so.service_sewer,
  so.service_storm,
  llc.tract_id,
  a.created,
  a.updated
FROM latest_account AS a
  LEFT JOIN latest_bill AS lb
    ON a.tu_id = lb.tu_id AND lb.rn = 1
  LEFT JOIN latest_location_census llc
    ON a.tu_id = llc.tu_id AND llc.rn = 1
  LEFT JOIN service_offer AS so
    ON a.tu_id = so.tu_id
WHERE a.rn = 1
ORDER BY a.updated DESC;