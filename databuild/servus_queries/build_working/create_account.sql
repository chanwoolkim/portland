CREATE OR REPLACE TABLE `servus-291816.portland_working.account` AS
WITH latest_account AS (
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY account_number ORDER BY updated DESC) AS rn
  FROM `servus-291816.portlandWater.account`
),
latest_location AS (
  SELECT 
    account_number, 
    location_id,
    ROW_NUMBER() OVER(PARTITION BY account_number ORDER BY updated DESC) AS rn
  FROM `servus-291816.portlandWater.location_account`
),
location_service AS (
  SELECT
  location_id,
  CASE WHEN MAX(CASE WHEN service_type = 'WATER' THEN 1 ELSE 0 END) = 1 
   THEN TRUE ELSE FALSE END AS service_water,
  CASE WHEN MAX(CASE WHEN service_type = 'SEWER' THEN 1 ELSE 0 END) = 1 
   THEN TRUE ELSE FALSE END AS service_sewer,
  CASE WHEN MAX(CASE WHEN service_type = 'STORM' THEN 1 ELSE 0 END) = 1 
   THEN TRUE ELSE FALSE END AS service_storm
  FROM `servus-291816.portlandWater.service`
  GROUP BY location_id
),
latest_location_census AS (
  SELECT 
    location_id, 
    state_id,
    county_id,
    tract_id,
    SUBSTR(block_id, 1, 1) AS block_group_id,
    ROW_NUMBER() OVER (PARTITION BY location_id ORDER BY created DESC) AS rn
  FROM `servus-291816.portlandWater.location_census`
),
latest_bill AS (
  SELECT 
    account_number, 
    type_code,
    bill_date,
    ROW_NUMBER() OVER(PARTITION BY account_number ORDER BY bill_date DESC) AS rn
  FROM `servus-291816.portlandWater.bill`
)
SELECT 
  a.account_number,
  a.cycle_code,
  a.billing_code,
  a.last_bill_date,
  a.last_bill_amount,
  CASE
  WHEN (lb.type_code = 'REGLR' OR lb.type_code = 'MSTMT') AND lb.bill_date > DATE_SUB(CURRENT_DATE(), INTERVAL 100 DAY)
    THEN TRUE
    ELSE FALSE
  END AS active,
  COALESCE(ls.service_water, TRUE) AS service_water,
  COALESCE(ls.service_sewer, TRUE) AS service_sewer,
  COALESCE(ls.service_storm, TRUE) AS service_storm,
  ll.location_id,
  llc.state_id, 
  llc.county_id, 
  llc.tract_id, 
  llc.block_group_id,
  a.created,
  a.updated
FROM latest_account AS a
  LEFT JOIN latest_location AS ll
    ON a.account_number = ll.account_number AND ll.rn = 1
  LEFT JOIN latest_bill AS lb
    ON a.account_number = lb.account_number AND lb.rn = 1
  LEFT JOIN location_service AS ls
    ON ll.location_id = ls.location_id
  LEFT JOIN latest_location_census llc
    ON ll.location_id = llc.location_id AND llc.rn = 1
WHERE a.rn = 1
ORDER BY a.updated DESC;