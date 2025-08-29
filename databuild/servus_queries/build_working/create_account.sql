WITH latest_account AS (
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY account_id ORDER BY updated DESC) AS rn
  FROM account
),
latest_location AS (
  SELECT 
    account_id, 
    location_account.location_id,
    house_number,
    street_prefix_direction,  
    street_name,
    street_type,
    city,
    state,
    postal_code,
    ROW_NUMBER() OVER(PARTITION BY account_id ORDER BY location_account.updated DESC) AS rn
  FROM location_account
  LEFT JOIN location
    ON location_account.location_id = location.location_id
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
  FROM service
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
  FROM location_census
),
latest_bill AS (
  SELECT 
    account_id, 
    type_code,
    bill_date,
    ROW_NUMBER() OVER(PARTITION BY account_id ORDER BY bill_date DESC) AS rn
  FROM bill
),
location_aspire AS (
  SELECT 
    account_id,
    location_id,
    rn,
    aspire_north.*
  FROM latest_location
  LEFT JOIN aspire_north_data AS aspire_north
    ON latest_location.house_number = aspire_north.house_number
    AND latest_location.street_prefix_direction = aspire_north.street_prefix_direction
    AND latest_location.street_name = aspire_north.street_name
    AND latest_location.street_type = aspire_north.street_type
    AND latest_location.city = aspire_north.city
    AND latest_location.state = aspire_north.state
    AND latest_location.postal_code = aspire_north.postal_code
)
SELECT 
  a.account_id,
  a.person_id,
  a.cycle_code,
  a.billing_code,
  a.last_bill_date,
  a.last_bill_amount,
  COALESCE(ls.service_water, TRUE) AS service_water,
  COALESCE(ls.service_sewer, TRUE) AS service_sewer,
  COALESCE(ls.service_storm, TRUE) AS service_storm,
  llc.state_id, 
  llc.county_id, 
  llc.tract_id, 
  llc.block_group_id,
  la.*,
  a.created,
  a.updated
FROM latest_account AS a
  LEFT JOIN location_aspire AS la
    ON a.account_id = la.account_id AND la.rn = 1
  LEFT JOIN latest_bill AS lb
    ON a.account_id = lb.account_id AND lb.rn = 1
  LEFT JOIN location_service AS ls
    ON la.location_id = ls.location_id
  LEFT JOIN latest_location_census llc
    ON la.location_id = llc.location_id AND llc.rn = 1
WHERE a.rn = 1
ORDER BY a.updated DESC;