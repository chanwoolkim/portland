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
    location_account.location_id,
    house_number,
    street_prefix_direction,  
    street_name,
    street_type,
    city,
    state,
    postal_code,
    ROW_NUMBER() OVER(PARTITION BY account_number ORDER BY location_account.updated DESC) AS rn
  FROM `servus-291816.portlandWater.location_account` AS location_account
  LEFT JOIN `servus-291816.portlandWater.location` AS location
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
),
location_aspire AS (
  SELECT 
    account_number,
    location_id,
    CASE 
      WHEN COMBINED_HOMEOWNER IN ('H', '7', '8', '9') THEN 'Homeowner'
      WHEN COMBINED_HOMEOWNER IN ('R', 'T') THEN 'Renter'
      WHEN COMBINED_HOMEOWNER = 'U' THEN 'Unknown'
      ELSE NULL
    END AS occupancy_status,
    REPLACE(REPLACE(COMBINED_AGE_1, 'E', ''), 'I', '') AS age,
    ECHV AS estimated_current_home_value,
    lu_inc_model_v6_amt AS income,
    REC_PERSCNT AS household_size,
    REC_ADULTCNT AS adult_count,
    REC_CHILDCNT AS child_count,
    rn,
    aspire_north.* 
    EXCEPT(house_number, street_prefix_direction, street_name, street_type, city, state, postal_code,
    AID, GEOC_STAT_2000, STAT_ABBR, RECD_ZIPC, POST_ZIP4, POST_PCDE, POST_CARR, CITY_CNAB, CITY_PLAC,
    PRMY_NUMB, STRT_PRED, STRT_NAME, STRT_SUFX, STRT_POST, SCDY_DESG, SCDY_NUMB, 
    ADDRESS_LINE_1, ADDRESS_LINE_2, AQI, CNTY_2000, COUNTYNAME, 
    LATITUDE, LONGITUDE, GEO_LEVEL, TIME_ZONE, LUID, 
    POBJ_POBJ, PHON_NUMB_1, PHON_NUMB_2, POBJ_USAGE, SU_STATE_POBJ, SU_NATIONAL_POBJ,
    PID_1, PERSTYPE_1, FNAM_FNAM_1, MNAM_MNAM_1, SNAM_SNAM_1, SFXT_SFXT_1, PFXT_PFXT_1,
    PID_2, PERSTYPE_2, FNAM_FNAM_2, MNAM_MNAM_2, SNAM_SNAM_2, SFXT_SFXT_2, PFXT_PFXT_2,
    PID_3, PERSTYPE_3, FNAM_FNAM_3, MNAM_MNAM_3, SNAM_SNAM_3, SFXT_SFXT_3, PFXT_PFXT_3,
    PID_4, PERSTYPE_4, FNAM_FNAM_4, MNAM_MNAM_4, SNAM_SNAM_4, SFXT_SFXT_4, PFXT_PFXT_4,
    PID_5, PERSTYPE_5, FNAM_FNAM_5, MNAM_MNAM_5, SNAM_SNAM_5, SFXT_SFXT_5, PFXT_PFXT_5,
    PID_6, PERSTYPE_6, FNAM_FNAM_6, MNAM_MNAM_6, SNAM_SNAM_6, SFXT_SFXT_6, PFXT_PFXT_6)
  FROM latest_location
  LEFT JOIN `servus-291816.aspire_north.aspire_north_data` AS aspire_north
    ON latest_location.house_number = aspire_north.house_number
    AND latest_location.street_prefix_direction = aspire_north.street_prefix_direction
    AND latest_location.street_name = aspire_north.street_name
    AND latest_location.street_type = aspire_north.street_type
    AND latest_location.city = aspire_north.city
    AND latest_location.state = aspire_north.state
    AND latest_location.postal_code = aspire_north.postal_code
)
SELECT 
  a.account_number,
  a.person_id,
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
  llc.state_id, 
  llc.county_id, 
  llc.tract_id, 
  llc.block_group_id,
  la.* EXCEPT(account_number),
  a.created,
  a.updated
FROM latest_account AS a
  LEFT JOIN location_aspire AS la
    ON a.account_number = la.account_number AND la.rn = 1
  LEFT JOIN latest_bill AS lb
    ON a.account_number = lb.account_number AND lb.rn = 1
  LEFT JOIN location_service AS ls
    ON la.location_id = ls.location_id
  LEFT JOIN latest_location_census llc
    ON la.location_id = llc.location_id AND llc.rn = 1
WHERE a.rn = 1
ORDER BY a.updated DESC;