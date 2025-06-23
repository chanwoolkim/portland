SELECT DISTINCT
  tu_id,
  (etie*1000*1.037 < 65331) AS ufh_status,
  (etie*1000*1.037 <= 70800) AS fa_status,
  (etie*1000*1.037 <= 116900) AS median_status,
  CASE 
    WHEN credit_score <= 600 THEN 1
    WHEN credit_score <= 660 THEN 2
    WHEN credit_score <= 780 THEN 3
    WHEN credit_score <= 850 THEN 4
    ELSE NULL
  END AS credit_quartile
FROM
  tu_data
WHERE etie IS NOT NULL
QUALIFY
  ROW_NUMBER() OVER (PARTITION BY tu_id ORDER BY credit_date DESC) = 1;