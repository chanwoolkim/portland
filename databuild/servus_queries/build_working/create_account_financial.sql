SELECT DISTINCT
  account.account_id,
  (account.lu_inc_model_v6_amt*1000 < 65331) AS ufh_status,
  (account.lu_inc_model_v6_amt*1000 <= 70800) AS fa_status,
  (account.lu_inc_model_v6_amt*1000 <= 116900) AS median_status,
FROM account