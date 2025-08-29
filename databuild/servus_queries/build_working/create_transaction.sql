WITH ranked_transactions AS (
  SELECT
    transaction_id,
    account_id,
    TRIM(transaction_type) AS transaction_type,
    source_reference,
    summary,
    status_code,
    TRIM(transaction_code) AS transaction_code,
    transaction_date,
    matched_date,
    last_matched_date,
    amount,
    adjusted_amount,
    remaining_amount,
    updated,
    ROW_NUMBER() OVER (PARTITION BY transaction_id ORDER BY updated DESC) AS row_num
  FROM transaction
)
SELECT
    transaction_id,
    account_id,
    transaction_type,
    source_reference,
    CASE
      WHEN transaction_code = 'ADJ' AND transaction_type LIKE '%CRISIS%' THEN 'discount'
      WHEN transaction_code = 'ADJ' THEN 'adjustment'
      WHEN transaction_code IN ('B2', 'B3', 'BILLB', 'BILLV') THEN 'bill'
      WHEN transaction_code = 'BNKRP' THEN 'writeoff'
      WHEN transaction_code = 'CONV' THEN 'bill'
      WHEN summary = 'DONAT' THEN 'bill'
      WHEN transaction_code = 'DSCNT' AND transaction_type LIKE '%RVR%' THEN 'cleanriver_discount'
      WHEN transaction_code = 'DSCNT' AND transaction_type LIKE '%SDP%' THEN 'rct_discount'
      WHEN transaction_code = 'DSCNT' AND transaction_type LIKE '%LINC%' THEN 'linc_discount'
      WHEN transaction_code = 'DSCNT' THEN 'discount'
      WHEN transaction_code LIKE 'FEES' THEN 'fees'
      WHEN transaction_code LIKE 'FEESP' THEN 'fees'
      WHEN transaction_code LIKE 'LRFEE' THEN 'fees'
      WHEN transaction_code LIKE 'LIENS' THEN 'liens'
      WHEN transaction_code = 'PYMNT' THEN 'payment'
      WHEN transaction_code = 'REFND' AND transaction_type = 'REFUND' THEN 'refund'
      WHEN transaction_code = 'WRTOF' AND status_code = 'CONFM' THEN 'writeoff'
      WHEN transaction_code = 'XFER' AND transaction_type IN ('PMT XFR', 'DNT BL XFR') THEN 'payment'
      WHEN transaction_code = 'XFER' AND summary IN ('PNLTY', 'SEWER', 'WATER') THEN 'transfer'
      ELSE 'unknown'
    END AS transaction_category,
    transaction_code,
    transaction_date,
    matched_date,
    last_matched_date,
    amount,
    adjusted_amount,
    remaining_amount,
    updated
FROM ranked_transactions
WHERE row_num = 1 AND account_id IN (SELECT account_id FROM account)
  AND transaction_type NOT IN ('VARCOMMIT', 'FIXCOMMIT', 'PAYCOMMIT')
  AND transaction_type NOT LIKE '%CF%'
  AND transaction_type NOT LIKE '%BB%'
ORDER BY transaction_id DESC;