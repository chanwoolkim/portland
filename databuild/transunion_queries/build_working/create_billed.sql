WITH filtered_bills AS (
  SELECT 
    *,
    ROW_NUMBER() OVER (PARTITION BY tu_id, bill_date ORDER BY updated DESC) AS bill_num
  FROM bill
  WHERE is_canceled = FALSE
    AND is_error = FALSE
    AND is_voided = FALSE
    AND is_corrected = FALSE
    AND audit_or_live = 'L'
    AND start_date IS NOT NULL
    AND end_date IS NOT NULL
    AND due_date IS NOT NULL
),
ordered_bills AS (
  SELECT 
    *,
    LEAD(source_code, 1) OVER (PARTITION BY tu_id ORDER BY bill_date) AS next_source_code,
    LEAD(due_date, 1) OVER (PARTITION BY tu_id ORDER BY bill_date) AS next_due_date,
    COALESCE(LEAD(amount, 1) OVER (PARTITION BY tu_id ORDER BY bill_date), 0) AS next_amount,
    LEAD(source_code, 2) OVER (PARTITION BY tu_id ORDER BY bill_date) AS next_next_source_code,
    LEAD(due_date, 2) OVER (PARTITION BY tu_id ORDER BY bill_date) AS next_next_due_date,
    COALESCE(LEAD(amount, 2) OVER (PARTITION BY tu_id ORDER BY bill_date), 0) AS next_next_amount
  FROM filtered_bills
  WHERE bill_num = 1
),
encapsulate_amount AS (
  SELECT
      tu_id,
      type_code,
      start_date,
      end_date,
      bill_date,
      source_code,
      CASE 
        WHEN source_code = 'QB1' AND next_source_code = 'QB2' AND next_next_source_code = 'QB3' THEN next_next_due_date
        WHEN source_code = 'QB1' AND next_source_code = 'QB2' THEN next_due_date
        ELSE due_date
      END AS due_date,
      created,
      COALESCE(previous_bill_amount, 0) AS previous_bill_amount,
      COALESCE(ar_due_before_bill, 0) AS ar_due_before_bill,
      COALESCE(ar_unapplied_cr_before_bill, 0) AS ar_unapplied_cr_before_bill,
      COALESCE(ar_due_after_bill, 0) AS ar_due_after_bill,
      COALESCE(ar_net_after_bill, 0) AS ar_net_after_bill,
      COALESCE(non_bill_generated_changes, 0) AS non_bill_generated_changes,
      COALESCE(total_payments, 0) AS total_payments,
      COALESCE(amount, 0) AS amount,
      CASE 
        WHEN source_code = 'QB1' AND next_source_code = 'QB2' AND next_next_source_code = 'QB3' THEN amount + next_amount + next_next_amount
        WHEN source_code = 'QB1' AND next_source_code = 'QB2' THEN amount + next_amount
        ELSE amount
      END AS current_amount,
  FROM ordered_bills AS bill
  WHERE bill.type_code = 'REGLR' OR bill.type_code = 'FINAL'
),
quarterly_bill AS (
  SELECT DISTINCT
    tu_id,
    type_code,
    start_date,
    end_date,
    due_date,
    bill_date,
    ar_net_after_bill AS amount_due,
    current_amount,
    created,
    previous_bill_amount,
    ar_due_before_bill + ar_unapplied_cr_before_bill AS previous_unpaid_amount
  FROM encapsulate_amount
)
SELECT
  quarterly_bill.tu_id,
  quarterly_bill.type_code,
  account.cycle_code,
  quarterly_bill.start_date,
  quarterly_bill.end_date,
  quarterly_bill.due_date,
  quarterly_bill.bill_date,
  COALESCE(LEAD(bill_date) OVER (PARTITION BY quarterly_bill.tu_id ORDER BY bill_date ASC), CURRENT_DATE()) AS next_bill_date,
  quarterly_bill.created,
  quarterly_bill.previous_bill_amount,
  quarterly_bill.previous_unpaid_amount,
  quarterly_bill.current_amount,
  quarterly_bill.amount_due
FROM quarterly_bill
  JOIN account 
  ON account.tu_id = quarterly_bill.tu_id
ORDER BY quarterly_bill.tu_id, quarterly_bill.bill_date;