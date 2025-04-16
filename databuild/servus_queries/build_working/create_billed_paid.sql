CREATE OR REPLACE TABLE `servus-291816.portland_working.billed_paid` AS
WITH minimum_start AS (
  SELECT 
    account_number, 
    MIN(start_date) AS first_start_date
  FROM `servus-291816.portland_working.billed`
  GROUP BY account_number
),
ordered_bill AS (
  SELECT
    *,
    amount_due AS amount_billed,
    ROW_NUMBER() OVER (PARTITION BY account_number ORDER BY start_date DESC) AS bill_num
  FROM `servus-291816.portland_working.billed`
),
payment_between_dates AS (
  SELECT
    bill.account_number,
    bill.type_code,
    bill.cycle_code,
    bill.start_date,
    bill.end_date,
    bill.due_date,
    bill.bill_date,
    bill.next_bill_date,
    bill.created,
    bill.previous_bill_amount,
    bill.previous_unpaid_amount,
    bill.amount_due AS amount_billed,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'bill' THEN transaction.amount ELSE 0 END), 0) AS amount_trans_billed,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'fees' THEN transaction.amount ELSE 0 END), 0) AS fees,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'payment' THEN transaction.amount ELSE 0 END), 0) AS amount_paid,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'cleanriver_discount' THEN transaction.amount ELSE 0 END), 0) AS cleanriver_discount,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'rct_discount' THEN transaction.amount ELSE 0 END), 0) AS rct_discount,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'linc_discount' THEN transaction.amount ELSE 0 END), 0) AS linc_discount,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'discount' THEN transaction.amount ELSE 0 END), 0) AS discount,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'liens' THEN transaction.amount ELSE 0 END), 0) AS liens,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'writeoff' THEN transaction.amount ELSE 0 END), 0) AS writeoff,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'refund' THEN transaction.amount ELSE 0 END), 0) AS refund,
    COALESCE(SUM(CASE WHEN transaction.transaction_category = 'transfer' THEN transaction.amount ELSE 0 END), 0) AS transfer
  FROM ordered_bill AS bill
  JOIN minimum_start
    ON minimum_start.account_number = bill.account_number
  LEFT JOIN `servus-291816.portland_working.transaction` AS transaction
    ON transaction.account_number = bill.account_number
    AND transaction.transaction_date < bill.next_bill_date
    AND (bill.start_date = minimum_start.first_start_date OR transaction.transaction_date >= bill.bill_date)
  GROUP BY
    bill.account_number,
    bill.type_code,
    bill.cycle_code,
    bill.start_date,
    bill.end_date,
    bill.due_date,
    bill.bill_date,
    bill.next_bill_date,
    bill.created,
    bill.previous_bill_amount,
    bill.previous_unpaid_amount,
    bill.amount_due
  ORDER BY bill.start_date DESC
),
numbered_payments AS (
  SELECT 
    payment_between_dates.account_number,
    payment_between_dates.type_code,
    payment_between_dates.cycle_code,
    payment_between_dates.start_date, 
    payment_between_dates.end_date,
    payment_between_dates.due_date,
    payment_between_dates.bill_date,
    payment_between_dates.next_bill_date,
    payment_between_dates.previous_bill_amount,
    payment_between_dates.previous_unpaid_amount,
    payment_between_dates.amount_billed,
    payment_between_dates.amount_trans_billed,
    payment_between_dates.fees,
    payment_between_dates.amount_paid,
    payment_between_dates.cleanriver_discount,
    payment_between_dates.rct_discount,
    payment_between_dates.linc_discount,
    payment_between_dates.discount,
    payment_between_dates.liens,
    payment_between_dates.writeoff,
    payment_between_dates.refund,
    payment_between_dates.transfer,
    ROW_NUMBER() OVER (PARTITION BY payment_between_dates.account_number ORDER BY payment_between_dates.start_date ASC) AS row_num
  FROM payment_between_dates
)
SELECT 
account_number,
type_code,
cycle_code,
start_date, 
end_date,
bill_date,
next_bill_date,
due_date,
previous_bill_amount,
previous_unpaid_amount,
amount_billed,
amount_trans_billed,
fees,
amount_paid,
cleanriver_discount,
rct_discount,
linc_discount,
discount,
liens,
writeoff,
refund,
transfer,
SUM(
  CASE 
    WHEN row_num = 1 
    THEN previous_unpaid_amount + amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer
    ELSE amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer
  END
) OVER (PARTITION BY account_number ORDER BY start_date ASC) AS running_owed
FROM numbered_payments
ORDER BY account_number, bill_date;
