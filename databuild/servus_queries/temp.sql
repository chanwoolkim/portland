-- For each row in action, find the latest bill before effective_date and get its previous_unpaid_amount
SELECT
  action.action_id,
  action.effective_date,
  previous_unpaid_amount + amount_trans_billed + amount_paid + fees + cleanriver_discount + rct_discount + linc_discount + discount + liens + writeoff + refund + transfer AS amount
FROM
  `portland_working.action` AS action
INNER JOIN
  `portland_working.billed_paid` AS billed_paid
ON
  action.account_number = billed_paid.account_number
WHERE
  billed_paid.bill_date <= action.effective_date
ORDER BY
  action.action_id,
  billed_paid.bill_date DESC;