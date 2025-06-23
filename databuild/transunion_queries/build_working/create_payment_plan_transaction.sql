WITH ranked_payment_plan_transaction AS (
  SELECT
    payment_plan_id,
    due_date,
    amount,
    transaction_id,
    last_pay_date,
    outstanding_amount,
    updated,
    ROW_NUMBER() OVER (PARTITION BY payment_plan_id, transaction_id ORDER BY updated DESC) AS row_num
  FROM transaction
)
SELECT
    ranked_payment_plan_transaction.payment_plan_id,
    due_date,
    amount,
    transaction_id,
    last_pay_date,
    outstanding_amount,
    account_number,
    start_date,
    end_date,
    COALESCE(last_pay_date, due_date) AS action_date,
    ranked_payment_plan_transaction.updated
FROM ranked_payment_plan_transaction
INNER JOIN payment_plan AS payment_plan
ON ranked_payment_plan_transaction.payment_plan_id = payment_plan.payment_plan_id
WHERE row_num = 1
ORDER BY payment_plan_id DESC;