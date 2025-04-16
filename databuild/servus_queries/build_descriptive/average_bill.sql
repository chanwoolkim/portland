SELECT
    EXTRACT(YEAR FROM bill_date) AS year,
    AVG(previous_bill_amount) AS average_bill,
    AVG(previous_bill_amount - discount) AS average_bill_discount,
    APPROX_QUANTILES(previous_bill_amount, 100)[OFFSET(50)] AS median_bill,
    APPROX_QUANTILES(previous_bill_amount - discount, 100)[OFFSET(50)] AS median_bill_discount
    FROM
      `servus-291816.portland_working.billed_paid`
    WHERE bill_date >= '2019-01-01'
    GROUP BY year;