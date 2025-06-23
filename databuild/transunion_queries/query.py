# Preliminary ####
# Function to read SQL query from file
def load_sql_query(file_path):
    with open(file_path, 'r') as file:
        return file.read()


# Main ####
# Load RCT subjects
discount = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'discount.parquet'))
rct_exclusions = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'rct_exclusions.parquet'))

# Load TU data
tu_data = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'tu_data.parquet'))

# Load pre-processed files
account = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'account.parquet'))
action = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'action.parquet'))
code = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'code.parquet'))
collection = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'collection.parquet'))
collection_payment = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'collection_payment.parquet'))
financial_assistance = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'financial_assistance.parquet'))
linc = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'linc.parquet'))
location_relation = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'location_relation.parquet'))
location_census = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'location_census.parquet'))
payment_plan = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'payment_plan.parquet'))
payment_plan_transaction = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'payment_plan_transaction.parquet'))
service = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'service.parquet'))
bill = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'bill.parquet'))
transaction = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'transaction.parquet'))
usage = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'usage.parquet'))

# Run SQL queries
create_account = load_sql_query(code_dir + '/build_working/create_account.sql')
account = duckdb.sql(create_account).fetchdf()
account.to_parquet(os.path.join(working_data_dir, 'processed', 'account.parquet'))

create_account_rct = load_sql_query(code_dir + '/build_working/create_account_rct.sql')
account_rct = duckdb.sql(create_account_rct).fetchdf()
account_rct.to_parquet(os.path.join(working_data_dir, 'processed', 'account_rct.parquet'))

create_account_financial = load_sql_query(code_dir + '/build_working/create_account_financial.sql')
account_financial = duckdb.sql(create_account_financial).fetchdf()
account_financial.to_parquet(os.path.join(working_data_dir, 'processed', 'account_financial.parquet'))

create_action = load_sql_query(code_dir + '/build_working/create_action.sql')
action = duckdb.sql(create_action).fetchdf()
action.to_parquet(os.path.join(working_data_dir, 'processed', 'action.parquet'))

create_code_info = load_sql_query(code_dir + '/build_working/create_code_info.sql')
code_info = duckdb.sql(create_code_info).fetchdf()
code_info.to_parquet(os.path.join(working_data_dir, 'processed', 'code_info.parquet'))

create_usage = load_sql_query(code_dir + '/build_working/create_usage.sql')
usage = duckdb.sql(create_usage).fetchdf()
usage.to_parquet(os.path.join(working_data_dir, 'processed', 'usage.parquet'))

create_transaction = load_sql_query(code_dir + '/build_working/create_transaction.sql')
transaction = duckdb.sql(create_transaction).fetchdf()
transaction.to_parquet(os.path.join(working_data_dir, 'processed', 'transaction.parquet'))

create_billed = load_sql_query(code_dir + '/build_working/create_billed.sql')
billed = duckdb.sql(create_billed).fetchdf()
billed.to_parquet(os.path.join(working_data_dir, 'processed', 'billed.parquet'))

create_billed_paid = load_sql_query(code_dir + '/build_working/create_billed_paid.sql')
billed_paid = duckdb.sql(create_billed_paid).fetchdf()
billed_paid.to_parquet(os.path.join(working_data_dir, 'processed', 'billed_paid.parquet'))

create_cycle_bill_date = load_sql_query(code_dir + '/build_working/create_cycle_bill_date.sql')
cycle_bill_date = duckdb.sql(create_cycle_bill_date).fetchdf()
cycle_bill_date.to_parquet(os.path.join(working_data_dir, 'processed', 'cycle_bill_date.parquet'))

create_collection_payment = load_sql_query(code_dir + '/build_working/create_collection_payment.sql')
collection_payment = duckdb.sql(create_collection_payment).fetchdf()
collection_payment.to_parquet(os.path.join(working_data_dir, 'processed', 'collection_payment.parquet'))

create_payment_plan = load_sql_query(code_dir + '/build_working/create_payment_plan.sql')
payment_plan = duckdb.sql(create_payment_plan).fetchdf()
payment_plan.to_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan.parquet'))

create_payment_plan_transaction = load_sql_query(code_dir + '/build_working/create_payment_plan_transaction.sql')
payment_plan_transaction = duckdb.sql(create_payment_plan_transaction).fetchdf()
payment_plan_transaction.to_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan_transaction.parquet'))

create_payment_plan_remainder = load_sql_query(code_dir + '/build_working/create_payment_plan_remainder.sql')
payment_plan_remainder = duckdb.sql(create_payment_plan_remainder).fetchdf()
payment_plan_remainder.to_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan_remainder.parquet'))

run_bigquery(code_dir + '/build_working/create_estimation_dataset.sql')
run_bigquery(code_dir + '/build_working/create_estimation_dataset_all.sql')


# Then run the queries to download the data ####
account_rct.to_csv(working_data_dir + '/analysis/account_rct.csv', index=False)
code_info.to_csv(working_data_dir + '/analysis/code_info.csv', index=False)
payment_plan.to_csv(working_data_dir + '/analysis/payment_plan.csv', index=False)
cycle_bill_date.to_csv(working_data_dir + '/analysis/cycle_bill_date.csv', index=False)
action.to_csv(working_data_dir + '/analysis/action.csv', index=False)
billed_paid.to_csv(working_data_dir + '/analysis/billed_paid.csv', index=False)
estimation_dataset.to_csv(working_data_dir + '/analysis/estimation_dataset.csv', index=False)
estimation_dataset_all.to_csv(working_data_dir + '/analysis/estimation_dataset_all.csv', index=False)

# Transaction during RCT period
transaction = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.transaction` WHERE transaction_date BETWEEN \'2024-12-12\' AND \'2025-05-21\'')
transaction.to_csv(working_data_dir + '/analysis/transaction.csv', index=False)

# Account counts
sql_account = load_sql_query(code_dir + '/build_descriptive/account_count.sql')
account_count = import_bigquery_data(sql_account)
account_count.to_csv(working_data_dir + '/analysis/account_count.csv', index=False)

sql_option = 'AND (account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_account_sdp = sql_account.replace('-- OPTIONS_PLACEHOLDER', sql_option)
account_count = import_bigquery_data(sql_account_sdp)
account_count.to_csv(working_data_dir + '/analysis/smart_discount/account_count.csv', index=False)

sql_account = load_sql_query(code_dir + '/build_descriptive/account_count_financial.sql')
account_count = import_bigquery_data(sql_account)
account_count.to_csv(working_data_dir + '/analysis/2024q4_financial/account_count.csv', index=False)

# Revenue by category
sql_transaction = load_sql_query(code_dir + '/build_descriptive/category_revenue.sql')
transaction_aggregate = import_bigquery_data(sql_transaction)
transaction_aggregate.to_csv(working_data_dir + '/analysis/transaction_aggregate.csv', index=False)

sql_option = 'AND (transaction.account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_transaction_sdp = sql_transaction.replace('-- OPTIONS_PLACEHOLDER', sql_option)
transaction_aggregate = import_bigquery_data(sql_transaction_sdp)
transaction_aggregate.to_csv(working_data_dir + '/analysis/smart_discount/transaction_aggregate.csv', index=False)

sql_transaction = load_sql_query(code_dir + '/build_descriptive/category_revenue_financial.sql')
transaction_aggregate = import_bigquery_data(sql_transaction)
transaction_aggregate.to_csv(working_data_dir + '/analysis/2024q4_financial/transaction_aggregate.csv', index=False)

# Revenue by collection payment
sql_collection = load_sql_query(code_dir + '/build_descriptive/collection_revenue.sql')
collection_aggregate = import_bigquery_data(sql_collection)
collection_aggregate.to_csv(working_data_dir + '/analysis/collection_aggregate.csv', index=False)

sql_option = 'AND (collection_payment.account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_collection_sdp = sql_collection.replace('-- OPTIONS_PLACEHOLDER', sql_option)
collection_aggregate = import_bigquery_data(sql_collection_sdp)
collection_aggregate.to_csv(working_data_dir + '/analysis/smart_discount/collection_aggregate.csv', index=False)

sql_collection = load_sql_query(code_dir + '/build_descriptive/collection_revenue_financial.sql')
collection_aggregate = import_bigquery_data(sql_collection)
collection_aggregate.to_csv(working_data_dir + '/analysis/2024q4_financial/collection_aggregate.csv', index=False)

# Revenue by final bill
sql_final = load_sql_query(code_dir + '/build_descriptive/final_revenue.sql')
final_aggregate = import_bigquery_data(sql_final)
final_aggregate.to_csv(working_data_dir + '/analysis/final_aggregate.csv', index=False)

sql_option = 'WHERE (transaction.account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_final_sdp = sql_final.replace('-- OPTIONS_PLACEHOLDER', sql_option)
final_aggregate = import_bigquery_data(sql_final_sdp)
final_aggregate.to_csv(working_data_dir + '/analysis/smart_discount/final_aggregate.csv', index=False)

sql_final = load_sql_query(code_dir + '/build_descriptive/final_revenue_financial.sql')
final_aggregate = import_bigquery_data(sql_final)
final_aggregate.to_csv(working_data_dir + '/analysis/2024q4_financial/final_aggregate.csv', index=False)

# Revenue by payment plan
sql_payment_plan = load_sql_query(code_dir + '/build_descriptive/payment_plan_revenue.sql')
payment_plan_aggregate = import_bigquery_data(sql_payment_plan)
payment_plan_aggregate.to_csv(working_data_dir + '/analysis/payment_plan_aggregate.csv', index=False)

sql_option = 'AND (account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_payment_plan_sdp = sql_payment_plan.replace('-- OPTIONS_PLACEHOLDER', sql_option)
payment_plan_aggregate = import_bigquery_data(sql_payment_plan_sdp)
payment_plan_aggregate.to_csv(working_data_dir + '/analysis/smart_discount/payment_plan_aggregate.csv', index=False)

sql_payment_plan = load_sql_query(code_dir + '/build_descriptive/payment_plan_revenue_financial.sql')
payment_plan_aggregate = import_bigquery_data(sql_payment_plan)
payment_plan_aggregate.to_csv(working_data_dir + '/analysis/2024q4_financial/payment_plan_aggregate.csv', index=False)

# Average bill
sql_average_bill = load_sql_query(code_dir + '/build_descriptive/average_bill.sql')
average_bill = import_bigquery_data(sql_average_bill)
average_bill.to_csv(working_data_dir + '/analysis/average_bill.csv', index=False)

# RCT usage/bill/transaction
sql_rct_usage = load_sql_query(code_dir + '/build_descriptive/rct_usage.sql')
rct_usage = import_bigquery_data(sql_rct_usage)
rct_usage.to_csv(working_data_dir + '/analysis/rct_usage.csv', index=False)

sql_rct_bill = load_sql_query(code_dir + '/build_descriptive/rct_bill.sql')
rct_bill = import_bigquery_data(sql_rct_bill)
rct_bill.to_csv(working_data_dir + '/analysis/rct_bill.csv', index=False)

sql_rct_transaction = load_sql_query(code_dir + '/build_descriptive/rct_transaction.sql')
rct_transaction = import_bigquery_data(sql_rct_transaction)
rct_transaction.to_csv(working_data_dir + '/analysis/rct_transaction.csv', index=False)

# Income credit
sql_income_credit = load_sql_query(code_dir + '/build_descriptive/income_credit.sql')
income_credit = import_bigquery_data(sql_income_credit)
income_credit.to_csv(working_data_dir + '/analysis/income_credit.csv', index=False)

# Newly delinquent bills
newly_delinquent_all = pd.DataFrame()

for year in range(2019, 2026) :
    for quarter in range(1, 5) :
        sql_newly_delinquent = load_sql_query(code_dir + '/build_descriptive/newly_delinquent.sql')
        sql_option = 'EXTRACT(YEAR FROM bill_date) = ' + str(year) + ' AND EXTRACT(QUARTER FROM bill_date) = ' + str(quarter)
        sql_newly_delinquent = sql_newly_delinquent.replace('-- OPTIONS_PLACEHOLDER', sql_option)
        newly_delinquent = import_bigquery_data(sql_newly_delinquent)
        newly_delinquent_all = pd.concat([newly_delinquent_all, newly_delinquent], ignore_index=True)

newly_delinquent_all.to_csv(working_data_dir + '/analysis/newly_delinquent.csv', index=False)