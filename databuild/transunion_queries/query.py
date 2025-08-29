# Preliminary ####
# Function to read SQL query from file
def load_sql_query(file_path):
    with open(file_path, 'r') as file:
        return file.read()

# SAFE_DIVIDE for SQL queries
def safe_divide(a, b):
    if a is None or b is None or b == 0:
        return None
    return a / b

duckdb.create_function('SAFE_DIVIDE', safe_divide, [float, float], float, null_handling='special')


# Load Basic Data ####
# Load RCT subjects
discount = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'discount.parquet'))
rct_exclusions = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'rct_exclusions.parquet'))

# Load TU data
tu_data = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'tu_data.parquet'))

# Load Census data
census_data = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'raw_census_data.parquet'))

# Load pre-processed files
account = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'account.parquet'))
action = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'action.parquet'))
bill = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'bill.parquet'))
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
transaction = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'transaction.parquet'))
usage = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'usage.parquet'))


# Run SQL queries ####
create_census_data = load_sql_query(code_dir + '/build_working/create_census_data.sql')
census_data = duckdb.sql(create_census_data).fetchdf()
census_data.to_parquet(os.path.join(working_data_dir, 'processed', 'census_data.parquet'))
census_data = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'census_data.parquet'))

create_account = load_sql_query(code_dir + '/build_working/create_account.sql')
account = duckdb.sql(create_account).fetchdf()
account.to_parquet(os.path.join(working_data_dir, 'processed', 'account.parquet'))
account = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'account.parquet'))

create_account_financial = load_sql_query(code_dir + '/build_working/create_account_financial.sql')
account_financial = duckdb.sql(create_account_financial).fetchdf()
account_financial.to_parquet(os.path.join(working_data_dir, 'processed', 'account_financial.parquet'))
account_financial = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'account_financial.parquet'))

create_action = load_sql_query(code_dir + '/build_working/create_action.sql')
action = duckdb.sql(create_action).fetchdf()
action.to_parquet(os.path.join(working_data_dir, 'processed', 'action.parquet'))
action = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'action.parquet'))

create_code_info = load_sql_query(code_dir + '/build_working/create_code_info.sql')
code_info = duckdb.sql(create_code_info).fetchdf()
code_info.to_parquet(os.path.join(working_data_dir, 'processed', 'code_info.parquet'))
code_info = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'code_info.parquet'))

create_linc = load_sql_query(code_dir + '/build_working/create_linc.sql')
linc = duckdb.sql(create_linc).fetchdf()
linc.to_parquet(os.path.join(working_data_dir, 'processed', 'linc.parquet'))
linc = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'linc.parquet'))

create_usage = load_sql_query(code_dir + '/build_working/create_usage.sql')
usage = duckdb.sql(create_usage).fetchdf()
usage.to_parquet(os.path.join(working_data_dir, 'processed', 'usage.parquet'))
usage = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'usage.parquet'))

create_transaction = load_sql_query(code_dir + '/build_working/create_transaction.sql')
transaction = duckdb.sql(create_transaction).fetchdf()
transaction.to_parquet(os.path.join(working_data_dir, 'processed', 'transaction.parquet'))
transaction = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'transaction.parquet'))

create_billed = load_sql_query(code_dir + '/build_working/create_billed.sql')
billed = duckdb.sql(create_billed).fetchdf()
billed.to_parquet(os.path.join(working_data_dir, 'processed', 'billed.parquet'))
billed = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'billed.parquet'))

create_billed_paid = load_sql_query(code_dir + '/build_working/create_billed_paid.sql')
billed_paid = duckdb.sql(create_billed_paid).fetchdf()
billed_paid.to_parquet(os.path.join(working_data_dir, 'processed', 'billed_paid.parquet'))
billed_paid = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'billed_paid.parquet'))

create_cycle_bill_date = load_sql_query(code_dir + '/build_working/create_cycle_bill_date.sql')
cycle_bill_date = duckdb.sql(create_cycle_bill_date).fetchdf()
cycle_bill_date.to_parquet(os.path.join(working_data_dir, 'processed', 'cycle_bill_date.parquet'))
cycle_bill_date = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'cycle_bill_date.parquet'))

create_collection_payment = load_sql_query(code_dir + '/build_working/create_collection_payment.sql')
collection_payment = duckdb.sql(create_collection_payment).fetchdf()
collection_payment.to_parquet(os.path.join(working_data_dir, 'processed', 'collection_payment.parquet'))
collection_payment = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'collection_payment.parquet'))

create_payment_plan = load_sql_query(code_dir + '/build_working/create_payment_plan.sql')
payment_plan = duckdb.sql(create_payment_plan).fetchdf()
payment_plan.to_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan.parquet'))
payment_plan = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan.parquet'))

create_payment_plan_transaction = load_sql_query(code_dir + '/build_working/create_payment_plan_transaction.sql')
payment_plan_transaction = duckdb.sql(create_payment_plan_transaction).fetchdf()
payment_plan_transaction.to_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan_transaction.parquet'))
payment_plan_transaction = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan_transaction.parquet'))

create_payment_plan_remainder = load_sql_query(code_dir + '/build_working/create_payment_plan_remainder.sql')
payment_plan_remainder = duckdb.sql(create_payment_plan_remainder).fetchdf()
payment_plan_remainder.to_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan_remainder.parquet'))
payment_plan_remainder = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'payment_plan_remainder.parquet'))

usage = duckdb.read_parquet(os.path.join(working_data_dir, 'pre-processed', 'usage.parquet'))

create_account_rct = load_sql_query(code_dir + '/build_working/create_account_rct.sql')
account_rct = duckdb.sql(create_account_rct).fetchdf()
account_rct.to_parquet(os.path.join(working_data_dir, 'processed', 'account_rct.parquet'))
account_rct = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'account_rct.parquet'))

create_estimation_dataset = load_sql_query(code_dir + '/build_working/create_estimation_dataset.sql')
estimation_dataset = duckdb.sql(create_estimation_dataset).fetchdf()
estimation_dataset.to_parquet(os.path.join(working_data_dir, 'processed', 'estimation_dataset.parquet'))
estimation_dataset = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'estimation_dataset.parquet'))

create_estimation_dataset_all = load_sql_query(code_dir + '/build_working/create_estimation_dataset_all.sql')
estimation_dataset_all = duckdb.sql(create_estimation_dataset_all).fetchdf()
estimation_dataset_all.to_parquet(os.path.join(working_data_dir, 'processed', 'estimation_dataset_all.parquet'))
estimation_dataset_all = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'estimation_dataset_all.parquet'))


# Save additional analysis files ####
# Transaction during RCT period
transaction_rct = duckdb.sql('''SELECT * FROM transaction WHERE transaction_date BETWEEN \'2024-12-12\' AND \'2025-05-21\'''').fetchdf()
transaction_rct.to_parquet(os.path.join(working_data_dir, 'analysis', 'transaction.parquet'))

# Account counts
sql_account = load_sql_query(code_dir + '/build_descriptive/account_count.sql')
account_count = duckdb.sql(sql_account).fetchdf()
account_count.to_parquet(os.path.join(working_data_dir, 'analysis', 'account_count.parquet'))

sql_option = 'AND (tu_id IN (SELECT tu_id FROM account_rct))'
sql_account_sdp = sql_account.replace('-- OPTIONS_PLACEHOLDER', sql_option)
account_count = duckdb.sql(sql_account_sdp).fetchdf()
account_count.to_parquet(os.path.join(working_data_dir, 'analysis', 'smart_discount', 'account_count.parquet'))

sql_account = load_sql_query(code_dir + '/build_descriptive/account_count_financial.sql')
account_count = duckdb.sql(sql_account).fetchdf()
account_count.to_parquet(os.path.join(working_data_dir, 'analysis', '2024q4_financial', 'account_count.parquet'))

# Revenue by category
sql_transaction = load_sql_query(code_dir + '/build_descriptive/category_revenue.sql')
transaction_aggregate = duckdb.sql(sql_transaction).fetchdf()
transaction_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'transaction_aggregate.parquet'))

sql_option = 'AND (transaction.tu_id IN (SELECT tu_id FROM account_rct))'
sql_transaction_sdp = sql_transaction.replace('-- OPTIONS_PLACEHOLDER', sql_option)
transaction_aggregate = duckdb.sql(sql_transaction_sdp).fetchdf()
transaction_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'smart_discount', 'transaction_aggregate.parquet'))

sql_transaction = load_sql_query(code_dir + '/build_descriptive/category_revenue_financial.sql')
transaction_aggregate = duckdb.sql(sql_transaction).fetchdf()
transaction_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', '2024q4_financial', 'transaction_aggregate.parquet'))

# Revenue by collection payment
sql_collection = load_sql_query(code_dir + '/build_descriptive/collection_revenue.sql')
collection_aggregate = duckdb.sql(sql_collection).fetchdf()
collection_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'collection_aggregate.parquet'))

sql_option = 'AND (collection_payment.tu_id IN (SELECT tu_id FROM account_rct))'
sql_collection_sdp = sql_collection.replace('-- OPTIONS_PLACEHOLDER', sql_option)
collection_aggregate = duckdb.sql(sql_collection_sdp).fetchdf()
collection_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'smart_discount', 'collection_aggregate.parquet'))

sql_collection = load_sql_query(code_dir + '/build_descriptive/collection_revenue_financial.sql')
collection_aggregate = duckdb.sql(sql_collection).fetchdf()
collection_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', '2024q4_financial', 'collection_aggregate.parquet'))

# Revenue by final bill
sql_final = load_sql_query(code_dir + '/build_descriptive/final_revenue.sql')
final_aggregate = duckdb.sql(sql_final).fetchdf()
final_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'final_aggregate.parquet'))

sql_option = 'WHERE (transaction.tu_id IN (SELECT tu_id FROM account_rct))'
sql_final_sdp = sql_final.replace('-- OPTIONS_PLACEHOLDER', sql_option)
final_aggregate = duckdb.sql(sql_final_sdp).fetchdf()
final_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'smart_discount', 'final_aggregate.parquet'))

sql_final = load_sql_query(code_dir + '/build_descriptive/final_revenue_financial.sql')
final_aggregate = duckdb.sql(sql_final).fetchdf()
final_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', '2024q4_financial', 'final_aggregate.parquet'))

# Revenue by payment plan
sql_payment_plan = load_sql_query(code_dir + '/build_descriptive/payment_plan_revenue.sql')
payment_plan_aggregate = duckdb.sql(sql_payment_plan).fetchdf()
payment_plan_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'payment_plan_aggregate.parquet'))

sql_option = 'AND (tu_id IN (SELECT tu_id FROM account_rct))'
sql_payment_plan_sdp = sql_payment_plan.replace('-- OPTIONS_PLACEHOLDER', sql_option)
payment_plan_aggregate = duckdb.sql(sql_payment_plan_sdp).fetchdf()
payment_plan_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', 'smart_discount', 'payment_plan_aggregate.parquet'))

sql_payment_plan = load_sql_query(code_dir + '/build_descriptive/payment_plan_revenue_financial.sql')
payment_plan_aggregate = duckdb.sql(sql_payment_plan).fetchdf()
payment_plan_aggregate.to_parquet(os.path.join(working_data_dir, 'analysis', '2024q4_financial', 'payment_plan_aggregate.parquet'))

# RCT usage/bill/transaction
usage = duckdb.read_parquet(os.path.join(working_data_dir, 'processed', 'usage.parquet'))
sql_rct_usage = load_sql_query(code_dir + '/build_descriptive/rct_usage.sql')
rct_usage = duckdb.sql(sql_rct_usage).fetchdf()
rct_usage.to_parquet(os.path.join(working_data_dir, 'analysis', 'rct_usage.parquet'))

sql_rct_bill = load_sql_query(code_dir + '/build_descriptive/rct_bill.sql')
rct_bill = duckdb.sql(sql_rct_bill).fetchdf()
rct_bill.to_parquet(os.path.join(working_data_dir, 'analysis', 'rct_bill.parquet'))

sql_rct_transaction = load_sql_query(code_dir + '/build_descriptive/rct_transaction.sql')
rct_transaction = duckdb.sql(sql_rct_transaction).fetchdf()
rct_transaction.to_parquet(os.path.join(working_data_dir, 'analysis', 'rct_transaction.parquet'))
