# Preliminary ####
SERVICE_ACCOUNT_FILE = project_dir + '/data/raw/servus-291816-f03620559670.json'
credentials = service_account.Credentials.from_service_account_file(SERVICE_ACCOUNT_FILE)
project_id = 'servus-291816'
client = bigquery.Client(credentials=credentials, project=project_id)

# Function to read SQL query from file
def load_sql_query(file_path):
    with open(file_path, 'r') as file:
        return file.read()
    
# Function to execute BigQuery SQL query but not return data
def run_bigquery(file_path):
    client.query(load_sql_query(file_path))
    return None

# Function to execute BigQuery SQL query
def import_bigquery_data(query):
    query_job = client.query(query)
    return query_job.to_dataframe()


# Main ####
# First create relevant datasets in BigQuery ####
run_bigquery(code_dir + '/build_working/create_account.sql')
run_bigquery(code_dir + '/build_working/create_account_rct.sql')
run_bigquery(code_dir + '/build_working/create_account_income.sql')
run_bigquery(code_dir + '/build_working/create_account_financial.sql')
run_bigquery(code_dir + '/build_working/create_action.sql')
run_bigquery(code_dir + '/build_working/create_code_info.sql')
run_bigquery(code_dir + '/build_working/create_usage.sql')
run_bigquery(code_dir + '/build_working/create_transaction.sql')
run_bigquery(code_dir + '/build_working/create_billed.sql')
run_bigquery(code_dir + '/build_working/create_billed_paid.sql')
run_bigquery(code_dir + '/build_working/create_cycle_bill_date.sql')
run_bigquery(code_dir + '/build_working/create_collection_payment.sql')
run_bigquery(code_dir + '/build_working/create_payment_plan.sql')
run_bigquery(code_dir + '/build_working/create_payment_plan_transaction.sql')
run_bigquery(code_dir + '/build_working/create_payment_plan_remainder.sql')
run_bigquery(code_dir + '/build_working/create_estimation_dataset.sql')
run_bigquery(code_dir + '/build_working/create_estimation_dataset_all.sql')

# Then run the queries to download the data ####
# RCT info
rct_info = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.account_rct`')
rct_info.to_csv(output_dir + '/account_rct.csv', index=False)

# Code info
code_info = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.code_info`')
code_info.to_csv(output_dir + '/code_info.csv', index=False)

# Payment plan
payment_plan = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.payment_plan`')
payment_plan.to_csv(output_dir + '/payment_plan.csv', index=False)

# Cycle bill date
cycle_bill_date = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.cycle_bill_date`')
cycle_bill_date.to_csv(output_dir + '/cycle_bill_date.csv', index=False)

# Action
action = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.action`')
action.to_csv(output_dir + '/action.csv', index=False)

# Billed
billed_paid = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.billed_paid`')
billed_paid.to_csv(output_dir + '/billed_paid.csv', index=False)

# Transaction during RCT period
transaction = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.transaction` WHERE transaction_date BETWEEN \'2024-12-12\' AND \'2025-05-21\'')
transaction.to_csv(output_dir + '/transaction.csv', index=False)

# Estimation dataset
estimation_dataset = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.estimation_dataset`')
estimation_dataset.to_csv(output_dir + '/estimation_dataset.csv', index=False)

estimation_dataset_all = import_bigquery_data('SELECT * FROM `servus-291816.portland_working.estimation_dataset_all`')
estimation_dataset_all.to_csv(output_dir + '/estimation_dataset_all.csv', index=False)

# Account counts
sql_account = load_sql_query(code_dir + '/build_descriptive/account_count.sql')
account_count = import_bigquery_data(sql_account)
account_count.to_csv(output_dir + '/account_count.csv', index=False)

sql_option = 'AND (account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_account_sdp = sql_account.replace('-- OPTIONS_PLACEHOLDER', sql_option)
account_count = import_bigquery_data(sql_account_sdp)
account_count.to_csv(output_dir + '/smart_discount/account_count.csv', index=False)

sql_account = load_sql_query(code_dir + '/build_descriptive/account_count_financial.sql')
account_count = import_bigquery_data(sql_account)
account_count.to_csv(output_dir + '/2024q4_financial/account_count.csv', index=False)

# Revenue by category
sql_transaction = load_sql_query(code_dir + '/build_descriptive/category_revenue.sql')
transaction_aggregate = import_bigquery_data(sql_transaction)
transaction_aggregate.to_csv(output_dir + '/transaction_aggregate.csv', index=False)

sql_option = 'AND (transaction.account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_transaction_sdp = sql_transaction.replace('-- OPTIONS_PLACEHOLDER', sql_option)
transaction_aggregate = import_bigquery_data(sql_transaction_sdp)
transaction_aggregate.to_csv(output_dir + '/smart_discount/transaction_aggregate.csv', index=False)

sql_transaction = load_sql_query(code_dir + '/build_descriptive/category_revenue_financial.sql')
transaction_aggregate = import_bigquery_data(sql_transaction)
transaction_aggregate.to_csv(output_dir + '/2024q4_financial/transaction_aggregate.csv', index=False)

# Revenue by collection payment
sql_collection = load_sql_query(code_dir + '/build_descriptive/collection_revenue.sql')
collection_aggregate = import_bigquery_data(sql_collection)
collection_aggregate.to_csv(output_dir + '/collection_aggregate.csv', index=False)

sql_option = 'AND (collection_payment.account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_collection_sdp = sql_collection.replace('-- OPTIONS_PLACEHOLDER', sql_option)
collection_aggregate = import_bigquery_data(sql_collection_sdp)
collection_aggregate.to_csv(output_dir + '/smart_discount/collection_aggregate.csv', index=False)

sql_collection = load_sql_query(code_dir + '/build_descriptive/collection_revenue_financial.sql')
collection_aggregate = import_bigquery_data(sql_collection)
collection_aggregate.to_csv(output_dir + '/2024q4_financial/collection_aggregate.csv', index=False)

# Revenue by final bill
sql_final = load_sql_query(code_dir + '/build_descriptive/final_revenue.sql')
final_aggregate = import_bigquery_data(sql_final)
final_aggregate.to_csv(output_dir + '/final_aggregate.csv', index=False)

sql_option = 'WHERE (transaction.account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_final_sdp = sql_final.replace('-- OPTIONS_PLACEHOLDER', sql_option)
final_aggregate = import_bigquery_data(sql_final_sdp)
final_aggregate.to_csv(output_dir + '/smart_discount/final_aggregate.csv', index=False)

sql_final = load_sql_query(code_dir + '/build_descriptive/final_revenue_financial.sql')
final_aggregate = import_bigquery_data(sql_final)
final_aggregate.to_csv(output_dir + '/2024q4_financial/final_aggregate.csv', index=False)

# Revenue by payment plan
sql_payment_plan = load_sql_query(code_dir + '/build_descriptive/payment_plan_revenue.sql')
payment_plan_aggregate = import_bigquery_data(sql_payment_plan)
payment_plan_aggregate.to_csv(output_dir + '/payment_plan_aggregate.csv', index=False)

sql_option = 'AND (account_number IN (SELECT account_number FROM `servus-291816.portland_working.account_rct`))'
sql_payment_plan_sdp = sql_payment_plan.replace('-- OPTIONS_PLACEHOLDER', sql_option)
payment_plan_aggregate = import_bigquery_data(sql_payment_plan_sdp)
payment_plan_aggregate.to_csv(output_dir + '/smart_discount/payment_plan_aggregate.csv', index=False)

sql_payment_plan = load_sql_query(code_dir + '/build_descriptive/payment_plan_revenue_financial.sql')
payment_plan_aggregate = import_bigquery_data(sql_payment_plan)
payment_plan_aggregate.to_csv(output_dir + '/2024q4_financial/payment_plan_aggregate.csv', index=False)

# Average bill
sql_average_bill = load_sql_query(code_dir + '/build_descriptive/average_bill.sql')
average_bill = import_bigquery_data(sql_average_bill)
average_bill.to_csv(output_dir + '/average_bill.csv', index=False)

# RCT usage/bill/transaction
sql_rct_usage = load_sql_query(code_dir + '/build_descriptive/rct_usage.sql')
rct_usage = import_bigquery_data(sql_rct_usage)
rct_usage.to_csv(output_dir + '/rct_usage.csv', index=False)

sql_rct_bill = load_sql_query(code_dir + '/build_descriptive/rct_bill.sql')
rct_bill = import_bigquery_data(sql_rct_bill)
rct_bill.to_csv(output_dir + '/rct_bill.csv', index=False)

sql_rct_transaction = load_sql_query(code_dir + '/build_descriptive/rct_transaction.sql')
rct_transaction = import_bigquery_data(sql_rct_transaction)
rct_transaction.to_csv(output_dir + '/rct_transaction.csv', index=False)

# Income credit
sql_income_credit = load_sql_query(code_dir + '/build_descriptive/income_credit.sql')
income_credit = import_bigquery_data(sql_income_credit)
income_credit.to_csv(output_dir + '/income_credit.csv', index=False)

# Newly delinquent bills
newly_delinquent_all = pd.DataFrame()

for year in range(2019, 2026) :
    for quarter in range(1, 5) :
        sql_newly_delinquent = load_sql_query(code_dir + '/build_descriptive/newly_delinquent.sql')
        sql_option = 'EXTRACT(YEAR FROM bill_date) = ' + str(year) + ' AND EXTRACT(QUARTER FROM bill_date) = ' + str(quarter)
        sql_newly_delinquent = sql_newly_delinquent.replace('-- OPTIONS_PLACEHOLDER', sql_option)
        newly_delinquent = import_bigquery_data(sql_newly_delinquent)
        newly_delinquent_all = pd.concat([newly_delinquent_all, newly_delinquent], ignore_index=True)

newly_delinquent_all.to_csv(output_dir + '/newly_delinquent.csv', index=False)