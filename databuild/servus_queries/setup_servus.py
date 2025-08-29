# Preliminary ####
SERVICE_ACCOUNT_FILE = project_dir + '/data/raw/servus-291816-f03620559670.json'
credentials = service_account.Credentials.from_service_account_file(SERVICE_ACCOUNT_FILE)
project_id = 'servus-291816'
client = bigquery.Client(credentials=credentials, project=project_id)

# Function to execute BigQuery SQL query
def import_bigquery_data(df_name, id=False):
    if id:
        query = f'''
                SELECT
                    a.* EXCEPT (account_number),
                    t.id AS account_id
                FROM `servus-291816.portlandWater.{df_name}` AS a
                JOIN `servus-291816.portland_auxiliary.account_id` AS t
                    ON a.account_number = t.account_number;
                '''
    else:
        query = f'''SELECT * FROM `servus-291816.portlandWater.{df_name}`;'''

    query_job = client.query(query)
    return query_job.to_dataframe()


# Main ####
# First assign unique IDs to accounts
client.query('''CREATE OR REPLACE TABLE `servus-291816.portland_auxiliary.account_id` AS
             SELECT
                account_number,
                ROW_NUMBER() OVER (ORDER BY account_number) AS id
             FROM `servus-291816.portlandWater.account`
             GROUP BY account_number;''')

# Load and save tables
account = import_bigquery_data('account', id=True)
account.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'account.parquet'))

action = import_bigquery_data('action', id=True)
action.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'action.parquet'))

bill = import_bigquery_data('bill', id=True)
bill.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'bill.parquet'))

code = import_bigquery_data('code', id=False)
code.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'code.parquet'))

collection = import_bigquery_data('collection', id=True)
collection.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'collection.parquet'))

collection_payment = import_bigquery_data('collection_payment', id=True)
collection_payment.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'collection_payment.parquet'))

discount = import_bigquery_data('discount', id=True)
discount.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'discount.parquet'))

discount_addition = import_bigquery_data('discount_addition', id=True)
discount_addition.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'discount_addition.parquet'))

financial_assistance = import_bigquery_data('financial_assistance', id=True)
financial_assistance.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'financial_assistance.parquet'))

linc = import_bigquery_data('linc', id=True)
linc.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'linc.parquet'))

location = import_bigquery_data('location', id=False)
location.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'location.parquet'))

location_account = import_bigquery_data('location_account', id=True)
location_account.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'location_account.parquet'))

location_census = import_bigquery_data('location_census')
location_census.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'location_census.parquet'))

location_person = import_bigquery_data('location_person', id=True)
location_person.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'location_person.parquet'))

payment_plan = import_bigquery_data('payment_plan', id=True)
payment_plan.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'payment_plan.parquet'))

payment_plan_transaction = import_bigquery_data('payment_plan_transaction', id=False)
payment_plan_transaction.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'payment_plan_transaction.parquet'))

rct_exclusion = import_bigquery_data('rct_exclusion', id=True)
rct_exclusion.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'rct_exclusion.parquet'))

service = import_bigquery_data('service', id=False)
service.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'service.parquet'))

transaction = import_bigquery_data('transaction', id=True)
transaction.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'transaction.parquet'))

usage = import_bigquery_data('usage', id=True)
usage.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'usage.parquet'))