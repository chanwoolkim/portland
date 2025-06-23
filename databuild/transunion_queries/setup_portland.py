# Preliminary ####
folder_list = ['201901-202407', '202407-202411', '202412-202502', '202503-202506']

# Helper to bind two dataframes
def df_bind(df1, df2):
    return pd.concat([df1, df2], ignore_index=True)

# Helper to read files
def load_and_bind(analysis_name, raw_name, multiple=False):
    if multiple:
        files = os.listdir(os.path.join(raw_pwb_data_dir, folder_list[0], raw_name))
        files = sorted([file for file in files if raw_name in file])
        
        df = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[0], raw_name, files[0]))

        for file in files[1:]:
            df_next = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[0], raw_name, file), header=None, names=df.columns)
            df = df_bind(df, df_next)
    else:
        df = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[0], f'{raw_name}.csv'))
    
    df.columns = df.columns.str.lower()
    df['created'] = pd.to_datetime(df['created'], errors='coerce')
    df['updated'] = pd.to_datetime(df['updated'], errors='coerce')

    for folder in folder_list[1:]:
        df_next = pd.read_csv(os.path.join(raw_pwb_data_dir, folder, f'{analysis_name}.csv'))
        df_next.columns = df_next.columns.str.lower()
        df_next.rename(columns={'tu_number': 'tu_id'}, inplace=True)
        df_next['created'] = pd.to_datetime(df_next['created'], errors='coerce')
        df_next['updated'] = pd.to_datetime(df_next['updated'], errors='coerce')
        df = df_bind(df, df_next)

    return df


# Load and rename tables ####
account = load_and_bind('account', 'UM00200M')
action = load_and_bind('action', 'RS00200M')
code = load_and_bind('code', 'AR50100C')
collection = load_and_bind('collection', 'CO00400T')
collection_payment = load_and_bind('collection_payment', 'CO00450T')
financial_assistance = load_and_bind('financial_assistance', 'UM00232T')
location_relation = load_and_bind('location_relation', 'UM00120T')
payment_plan = load_and_bind('payment_plan', 'CO00200M')
payment_plan_transaction = load_and_bind('payment_plan_transaction', 'CO00210T')

# Census tract only exist in the initial batch
location_census = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[0], 'UM00250T.csv'))
location_census.columns = location_census.columns.str.lower()
location_census['created'] = pd.to_datetime(location_census['created'], errors='coerce')
location_census['updated'] = pd.to_datetime(location_census['updated'], errors='coerce')

# Service table only exist in later batches
service = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[1], 'service.csv'))

for folder in folder_list[2:]:
    service_next = pd.read_csv(os.path.join(raw_pwb_data_dir, folder, 'service.csv'))
    service = df_bind(service, service_next)

service.columns = service.columns.str.lower()
service.rename(columns={'tu_number': 'tu_id'}, inplace=True)
service['created'] = pd.to_datetime(service['created'], errors='coerce')
service['updated'] = pd.to_datetime(service['updated'], errors='coerce')

# LINC structured differently
linc = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[1], 'linc.csv'))

for folder in folder_list[2:]:
    linc_next = pd.read_csv(os.path.join(raw_pwb_data_dir, folder, 'linc.csv'))
    linc = df_bind(linc, linc_next)

linc.columns = linc.columns.str.lower()
linc.rename(columns={'tu_number': 'tu_id'}, inplace=True)
linc['created'] = pd.to_datetime(linc['created'], errors='coerce')
linc['updated'] = pd.to_datetime(linc['updated'], errors='coerce')

files = os.listdir(os.path.join(raw_pwb_data_dir, folder_list[0], 'LINC'))
files = [file for file in files if 'LINC' in file]

for file in files[0:]:
    linc_next = pd.read_csv(os.path.join(raw_pwb_data_dir, folder_list[0], 'LINC', file))
    linc = df_bind(linc, linc_next)
    
# Save individual df to Parquet using pyarrow
df_list = ['account',
           'action', 
           'code', 
           'collection', 
           'collection_payment',
           'financial_assistance', 
           'linc', 
           'location_relation', 
           'location_census',
           'payment_plan',
           'payment_plan_transaction', 
           'service']

for df in df_list:
    df_name = globals()[df]
    df_name.to_parquet(os.path.join(working_data_dir, 'pre-processed', f'{df}.parquet'))


# Large size files separately ####
bill = load_and_bind('bill', 'UM00260T', multiple=True)
transaction = load_and_bind('transaction', 'AR00200T', multiple=True)
usage = load_and_bind('usage', 'UM00262T', multiple=True)

df_list_large = ['bill', 
                 'transaction',
                 'usage']

for df in df_list_large:
    df_name = globals()[df]
    df_name.to_parquet(os.path.join(working_data_dir, 'pre-processed', f'{df}.parquet'))
    

# RCT subjects
rct_subjects = pd.read_csv(os.path.join(project_dir, 'data/analysis', 'portland_rct_subject.csv'))
rct_additional = pd.read_csv(os.path.join(project_dir, 'data/analysis', 'portland_rct_additional.csv'))
rct_subjects = pd.concat([rct_subjects, rct_additional], ignore_index=True)
rct_subjects.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'discount.parquet'))

rct_exclusions = pd.read_excel(os.path.join(auxiliary_data_dir, 'RCT1AcctsNotBilledOptOutTrialAnonymizedBooth_FINAL03122025.xlsx'),
                               sheet_name='3-12-2025')
rct_exclusions = rct_exclusions.rename(columns={'TU Number': 'tu_id', 'Reason ': 'reason'})
rct_exclusions = rct_exclusions[['tu_id', 'reason']]
rct_exclusions_additional = pd.read_excel(os.path.join(auxiliary_data_dir, 'RCT1AcctsNotBilledOptOutTrialAnonymizedBooth_FINAL03122025.xlsx'),
                                          sheet_name='10-11-2024')
rct_exclusions_additional = rct_exclusions_additional.rename(columns={'Tu Id': 'tu_id', 'Exclusion Reason': 'reason'})
rct_exclusions = pd.concat([rct_exclusions, rct_exclusions_additional], ignore_index=True).drop_duplicates(subset=['tu_id'])
rct_exclusions.to_parquet(os.path.join(working_data_dir, 'pre-analysis', 'rct_exclusions.parquet'))