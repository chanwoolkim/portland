# Load data ####
tu_data_files = glob.glob(os.path.join(raw_tu_data_dir, '**', '*CSV*'), recursive=True)
tu_data = pd.concat([pd.read_csv(f, delimiter='|', dtype=str) for f in tu_data_files], ignore_index=True)

tu_demographic_files = glob.glob(os.path.join(raw_tu_data_dir, '**', '*PSV*'), recursive=True)
tu_demographic = pd.concat([pd.read_csv(f, delimiter='|', dtype=str) for f in tu_demographic_files], ignore_index=True)

# Merge
tu_data = tu_data.merge(tu_demographic, left_on='customerInput_tusequencenumber', right_on='IN1', how='left')

# Transform
def to_numeric(s):
    return pd.to_numeric(s, errors='coerce')

tu_data = pd.DataFrame({
    'state': tu_data['customerInput_state'],
    'zip': tu_data['customerInput_zip'],
    'state_mail': tu_data['customerInput_state1'],
    'zip_mail': tu_data['customerInput_zip1'],
    'tu_id': to_numeric(tu_data['customerInput_tusequencenumber']),
    'tu_permid': tu_data['customerInput_customerInputPermId'],
    'credit_date': tu_data['creditAsOfDate_creditAsOfDate'],
    'credit_score': to_numeric(tu_data['cvtg03_finscore']),
    'advrs1': to_numeric(tu_data['cvtg03_advrs1']),
    'advrs2': to_numeric(tu_data['cvtg03_advrs2']),
    'advrs3': to_numeric(tu_data['cvtg03_advrs3']),
    'advrs4': to_numeric(tu_data['cvtg03_advrs4']),
    'edie': to_numeric(tu_data['edie04_score']),
    'etie': to_numeric(tu_data['etie04_score']),
    'hh_income_estimate': to_numeric(tu_data['EstHHIncome']),
    'ethnicity': tu_data.apply(lambda row: 1 if pd.notna(row['GroupD']) else (0 if pd.notna(row['GroupB']) else pd.NA), axis=1)
}).drop_duplicates()

# Save as .parquet (pyarrow format, R-compatible)
tu_data.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'tu_data.parquet'), index=False)