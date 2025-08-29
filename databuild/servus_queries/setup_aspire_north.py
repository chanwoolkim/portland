# Load data ####
aspire_north_data = pd.read_csv(os.path.join(raw_aspire_north_data_dir, 'address_Append_20250507.csv'), low_memory=False)
aspire_north_codebook = pd.read_csv(os.path.join(working_data_dir, 'pre-processed', 'aspire_north_codebook.csv'), low_memory=False)


# Process data ####
# Remove columns that are not needed
aspire_north_data = aspire_north_data.drop(columns=[
    'AID', 'GEOC_STAT_2000', 'STAT_ABBR',
    'RECD_ZIPC', 'POST_ZIP4', 'POST_PCDE', 'POST_CARR',
    'CITY_CNAB', 'CITY_PLAC', 'PRMY_NUMB', 'STRT_PRED', 'STRT_NAME',
    'STRT_SUFX', 'STRT_POST', 'SCDY_DESG', 'SCDY_NUMB',
    'ADDRESS_LINE_1', 'ADDRESS_LINE_2', 'AQI', 'CNTY_2000',
    'COUNTYNAME', 'LATITUDE', 'LONGITUDE', 'GEO_LEVEL', 'TIME_ZONE',
    'LUID', 'POBJ_POBJ', 'PHON_NUMB_1', 'PHON_NUMB_2',
    'POBJ_USAGE', 'SU_STATE_POBJ', 'SU_NATIONAL_POBJ',
    'PID_1', 'PERSTYPE_1', 'FNAM_FNAM_1', 'MNAM_MNAM_1',
    'SNAM_SNAM_1', 'SFXT_SFXT_1', 'PFXT_PFXT_1',
    'PID_2', 'PERSTYPE_2', 'FNAM_FNAM_2', 'MNAM_MNAM_2',
    'SNAM_SNAM_2', 'SFXT_SFXT_2', 'PFXT_PFXT_2',
    'PID_3', 'PERSTYPE_3', 'FNAM_FNAM_3', 'MNAM_MNAM_3',
    'SNAM_SNAM_3', 'SFXT_SFXT_3', 'PFXT_PFXT_3',
    'PID_4', 'PERSTYPE_4', 'FNAM_FNAM_4', 'MNAM_MNAM_4',
    'SNAM_SNAM_4', 'SFXT_SFXT_4', 'PFXT_PFXT_4',
    'PID_5', 'PERSTYPE_5', 'FNAM_FNAM_5', 'MNAM_MNAM_5',
    'SNAM_SNAM_5', 'SFXT_SFXT_5', 'PFXT_PFXT_5',
    'PID_6', 'PERSTYPE_6', 'FNAM_FNAM_6', 'MNAM_MNAM_6',
    'SNAM_SNAM_6', 'SFXT_SFXT_6', 'PFXT_PFXT_6'
])

# Clean up data
# Values over 101 should be capped at 100
fix_over_100 = ['LU_HOP', 'POC03_V3_SCORE', 'POC46_V3_SCORE', 'POC79_V3_SCORE', 'POC1012_V3_SCORE', 'POC1315_V3_SCORE', 'POC1618_V3_SCORE']

for col in fix_over_100:
    if col in aspire_north_data.columns:
        aspire_north_data[col] = np.where(aspire_north_data[col] > 100, 100, aspire_north_data[col])

# Strip off first 1 character from the following columns
strip_first_1 = ['OCCUP_V2_1', 'COMBINED_AGE_1', 'EDUC_MODEL_1', 'MRTL_MODEL_1',
                 'OCCUP_V2_2', 'COMBINED_AGE_2', 'EDUC_MODEL_2', 'MRTL_MODEL_2',
                 'OCCUP_V2_3', 'COMBINED_AGE_3', 'EDUC_MODEL_3', 'MRTL_MODEL_3',
                 'OCCUP_V2_4', 'COMBINED_AGE_4', 'EDUC_MODEL_4', 'MRTL_MODEL_4',
                 'OCCUP_V2_5', 'COMBINED_AGE_5', 'EDUC_MODEL_5', 'MRTL_MODEL_5',
                 'OCCUP_V2_6', 'COMBINED_AGE_6', 'EDUC_MODEL_6', 'MRTL_MODEL_6',
                 'EST_CUR_LTV_V2_FULL',
                 'political_code_1', 'political_code_2', 'political_code_3',
                 'political_code_4', 'political_code_5', 'political_code_6']

for col in strip_first_1:
    if col in aspire_north_data.columns:
        aspire_north_data[col] = aspire_north_data[col].where(
            aspire_north_data[col].isna(),  # keep NaNs as-is
            aspire_north_data[col].astype(str).str[1:]
        )

        if aspire_north_data[col].dtype == 'object':
            try:
                aspire_north_data[col] = pd.to_numeric(aspire_north_data[col], errors='raise')
            except ValueError:
                pass

# Strip off first 2 characters from the following columns
strip_first_2 = ['EST_CUR_MORT_AMT', 'EST_CUR_MORT_PAY_FULL', 'EST_AVAIL_EQTY_FULL']

for col in strip_first_2:
    if col in aspire_north_data.columns:
        aspire_north_data[col] = aspire_north_data[col].where(
            aspire_north_data[col].isna(),
            aspire_north_data[col].astype(str).str[2:]
        )

        if aspire_north_data[col].dtype == 'object':
            try:
                aspire_north_data[col] = pd.to_numeric(aspire_north_data[col], errors='raise')
            except ValueError:
                pass

# If codebook has data_type 'numerical', convert the column to numeric
for col in aspire_north_codebook['Field Cards']:
    if col in aspire_north_data.columns:
        data_type = aspire_north_codebook.loc[aspire_north_codebook['Field Cards'] == col, 'data_type'].values[0]
        if data_type == 'numerical':
            aspire_north_data[col] = pd.to_numeric(aspire_north_data[col], errors='coerce')

# Take care of coded unknowns
for col in aspire_north_codebook['Field Cards']:
    if col in aspire_north_data.columns:
        description = str(aspire_north_codebook.loc[aspire_north_codebook['Field Cards'] == col, 'Long Description'].values[0])
        field_values = str(aspire_north_codebook.loc[aspire_north_codebook['Field Cards'] == col, 'Field Values'].values[0])
        if '0=Unknown' in description or '0=Unknown' in field_values or '00, Unknown' in description or '00, Unknown' in field_values:
            aspire_north_data[col] = aspire_north_data[col].replace([0, '0', '00', '000', '0U', 'U0', 'U00'], np.nan)
        if 'U=Unknown' in description or 'U=Unknown' in field_values or 'UC=Uncodable' in description or 'UC=Uncodable' in field_values:
            aspire_north_data[col] = aspire_north_data[col].replace(['0U', '5U', 'UC', 'U'], np.nan)

aspire_north_data.columns = aspire_north_data.columns.str.lower().str.replace('-', '_')

# Save as .parquet
aspire_north_data.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'aspire_north_data.parquet'), index=False)
