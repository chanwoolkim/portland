README.txt

This document describes the scripts in portland_servus (in order of the process in the master.R file). For replication, simply run master.R.


DATA PREPARATION
geocoding_api.R
- Codes Census tract based on address information in the Portland data
tidy_census_api.R
- Loads Census data and saves relevant variables
setup_portland.R
- Loads Portland data and saves in the .RData format (analysis_info.Rdata for all data, financial_assistance_info.RData for financial assistance programme only)
delinquency_measure.R
- Constructs delinquency-related variables: delinquency status and amount, shutoff status
financial_assistance_clean.R
- Constructs variables for various financial assistance programs: financial assistance and payment arrangement
merge_data.R
- Merge all data, saves account_info_analysis.RData

data/analysis/analysis_info.Rdata contains the raw data
data/analysis/account_info_analysis.RData contains the data primarily used in the analysis
- Note: this file is in the account level
- Note: this file does not restrict to residential single family: filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST")) to restrict to single family
data/analysis/delinquency_status/RData contains the panel data on bills and payments
