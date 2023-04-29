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

working_data/analysis_info.Rdata contains the raw data
working_data/account_info_analysis.RData contains the data primarily used in the analysis
- Note: this file is in the account level
- Note: this file does not restrict to residential single family: filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST")) to restrict to single family


ANALYSIS
descriptive_statistics.R
- Create tables for basic descriptive statistics and overview of the data (account types)
descriptive_statistics_graph.R
- Create graphs for basic descriptive statistics (pie charts)
descriptive_statistics_resmf.R
- Create tables for basic descriptive statistics for residential multi-family units only
descriptive_statistics_payment.R
- Create tables/graphs for the payment arrangement programme
- Note: this file also loads financial_assistance_info.RData
descriptive_statistics_linc.R
- Create tables/graphs for the financial assistance programme
descriptive_statistics_did.R
- Create tables for the difference-in-differences exercise