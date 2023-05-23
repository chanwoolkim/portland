#=========================================================================#
# mklist_for_matches
#
# Open estimation sample from Portland and create list of unique household-dates
#
# Chanwool Kim, May 22, 2023
#
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
rm(list=ls())
start_time <- Sys.time()

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))

#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
load(file=paste0(working_data_dir, "/delinquency_status.RData"))
load(file=paste0(working_data_dir, "/geocode_address_info_subset.RData"))
load(file=paste0(working_data_dir, "/analysis_info.RData"))

# Match person number
delinquency_status <- left_join(delinquency_status,
                                account_info %>% select(ACCOUNT_NO, PERSON_NO),
                                by="ACCOUNT_NO")

# Match location number
delinquency_status <-
  left_join(delinquency_status,
            location_relation %>%
              select(ACCT_TO_FRC_CONNECT,
                     LOCATION_NO,
                     PERSON_NO),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT",
                 "PERSON_NO"))

# Match address info
delinquency_status <-
  left_join(delinquency_status,
            geocode_address_info_subset %>%
              select(LOCATION_NO, input_address, matched_address, census_tract),
            by="LOCATION_NO")

# Select relevant variables
output_data <- delinquency_status %>%
  select(ACCOUNT_NO, DUE_DT, PERIOD_FROM_DT, PERIOD_TO_DT, PERSON_NO, LOCATION_NO,
         input_address, matched_address, census_tract)

# Get the range of dates for each account
output_data_summary <- output_data %>%
  group_by(ACCOUNT_NO, PERSON_NO, LOCATION_NO, input_address, matched_address, census_tract) %>%
  summarise(observe_from_dt=min(PERIOD_FROM_DT),
            observe_to_dt=max(PERIOD_TO_DT))
  