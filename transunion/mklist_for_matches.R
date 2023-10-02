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
load(file=paste0(working_data_dir, "/analysis_info.RData.gz"))
load(file=paste0(working_data_dir, "/geocode_address_info_subset.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))
load(file=paste0(working_data_dir, "/location_financial.RData"))


#---------+---------+---------+---------+---------+---------+
# Create Purchase Panel and List of Unique Accounts
#---------+---------+---------+---------+---------+---------+
# Begin from bill info
bill_info <- bill_info %>%
  mutate(DUE_DT=mdy(DUE_DT),
         BILL_RUN_DT=mdy(BILL_RUN_DT))

bill_info_filtered <- bill_info %>% 
  filter(!CANCELED_BILL_YN,
         !is.na(PERIOD_FROM_DT), 
         !is.na(PERIOD_TO_DT),
         !is.na(DUE_DT),
         !is.na(BILL_RUN_DT),
         !ERROR_YN,
         AUDIT_OR_LIVE=="L",
         BILL_TP %in% c("REGLR", "MSTMT", "FINAL"),
         SOURCE_CD %in% c("", "QB1", "QB2", "QB3"),
         !CORRECTED_BILL_YN,
         year(BILL_RUN_DT)==2023) %>%
  select(ACCOUNT_NO, PERSON_NO, DUE_DT, BILL_RUN_DT,
         PERIOD_FROM_DT, PERIOD_TO_DT)

# Get location relation
bill_location <- bill_info_filtered %>%
  left_join(location_financial, by=c("ACCOUNT_NO", "DUE_DT", "BILL_RUN_DT"="BILL_DT"))

location_relation <- location_relation %>%
  mutate(ACTUAL_END_DT=ifelse(ACTUAL_END_DT=="", "12/31/2099", ACTUAL_END_DT),
         EFFECTIVE_DT=mdy(EFFECTIVE_DT),
         ACTUAL_END_DT=mdy(ACTUAL_END_DT),
         PERSON_NO=as.numeric(PERSON_NO),
         LOCATION_NO=as.numeric(LOCATION_NO))

bill_location <- bill_location %>%
  left_join(location_relation %>%
              select(ACCT_TO_FRC_CONNECT,
                     PERSON_NO,
                     LOCATION_NO,
                     EFFECTIVE_DT,
                     ACTUAL_END_DT),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT",
                 "PERSON_NO"))

# Prioritise location relation
bill_location_loc_rel <- bill_location %>%
  filter(!is.na(LOCATION_NO.y)) %>%
  mutate(LOCATION_NO=LOCATION_NO.y) %>%
  filter(!is.na(EFFECTIVE_DT),
         !is.na(ACTUAL_END_DT)) %>%
  filter(between(BILL_RUN_DT, EFFECTIVE_DT, ACTUAL_END_DT))

bill_location_fin <- bill_location %>%
  filter(is.na(LOCATION_NO.y)) %>%
  mutate(LOCATION_NO=LOCATION_NO.x)

bill_location <- rbind(bill_location_loc_rel, bill_location_fin) %>%
  select(-LOCATION_NO.x, -LOCATION_NO.y) %>%
  group_by(ACCOUNT_NO, PERSON_NO) %>%
  fill(LOCATION_NO, .direction="downup") %>%
  filter(!is.na(LOCATION_NO)) %>%
  ungroup() %>%
  distinct(ACCOUNT_NO, PERSON_NO, LOCATION_NO, .keep_all=TRUE)

# Add in Census tract info
bill_location <- bill_location %>%
  left_join(geocode_address_info_subset %>%
              mutate(LOCATION_NO=as.numeric(LOCATION_NO)) %>%
              select(LOCATION_NO, census_tract, input_address, matched_address),
            by="LOCATION_NO") %>%
  select(-EFFECTIVE_DT, -ACTUAL_END_DT) %>%
  filter(!is.na(census_tract))

# Add in account type and only select residential families
bill_location <- bill_location %>%
  left_join(account_info %>%
              mutate(ACCOUNT_CLASS_DFLT=trimws(ACCOUNT_CLASS_DFLT),
                     PERSON_NO=as.numeric(PERSON_NO)) %>%
              select(ACCOUNT_NO, PERSON_NO, ACCOUNT_CLASS_DFLT),
            by=c("ACCOUNT_NO", "PERSON_NO")) %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST", "RESMF")) %>%
  mutate(ACCOUNT_CLASS_DFLT=ifelse(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST"),
                                   "RESSF",
                                   "RESMF"))


#---------+---------+---------+---------+---------+---------+
# Summary Statistics
#---------+---------+---------+---------+---------+---------+
summary_stats <- bill_location %>%
  group_by(ACCOUNT_CLASS_DFLT, census_tract) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  spread(ACCOUNT_CLASS_DFLT, n) %>%
  mutate(RESMF=ifelse(is.na(RESMF), 0, RESMF),
         RESSF=ifelse(is.na(RESSF), 0, RESSF)) %>%
  left_join(portland_demographics_tract_wide %>%
              select(tract, black, hispanic, white, hh_income),
            by=c("census_tract"="tract")) %>%
  mutate(other=100-black-hispanic-white) %>%
  select(Tract=census_tract,
         Single_Family=RESSF,
         Multi_Family=RESMF,
         Black=black,
         Hispanic=hispanic,
         White=white,
         Other=other,
         Income=hh_income)

write.csv(summary_stats,file="data/forTransUnion/summary_stats.csv",row.names=FALSE)


#---------+---------+---------+---------+---------+---------+
# Output Data
#---------+---------+---------+---------+---------+---------+
# Select relevant variables
output_data <- bill_location %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESSF", "ASST")) %>%
  select(ACCOUNT_NO, PERSON_NO, LOCATION_NO,
         input_address, matched_address, census_tract)


#---------+---------+---------+---------+---------+---------+
# Create List of Accounts for TransUnion
#---------+---------+---------+---------+---------+---------+
write.csv(output_data,file="data/forTransUnion/accountlist.csv",row.names=FALSE)
