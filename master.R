#=========================================================================#
# Master file
#
# Chanwool Kim, August 21, 2024
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
wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/PWB")
tu_data_dir <- paste0(wd, "/data/raw/TU")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Run Databuild Scripts
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/databuild/setup_portland.R"))
source(paste0(code_dir, "/databuild/setup_transunion.R"))
source(paste0(code_dir, "/databuild/tidy_census_api.R"))
source(paste0(code_dir, "/databuild/delinquency_measure.R"))
source(paste0(code_dir, "/databuild/financial_assistance_clean.R"))
source(paste0(code_dir, "/databuild/merge_data.R"))
source(paste0(code_dir, "/databuild/bill_usage_clean.R"))
source(paste0(code_dir, "/databuild/delinquency_construct.R"))
source(paste0(code_dir, "/databuild/panel_construct.R"))
source(paste0(code_dir, "/databuild/panel_estimation_construct.R"))
source(paste0(code_dir, "/databuild/merge_portland_transunion.R"))

#---------+---------+---------+---------+---------+---------+
# Choose RCT Subjects
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/rct/rct1_randomize.R"))

end_time <- Sys.time()
end_time-start_time
