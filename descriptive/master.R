#=========================================================================#
# Master file
#
# Analyze SERVUS Data
#
# JP Dube, May 8, 2023
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
data_dir <- paste0(wd, "/data/raw/servus")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Run Analysis Scripts
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/descriptive/descriptive_statistics.R"))
source(paste0(code_dir, "/descriptive/descriptive_statistics_graph.R"))
source(paste0(code_dir, "/descriptive/descriptive_statistics_resmf.R"))
source(paste0(code_dir, "/descriptive/descriptive_statistics_payment.R"))
source(paste0(code_dir, "/descriptive/descriptive_statistics_linc.R"))
source(paste0(code_dir, "/descriptive/descriptive_statistics_did.R"))

end_time <- Sys.time()
end_time-start_time
