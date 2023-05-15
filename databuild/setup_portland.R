#=========================================================================#
# setup_portland.R
#
# Load Raw Portland Data and save to Rdata format
#
# Chanwool Kim, Feb 20, 2023
#
#=========================================================================#


location_relation <- read.table(file=paste0(data_dir, "/UM00120T_FINAL.txt"),
                                sep=",", quote="", comment.char="",
                                fill=TRUE, header=TRUE)

address_info <- read.table(file=paste0(data_dir, "/UM00100M_FINAL.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE)

account_info <- read.table(file=paste0(data_dir, "/UM00200M_Redacted FINAL.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE)

financial_info <- read.table(unz(paste0(data_dir, "/servus_largefiles.zip"),
                                 "AR00200t_FINAL.txt"),
                             sep=",", quote="", comment.char="",
                             fill=TRUE, header=TRUE, row.names=NULL)

colnames(financial_info) <- colnames(financial_info)[2:ncol(financial_info)]
financial_info <- financial_info[1:(ncol(financial_info)-1)]

bill_info <- read.table(file=paste0(data_dir, "/UM00260T_redacted_FINAL.txt"),
                        sep=",", quote="", comment.char="",
                        fill=TRUE, header=TRUE)

financial_assist <- read.table(file=paste0(data_dir, "/UM00232T_FINAL.txt"),
                               sep=",", quote="", comment.char="",
                               fill=TRUE, header=TRUE)

cutoff_info <- read.table(file=paste0(data_dir, "/RS00200M_CUTOF_redacted_FINAL.txt"),
                          sep=",", quote="", comment.char="",
                          fill=TRUE, header=TRUE)

reconnect_info <- read.table(file=paste0(data_dir, "/RS00200M_RCNCT_redacted_FINAL.txt"),
                             sep=",", quote="", comment.char="",
                             fill=TRUE, header=TRUE)

payment_arrangement <- read.table(file=paste0(data_dir, "/CO00200M_FINAL.txt"),
                                  sep=",", quote="", comment.char="",
                                  fill=TRUE, header=TRUE)

payment_arrangement_info <- read.table(file=paste0(data_dir, "/CO00210T_FINAL.txt"),
                                       sep=",", quote="", comment.char="",
                                       fill=TRUE, header=TRUE)

usage_info <- read.csv(unz(paste0(data_dir, "/servus_largefiles.zip"),
                           "UM00262T.csv"),
                       sep=",", quote="", comment.char="",
                       header=TRUE, row.names=NULL, stringsAsFactors = FALSE)

colnames(usage_info) <- colnames(usage_info)[2:ncol(usage_info)]
usage_info <- usage_info[1:(ncol(usage_info)-1)]

code_info <- read.table(file=paste0(data_dir, "/AR50100C_FINAL.txt"),
                        sep=",", quote="", comment.char="",
                        fill=TRUE, header=TRUE)

save(account_info, address_info, 
     geocode_address_info_subset,
     financial_info, bill_info, 
     location_relation, financial_assist,
     cutoff_info, reconnect_info,
     payment_arrangement, payment_arrangement_info,
     code_info,
     file=paste0(working_data_dir, "/analysis_info.RData"))

save(financial_info, usage_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info_large.RData.gz")))
  