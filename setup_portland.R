# Setup - Portland Data

data_all <- list.files(path=data_dir,
                       pattern="*.csv",
                       full.names=TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

location_relation <- read.table(file=paste0(data_dir, "/UM00120T_FINAL.txt"),
                                sep=",", quote="", comment.char="",
                                fill=TRUE, header=TRUE)

address_info <- read.table(file=paste0(data_dir, "/UM00100M_FINAL.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE)

account_info <- read.table(file=paste0(data_dir, "/UM00200M_Redacted FINAL.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE)

#financial_info <- read.table(file=paste0(data_dir, "/AR00200T_FINAL.txt"),
#                            sep=",", quote="", comment.char="",
#                            fill=TRUE, header=TRUE)

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

save(account_info, address_info, geocode_address_info_subset, bill_info, 
     location_relation, financial_assist,
     cutoff_info, reconnect_info,
     payment_arrangement, payment_arrangement_info,
     file=paste0(working_data_dir, "/analysis_info.RData"))
