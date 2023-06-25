#=========================================================================#
# setup_portland.R
#
# Load Raw Portland Data and save to Rdata format
#
# Chanwool Kim, Feb 20, 2023
#
#=========================================================================#


# Load data ####
location_relation <- read.table(file=paste0(data_dir, "/UM00120T.txt"),
                                sep=",", quote="", comment.char="",
                                fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

address_info <- read.table(file=paste0(data_dir, "/UM00100M.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

account_info <- read.table(file=paste0(data_dir, "/UM00200M.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

financial_info <- read.table(unz(paste0(data_dir, "/servus_largefiles.zip"),
                                 "AR00200t_FINAL.txt"),
                             sep=",", quote="", comment.char="",
                             fill=TRUE, header=TRUE, row.names=NULL, stringsAsFactors=FALSE)

colnames(financial_info) <- colnames(financial_info)[2:ncol(financial_info)]
financial_info <- financial_info[1:(ncol(financial_info)-1)]

financial_info_update <- read.table(unz(paste0(data_dir, "/servus_largefiles.zip"),
                                        "AR00200t_11012022-06222023.txt"),
                                    sep=",", quote="", comment.char="",
                                    fill=TRUE, header=TRUE, row.names=NULL, stringsAsFactors=FALSE)

colnames(financial_info_update) <- colnames(financial_info_update)[2:ncol(financial_info_update)]
financial_info_update <- financial_info_update[1:(ncol(financial_info_update)-1)]

financial_info_update <- financial_info_update %>%
  mutate(SOURCE_REFERENCE="",
         SOURCE_SPEC1="",
         SOURCE_SPEC2="",
         SOURCE_SPEC3="") %>%
  select(-ADD_BY, -CHG_BY, -ITEM_CMTS)

financial_info <- rbind(financial_info, financial_info_update) %>% distinct()

bill_info <- read.table(file=paste0(data_dir, "/UM00260T_redacted_FINAL.txt"),
                        sep=",", quote="", comment.char="",
                        fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

bill_info_update <- read.table(file=paste0(data_dir, "/UM00260T_11012022-06222023.txt"),
                               sep=",", quote="", comment.char="",
                               fill=TRUE, header=TRUE, row.names=NULL, stringsAsFactors=FALSE)

colnames(bill_info_update) <- colnames(bill_info_update)[2:ncol(bill_info_update)]
bill_info_update <- bill_info_update[1:(ncol(bill_info_update)-1)]

bill_info_update <- bill_info_update %>%
  mutate(ROUTE_PFX="",
         ADDR_ROUTE="",
         COUNTRY_CD="",
         BUILDING_NAME="",
         CONTACT_NO=NA) %>%
  select(-TITLE_CODE, -BANK_DRAFT_ACTIVE_DT, -BANK_DRAFT_CREATED_YN,
         -REPORT_ID, -ADD_BY, -CHG_BY, -ADDR_PERSON_NO)

bill_info <- rbind(bill_info, bill_info_update) %>% distinct()

financial_assist <- read.table(file=paste0(data_dir, "/UM00232T.txt"),
                               sep=",", quote="", comment.char="",
                               fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

cutoff_info <- read.table(file=paste0(data_dir, "/RS00200M_CUTOF_redacted_FINAL.txt"),
                          sep=",", quote="", comment.char="",
                          fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

cutoff_info_update <- read.table(file=paste0(data_dir, "/RS00200M_CUTOF 11012022-06222023.txt"),
                                 sep=",", quote="", comment.char="",
                                 fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

cutoff_info_update <- cutoff_info_update %>%
  select(-RESOLVED_BY, -OVERDUE_AMT, -ADD_BY, -CHG_BY, -ACTION_CMTS)

cutoff_info <- rbind(cutoff_info, cutoff_info_update) %>% distinct()

reconnect_info <- read.table(file=paste0(data_dir, "/RS00200M_RCNCT_redacted_FINAL.txt"),
                             sep=",", quote="", comment.char="",
                             fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

reconnect_info_update <- read.table(file=paste0(data_dir, "/RS00200M_RCNCT 11012022-06222023.txt"),
                                    sep=",", quote="", comment.char="",
                                    fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

reconnect_info_update <- reconnect_info_update %>%
  select(-RESOLVED_BY, -OVERDUE_AMT, -ADD_BY, -CHG_BY, -ACTION_CMTS)

reconnect_info <- rbind(reconnect_info, reconnect_info_update) %>% distinct()

payment_arrangement <- read.table(file=paste0(data_dir, "/CO00200M.txt"),
                                  sep=",", quote="", comment.char="",
                                  fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

payment_arrangement_info <- read.table(file=paste0(data_dir, "/CO00210T.txt"),
                                       sep=",", quote="", comment.char="",
                                       fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

usage_info <- read.csv(unz(paste0(data_dir, "/servus_largefiles.zip"),
                           "UM00262T.txt"),
                       sep=",", quote="", comment.char="",
                       fill=TRUE, header=TRUE, row.names=NULL, stringsAsFactors=FALSE)

colnames(usage_info) <- colnames(usage_info)[2:ncol(usage_info)]
usage_info <- usage_info[1:(ncol(usage_info)-1)]

code_info <- read.table(file=paste0(data_dir, "/AR50100C_FINAL.txt"),
                        sep=",", quote="", comment.char="",
                        fill=TRUE, header=TRUE, stringsAsFactors=FALSE)


# Fix comma issue ####
fix_comma <- function(df) {
  loc_var <- which(colnames(df)=="U_VERSION")
  
  if (length(loc_var)==0) {
    return(df)
  } else {
    loc_comma <- which(df$U_VERSION=="")[c(TRUE, FALSE)]
    n_last <- ncol(df)
    df[loc_comma, loc_var:(n_last-1)] <- df[loc_comma, (loc_var+1):n_last]
    df[loc_comma, n_last] <- df[(loc_comma+1), 1]
    df <- df[-(loc_comma+1),]
    return(df)
  }
}

account_info <- fix_comma(account_info)
address_info <- fix_comma(address_info)
bill_info <- fix_comma(bill_info)
location_relation <- fix_comma(location_relation)
financial_assist <- fix_comma(financial_assist)
cutoff_info <- fix_comma(cutoff_info)
reconnect_info <- fix_comma(reconnect_info)
payment_arrangement <- fix_comma(payment_arrangement)
payment_arrangement_info <- fix_comma(payment_arrangement_info)
code_info <- fix_comma(code_info)
financial_info <- fix_comma(financial_info)
usage_info <- fix_comma(usage_info)


# Save ####
save(account_info, address_info, 
     geocode_address_info_subset,
     bill_info, 
     location_relation, financial_assist,
     cutoff_info, reconnect_info,
     payment_arrangement, payment_arrangement_info,
     code_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info.RData.gz")))

save(financial_info, usage_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info_large.RData.gz")))
