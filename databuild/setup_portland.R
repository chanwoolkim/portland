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

collection_info <- read.table(file=paste0(data_dir, "/CO00400T_FINAL.txt"),
                              sep=",", quote="", comment.char="",
                              fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

collection_info_update <- read.table(file=paste0(data_dir, "/CO00400T 11012022-06222023.txt"),
                                     sep=",", quote="", comment.char="",
                                     fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

collection_info_update <- collection_info_update %>%
  select(-ADD_BY, -CHG_BY)

collection_info <- rbind(collection_info, collection_info_update) %>% distinct()

collection_amount <- read.table(file=paste0(data_dir, "/CO00450T_redacted_FINAL.TXT"),
                                sep=",", quote="", comment.char="",
                                fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

collection_amount_update <- read.table(file=paste0(data_dir, "/CO00450T 11012022-06222023.txt"),
                                       sep=",", quote="", comment.char="",
                                       fill=TRUE, header=TRUE, stringsAsFactors=FALSE)

collection_amount_update <- collection_amount_update %>%
  select(-PRIMARY_ADDR, -ADD_BY, -CHG_BY)

collection_amount <- rbind(collection_amount, collection_amount_update) %>% distinct()

usage_df_load <- function(filename) {
  df <- read.table(unz(paste0(data_dir, "/servus_largefiles.zip"),
                       paste0(filename, ".txt")),
                   sep=",", quote="", comment.char="",
                   fill=TRUE, header=TRUE, row.names=NULL, stringsAsFactors=FALSE)
  
  colnames(df) <- colnames(df)[2:ncol(df)]
  df <- df[1:(ncol(df)-1)]
  return(df)
}

usage_info <- rbind(usage_df_load("um00262t_01012019-12312019"),
                    usage_df_load("um00262t_01012020-12312020"),
                    usage_df_load("um00262t_01012021-12312021"),
                    usage_df_load("um00262t_01012022-12312022"),
                    usage_df_load("um00262t_01012023-06272023"))

code_info <- read.table(file=paste0(data_dir, "/AR50100C_FINAL.txt"),
                        sep=",", quote="", comment.char="",
                        fill=TRUE, header=TRUE, stringsAsFactors=FALSE)


# Fix comma issue ####
fix_comma <- function(df, main_var, num=FALSE) {
  loc_var <- which(colnames(df)=="U_VERSION")
  
  if (length(loc_var)==0) {
    return(df)
  } else {
    loc_comma <- which(df$U_VERSION=="")
    n_last <- ncol(df)
    df[loc_comma, loc_var:(n_last-1)] <- df[loc_comma, (loc_var+1):n_last]
    if (!num) {
      df <- df %>% filter(get(main_var)!="")
    } else {
      df <- df %>% filter(!grepl("\\D", get(main_var)), get(main_var)!="")
    }
    return(df)
  }
}

account_info <- fix_comma(account_info, "ACCOUNT_NO")
account_info <- account_info %>% filter(floor(log10(as.numeric(ACCOUNT_NO)))+1==10)
address_info <- fix_comma(address_info, "LOCATION_NO")
address_info <- address_info %>% filter(floor(log10(as.numeric(LOCATION_NO)))+1==10)
bill_info <- fix_comma(bill_info, "ACCOUNT_NO")
bill_info <- bill_info %>% filter(floor(log10(as.numeric(ACCOUNT_NO)))+1==10)
location_relation <- fix_comma(location_relation, "LOCATION_NO")
location_relation <- location_relation %>% filter(floor(log10(as.numeric(LOCATION_NO)))+1==10)
financial_assist <- fix_comma(financial_assist, "ACCOUNT_NO")
cutoff_info <- fix_comma(cutoff_info, "ACCOUNT_NO")
reconnect_info <- fix_comma(reconnect_info, "ACCOUNT_NO")
payment_arrangement <- fix_comma(payment_arrangement, "SS_ACCOUNT_NO")
payment_arrangement <- payment_arrangement %>% filter(floor(log10(as.numeric(SS_ACCOUNT_NO)))+1==10)
payment_arrangement_info <- payment_arrangement_info %>% filter(U_VERSION!="")
collection_info <- fix_comma(collection_info, "SS_ACCOUNT_NO")
collection_amount <- fix_comma(collection_amount, "SS_ACCOUNT_NO")
code_info <- fix_comma(code_info, "ITEM_TP")
financial_info <- fix_comma(financial_info, "SS_ACCOUNT_NO")
usage_info <- fix_comma(usage_info, "ACCOUNT_NO")


# Location info from financial info ####
location_financial <- financial_info %>%
  transmute(ACCOUNT_NO=SS_ACCOUNT_NO,
            DUE_DT=mdy(DUE_DT),
            LOCATION_NO=as.numeric(LOCATION_NO)) %>%
  filter(!is.na(LOCATION_NO)) %>%
  unique()


# Save ####
save(account_info, address_info, 
     geocode_address_info_subset,
     bill_info, 
     location_relation, financial_assist,
     cutoff_info, reconnect_info,
     payment_arrangement, payment_arrangement_info,
     collection_info, collection_amount,
     code_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info.RData.gz")))

save(financial_info, usage_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info_large.RData.gz")))

save(location_financial,
     file=paste0(working_data_dir, "/location_financial.RData"))
