# Load data ####

tu_data <- list.files(tu_data_dir, recursive=TRUE, full.names=TRUE) %>% 
  str_subset("CSV") %>%
  read_delim(delim="|") %>%
  bind_rows

tu_demographic <- list.files(tu_data_dir, recursive=TRUE, full.names=TRUE) %>% 
  str_subset("PSV") %>%
  read_delim(delim="|") %>%
  bind_rows

tu_data <- tu_data %>%
  left_join(tu_demographic,
            by=c("customerInput_tusequencenumber"="customerInput_tusequencenumber0029",
                 "customerInput_customerInputPermId"="PermID"))


# Save ####
save(address_info,
     file=paste0(working_data_dir, "/address_info.RData"))

save(account_info, address_info,
     location_relation, location_account_relation,
     bill_info, financial_assist, financial_assist_detail,
     cutoff_info,
     payment_arrangement, payment_arrangement_info,
     collection_info, collection_amount,
     code_info,
     file=paste0(working_data_dir, "/analysis_info.RData"))

save(financial_info, usage_info,
     file=gzfile(paste0(working_data_dir, "/analysis_info_large.RData.gz")))

save(location_financial,
     file=paste0(working_data_dir, "/location_financial.RData"))
