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

tu_data <- tu_data %>%
  transmute(state=customerInput_state,
            zip=customerInput_zip,
            state_mail=customerInput_state1,
            zip_mail=customerInput_zip1,
            tu_id=customerInput_tusequencenumber %>% as.numeric(),
            tu_permid=customerInput_customerInputPermId,
            credit_date=creditAsOfDate_creditAsOfDate,
            credit_score=cvtg03_finscore,
            advrs1=cvtg03_advrs1,
            advrs2=cvtg03_advrs2,
            advrs3=cvtg03_advrs3,
            advrs4=cvtg03_advrs4,
            edie=edie04_score,
            etie=etie04_score,
            hh_income_estimate=Estimated_HH_Income,
            ethnicity=Ethnicity_Group) %>%
  distinct()


# Save ####
save(tu_data,
     file=paste0(working_data_dir, "/tu_data.RData"))
