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
            by=c("customerInput_tusequencenumber"="IN1"))

tu_data <- tu_data %>%
  transmute(state=customerInput_state,
            zip=customerInput_zip,
            state_mail=customerInput_state1,
            zip_mail=customerInput_zip1,
            tu_id=customerInput_tusequencenumber %>% as.numeric(),
            tu_permid=customerInput_customerInputPermId,
            credit_date=creditAsOfDate_creditAsOfDate,
            credit_score=cvtg03_finscore %>% as.numeric(),
            advrs1=cvtg03_advrs1 %>% as.numeric(),
            advrs2=cvtg03_advrs2 %>% as.numeric(),
            advrs3=cvtg03_advrs3 %>% as.numeric(),
            advrs4=cvtg03_advrs4 %>% as.numeric(),
            edie=edie04_score %>% as.numeric(),
            etie=etie04_score %>% as.numeric(),
            hh_income_estimate=EstHHIncome %>% as.numeric(),
            ethnicity=case_when(
              !is.na(GroupD) ~ 1,
              !is.na(GroupB) ~ 0,
              .default=NA)) %>%
  distinct()


# Save ####
save(tu_data,
     file=paste0(working_data_dir, "/tu_data.RData"))
