# LOAD DATA
load(file=paste0(working_data_dir, "/portland_panel_estimation.RData"))
load(file=paste0(working_data_dir, "/tu_data.RData"))


# Precleaning ####
tu_data <- tu_data %>%
  select(tu_id, state, zip,
         credit_date, credit_score,
         hh_income_estimate, 
         ethnicity)

portland_transunion <- portland_panel_estimation %>%
  left_join(tu_data, by="tu_id")

# Save the dataset
save(portland_panel_estimation,
     file=paste0(working_data_dir, "/portland_panel_estimation.RData"))
