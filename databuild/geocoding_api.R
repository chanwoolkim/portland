# Geocode API

# Load address info from Portland data
address_info <- read.table(file=paste0(data_dir, "/UM00100M_FINAL.txt"),
                           sep=",", quote="", comment.char="",
                           fill=TRUE, header=TRUE)

address_info <- address_info %>%
  mutate(address=paste(
    trimws(HOUSE_NO),
    trimws(STREET_PFX_DIR),
    trimws(STREET_NM),
    trimws(STREET_NM_SFX),
    sep=" "))

# Pass in a df - geocode function values should match df column names
geocode_address <- function(df) {
  geotemp <- geocode(
    df,
    street=address,
    city=CITY,
    state=PROVINCE_CD,
    postalcode=POSTAL_CODE,
    method="census",
    full_results=TRUE,
    api_options=list(census_return_type="geographies"),
    batch_limit=10000
  )
  return(data.table(geotemp))
}

# Approximately 90 seconds per batch
address_list <- list(address_info[1:10000,],
                     address_info[10001:20000,],
                     address_info[20001:30000,],
                     address_info[30001:40000,],
                     address_info[40001:50000,],
                     address_info[50001:60000,],
                     address_info[60001:70000,],
                     address_info[70001:80000,],
                     address_info[80001:90000,],
                     address_info[90001:100000,],
                     address_info[100001:110000,],
                     address_info[110001:120000,],
                     address_info[120001:130000,],
                     address_info[130001:140000,],
                     address_info[140001:150000,],
                     address_info[150001:160000,],
                     address_info[160001:170000,],
                     address_info[170001:180000,],
                     address_info[180001:190000,],
                     address_info[190001:length(address_info$LOCATION_NO),]
)

# Execute
geocode_address_info <- do.call(rbind,
                                lapply(address_list,
                                       FUN=geocode_address))

# Save and select
save(geocode_address_info,
     file=paste0(working_data_dir, "/geocode_address_info.RData"))

geocode_address_info_subset <- geocode_address_info %>% 
  select(LOCATION_NO, LOCATION_CLASS, LOCATION_STAT, NBR_OF_UNITS,
         HOUSE_NO, STREET_PFX_DIR, STREET_NM, STREET_NM_SFX,
         CITY, PROVINCE_CD, POSTAL_CODE,
         address, lat, long, id, input_address,
         match_indicator, match_type, matched_address,
         tiger_line_id, tiger_side,
         state_fips, county_fips, census_tract, census_block)

save(geocode_address_info_subset,
     file=paste0(working_data_dir, "/geocode_address_info_subset.RData"))
