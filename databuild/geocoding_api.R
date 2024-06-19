# Geocode API

# Load address info from Portland data
load(file=paste0(working_data_dir, "/address_info.RData"))

address_info <- address_info %>%
  mutate(address=paste(
    trimws(house_number),
    trimws(street_prefix_dir),
    trimws(street_name),
    trimws(street_suffix_dir),
    sep=" "))

# Pass in a df - geocode function values should match df column names
geocode_address <- function(df) {
  geotemp <- geocode(
    df,
    street=address,
    city=city,
    state=state,
    postalcode=postal_code,
    method="census",
    full_results=TRUE,
    api_options=list(census_return_type="geographies"),
    batch_limit=10000
  )
  return(geotemp)
}

# Approximately 90 seconds per batch
geocode_address_info <- data.frame()
num_batches <- nrow(address_info) %/% 10000+1

for (i in 1:num_batches) {
  start <- (i-1)*10000+1
  end <- min(i*10000, nrow(address_info))
  temp <- geocode_address(address_info[start:end,])
  geocode_address_info <- rbind(geocode_address_info, temp)
  print(paste("Batch ", i, " of ", num_batches, " completed"))
}

# Save and select
save(geocode_address_info,
     file=paste0(working_data_dir, "/geocode_address_info.RData"))

geocode_address_info_subset <- geocode_address_info %>% 
  select(location_number, occupancy_code, is_active, number_units,
         house_number, street_prefix_dir, street_name, street_suffix_dir,
         city, state, postal_code,
         address, lat, long, id, input_address,
         match_indicator, match_type, matched_address,
         tiger_line_id, tiger_side,
         state_fips, county_fips, census_tract, census_block)

save(geocode_address_info_subset,
     file=paste0(working_data_dir, "/geocode_address_info_subset.RData"))
