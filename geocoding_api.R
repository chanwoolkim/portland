# Geocode API ####

# Load address info from Portland data
AddressInfo <- read.table(file=paste0(data_dir, "/UM00100M_FINAL.txt"),
                          sep=",", quote="", comment.char="",
                          fill=TRUE, header=TRUE)

AddressInfo <- AddressInfo %>% 
  mutate(address=paste(
    trimws(HOUSE_NO), 
    trimws(STREET_PFX_DIR),
    trimws(STREET_NM),
    trimws(STREET_NM_SFX),
    sep=" "))

# Pass in a df - geocode function values should match df column names
GeocodeAddresses <- function(x){
  geotemp <- geocode(
    x,
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
dfList <- list(AddressInfo[1:10000,],
               AddressInfo[10001:20000,],
               AddressInfo[20001:30000,],
               AddressInfo[30001:40000,],
               AddressInfo[40001:50000,],
               AddressInfo[50001:60000,],
               AddressInfo[60001:70000,],
               AddressInfo[70001:80000,],
               AddressInfo[80001:90000,],
               AddressInfo[90001:100000,],
               AddressInfo[100001:110000,],
               AddressInfo[110001:120000,],
               AddressInfo[120001:130000,],
               AddressInfo[130001:140000,],
               AddressInfo[140001:150000,],
               AddressInfo[150001:160000,],
               AddressInfo[160001:170000,],
               AddressInfo[170001:180000,],
               AddressInfo[180001:190000,],
               AddressInfo[190001:length(AddressInfo$LOCATION_NO),]
)

# Execute
GeoCodedAddressInfo <- do.call(rbind,
                               lapply(dfList,
                                      FUN=GeocodeAddresses))

# Save and select
save(GeoCodedAddressInfo,
     file=paste0(working_data_dir, "/GeocodedAddressInfo.RData"))

GeoCodedAddressInfo_subset <- GeoCodedAddressInfo %>% 
  select(LOCATION_NO, LOCATION_CLASS, LOCATION_STAT, NBR_OF_UNITS,
         HOUSE_NO, STREET_PFX_DIR, STREET_NM, STREET_NM_SFX, CITY, PROVINCE_CD,
         POSTAL_CODE, address, lat, long, id, input_address, match_indicator, 
         match_type, matched_address, tiger_line_id, tiger_side, state_fips, 
         county_fips, census_tract, census_block)
