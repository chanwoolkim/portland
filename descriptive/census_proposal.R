# Census basic ####
# Regions SERVUS spoke with already
regions_to_load <- data.frame(county=c("Cook", "Suffolk", "Miami-Dade", 
                                       "Knox", "Bernalillo", "Hamilton", 
                                       "Lancaster", "Pierce", "Montgomery", "Dane"),
                              state=c("IL", "MA", "FL",
                                      "TN", "NM", "TN", 
                                      "PA", "WA", "MD", "WI"),
                              state_long=c("Illinois", "Massachusetts", "Florida",
                                           "Tennessee", "New Mexico", "Tennessee",
                                           "Pennsylvania", "Washington", "Maryland", "Wisconsin"))

variable_list_acs <- load_variables(2023, "acs5/profile")

# Load regions
# Load population characteristics for counties
acs_county <- data.frame()
varlist <- c("DP05_0001E", "DP02_0001", "DP04_0007")

for (state in regions_to_load %>% select(state) %>% distinct() %>% pull(state)) {
  acs_county_temp <- get_acs(
    geography="county",
    state=state,
    variables=varlist,
    output="wide",
    year=2023)
  
  acs_county <- rbind(acs_county, acs_county_temp)
}

acs_county <- acs_county %>%
  mutate(REGION=as.character(NAME)) %>%
  select(-contains("M")) %>%
  rename(total_population=DP05_0001E,
         total_household=DP02_0001E,
         total_single_family=DP04_0007E)

# Census summary for the regions of interest
census_results <- data.frame()

for (i in 1:nrow(regions_to_load)) {
  county <- regions_to_load$county[i]
  state_long <- regions_to_load$state_long[i]
  
  census_temp <- acs_county %>%
    filter(grepl(county, REGION),
           grepl(state_long, REGION)) %>%
    summarise(total_population=sum(total_population, na.rm=TRUE),
              total_household=sum(total_household, na.rm=TRUE),
              total_single_family=sum(total_single_family, na.rm=TRUE)) %>%
    ungroup()
  
  census_results <- rbind(census_results,
                          census_temp %>%
                            mutate(county=regions_to_load$county[i],
                                   state=regions_to_load$state[i]))
}

census_results <- census_results %>%
  arrange(desc(total_population)) %>%
  left_join(regions_to_load %>%
              select(state, state_long) %>%
              distinct(),
            by="state")

# Export results into table
tab <- TexRow(c("County",
                "State", 
                "Total Population", 
                "Total Households", 
                "Total Single Family Households")) +
  TexMidrule()

for (i in 1:nrow(census_results)) {
  tab <- tab +
    TexRow(c(str_to_title(letters[i]),
             census_results$state_long[i])) /
    TexRow(c(census_results$total_population[i],
             census_results$total_household[i],
             census_results$total_single_family[i]),
           dec=0)
}

TexSave(tab,
        filename=paste0(output_dir, "/tables/census_list"),
        positions=c("l", "l", "c", "c", "c"))
