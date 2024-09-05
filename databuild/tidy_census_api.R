# Set access key
accessKey <- "36d87abb2548485a59e01d3779c2ee90e05d96b5"

# Set dataset: current American Community Survey (ACS) dataset
baseurl <- "https://api.census.gov/data/2021/acs/acs1/profile?get=NAME,"

# Census ####
variable_list_acs_2022 <- load_variables(2022, "acs5/profile")

demographic_stats <- c("DP05_0001E", "DP02_0001E", "DP02_0016E", "DP03_0009PE",
                       "DP03_0062E", "DP03_0092E", "DP03_0069E", "DP03_0071E",
                       "DP03_0072PE", "DP03_0073E", "DP03_0074PE",
                       "DP03_0119PE", "DP04_0058PE",
                       "DP05_0073PE", "DP05_0080PE", "DP05_0079PE")

demographic_stats_name <- c("total_n", "total_hh", "hh_size", "unemployment",
                            "hh_income", "hh_earnings", "hh_retirement", "hh_ssi",
                            "cash_assistance", "hh_cash_assistance", "food_stamp",
                            "hh_poverty", "hh_nocar",
                            "hispanic", "black", "white")


# Get the ACS Data ####
acs_tract <- get_acs(
  geography="tract",
  variables=demographic_stats,
  key=accessKey,
  output="wide",
  state="OR",
  county=c("Multnomah", "Washington", "Clackamas"),
  year=2022
)

acs_tract_geometry <- get_acs(
  geography="tract",
  variables=demographic_stats,
  key=accessKey,
  output="wide",
  state="OR",
  county=c("Multnomah", "Washington", "Clackamas"),
  year=2022,
  geometry=TRUE
)

save(acs_tract, acs_tract_geometry,
     file=paste0(working_data_dir, "/acs_tract.RData"))


# Portland statistics by tract ####
portland_demographics_tract_wide <- acs_tract %>% 
  mutate(tract=as.numeric(substr(GEOID, 7, 11))) %>%
  filter(tract!=80000) %>%
  select(-contains("M"), -GEOID) %>%
  rename_at(vars(demographic_stats), ~demographic_stats_name)

# Add in life expectancy info
life_expectancy <- read.csv(paste0(auxiliary_data_dir, "/US_A.CSV"),
                            header=TRUE) %>%
  select(tract_id=Tract.ID, life_expectancy=e.0.) %>%
  filter(grepl("^41", tract_id)) %>%
  mutate(tract_id=as.character(substr(tract_id, 6, 12)))

census_tract_change <- read.table(file=paste0(auxiliary_data_dir,
                                              "/tab20_tract20_tract10_st41.txt"),
                                  sep="|", quote="", comment.char="",
                                  fill=TRUE, header=TRUE) %>%
  mutate(GEOID_10=substr(GEOID_TRACT_10, 6, 12),
         GEOID_20=substr(GEOID_TRACT_20, 6, 12)) %>%
  select(GEOID_10, GEOID_20)

life_expectancy_tract <- left_join(life_expectancy,
                                   census_tract_change,
                                   by=c("tract_id"="GEOID_10")) %>%
  select(tract=GEOID_20, life_expectancy) %>%
  group_by(tract) %>%
  summarise(life_expectancy=mean(life_expectancy, na.rm=TRUE)) %>%
  distinct()

portland_demographics_tract_wide <-
  left_join(portland_demographics_tract_wide,
            life_expectancy_tract %>% 
              mutate(tract=as.numeric(tract)),
            by="tract") %>%
  distinct()

save(portland_demographics_tract_wide,
     file=paste0(working_data_dir, "/portland_demographics_tract.RData"))
