# Set access key
accessKey <- "36d87abb2548485a59e01d3779c2ee90e05d96b5"

# Set dataset: current American Community Survey (ACS) dataset
baseurl <- "https://api.census.gov/data/2021/acs/acs1/profile?get=NAME,"

# Function to evaluate variable
evaluate_variable <- function(variable) {
  return(cbind(variable=as.character(variable), 
               variable_map_raw[[as.character(variable)]] %>% as.data.frame))
}

# Function to split temporary column
split_temp_column <- function(string, index) {
  tryCatch(
    return(string[[index]]), error=function(e){"N/A"}
  )
}


# Retrieve dataset variable legend ####

# Set variable url
variable_url <- "https://api.census.gov/data/2021/acs/acs1/profile/variables.json"

# Retrieve variables & build data frame
variable_map_raw <- fromJSON(rawToChar(
  GET(variable_url)[["content"]]))[["variables"]]
variable_map_list <- lapply(names(variable_map_raw), evaluate_variable)
list_data <- Map(as.data.frame, variable_map_list)
variable_map <- rbindlist(list_data, fill=TRUE)
variable_map <- variable_map %>% filter(str_detect(variable, "DP"))
variable_map <- variable_map %>% mutate(temp=strsplit(label, "!!"))

#Set variable Related Fields and Merge
type <- data.table(lapply(variable_map$temp, split_temp_column, index=1))
category <- data.table(lapply(variable_map$temp, split_temp_column, index=2))
metric_group <- data.table(lapply(variable_map$temp, split_temp_column, index=3))
metric <- data.table(lapply(variable_map$temp, split_temp_column, index=4))
submetric <- data.table(lapply(variable_map$temp, split_temp_column, index=5))
subsubmetric <- data.table(lapply(variable_map$temp, split_temp_column, index=6))
submetric = data.table(submetric, subsubmetric)
names(submetric) <- c("sub", "sub2")
submetric <- submetric %>%
  mutate(submetric=paste0(sub, "::", sub2)) %>% 
  select(submetric)

variable_map_test <- cbind(variable_map, type=type, category=category, 
                           metric_group=metric_group, metric=metric)
names(variable_map_test)[12:15]<- c("type", "category", "metric_group", "metric")

variable_map_test <- cbind(variable_map_test, submetric=submetric)
names(variable_map_test)[16] <- "submetric"

variable_map <- variable_map_test

variable_map_test <- variable_map %>% 
  mutate(metric=
           case_when(
             submetric=="N/A::N/A" ~ paste0(metric),
             TRUE ~ paste0(metric, "::", submetric)
           ))

variable_map_test <- variable_map_test %>% select(-submetric)
variable_map <- variable_map_test

save(variable_map, file=paste0(working_data_dir, "/acs_variables.RData"))

# Get the ACS Data ####
acs_tract <- get_acs(
  geography="tract",
  variables=variable_map$variable,
  key=accessKey,
  output="wide",
  state="OR",
  county=c("Multnomah", "Washington", "Clackamas")
)

acs_tract_geometry <- get_acs(
  geography="tract",
  variables=variable_map$variable,
  key=accessKey,
  output="wide",
  state="OR",
  county=c("Multnomah", "Washington", "Clackamas"),
  geometry=TRUE,
)
save(acs_tract, acs_tract_geometry,
     file=paste0(working_data_dir, "/acs_tract.RData"))


# Portland statistics by tract ####
tracts <- read.csv(file=paste0(auxiliary_data_dir, "/portland_geoid.csv"),
                   header=TRUE)$GEOID

portland_acs_tract <- acs_tract %>% 
  filter(GEOID %in% tracts) %>%
  pivot_longer(cols=starts_with("DP"),
               names_to="variable") %>% 
  filter(substr(variable, 5, 6)!="PR") %>% 
  mutate(group=substr(variable, 1, 9),
         stat=substring(variable, first=10)) %>% 
  filter(stat!="PM")  %>% 
  pivot_wider(names_from=stat, values_from=value) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(GEOID, group) %>% 
  summarise(estimate=max(E),
            percent_estimate=max(PE),
            moe=max(M)) %>% 
  ungroup() %>%
  mutate(lookup=paste0(group, "E")) %>% 
  left_join(variable_map %>% 
              filter(type=="Estimate") %>% 
              select(variable, category, metric_group, metric),
            by=c("lookup"="variable")) %>%
  select(-lookup)

# Portland tract demographics
demographic_stats <- c("DP02_0001", "DP02_0016", "DP03_0009",
                       "DP03_0063", "DP03_0065", "DP03_0069", "DP03_0071",
                       "DP03_0072", "DP03_0073", "DP03_0074",
                       "DP03_0119",
                       "DP05_0071", "DP05_0078")

portland_demographics_tract <- portland_acs_tract %>% 
  filter(group %in% demographic_stats)

metric_lookup <- data.table(
  group=c("DP02_0001", "DP02_0016", "DP03_0009",
          "DP03_0063", "DP03_0065", "DP03_0069", "DP03_0071",
          "DP03_0072", "DP03_0073", "DP03_0074",
          "DP03_0119",
          "DP05_0071", "DP05_0078"),
  metric_name=c("total_hh", "hh_size", "unemployment",
                "hh_income", "hh_earnings", "hh_retirement", "hh_ssi",
                "cash_assistance", "hh_cash_assistance", "food_stamp",
                "hh_poverty",
                "hispanic", "black"))

portland_demographics_tract <- portland_demographics_tract %>% 
  select(-c(category, metric_group, metric))

portland_demographics_tract <- portland_demographics_tract %>%
  left_join(metric_lookup)

portland_demographics_tract <- portland_demographics_tract %>% 
  select(GEOID, group, metric_name, estimate, percent_estimate)

portland_demographics_tract <- portland_demographics_tract %>% 
  group_by(GEOID, group, metric_name, estimate, percent_estimate) %>% 
  mutate(value=case_when(
    percent_estimate > 0 ~ percent_estimate,
    TRUE ~ estimate
  )) %>%
  ungroup()

portland_demographics_tract_wide <- portland_demographics_tract %>% 
  pivot_wider(id_cols=c("GEOID"),
              names_from=metric_name,
              values_from=value)

portland_demographics_tract_wide <- portland_demographics_tract_wide %>% 
  mutate(tract=substr(GEOID, 6, 12))

portland_demographics_tract_wide <- portland_demographics_tract_wide %>% 
  group_by(tract) %>% 
  summarise(total_hh=sum(total_hh),
            hh_size=mean(hh_size),
            unemployment=mean(unemployment),   
            hh_income=mean(hh_income),
            hh_earnings=mean(hh_earnings),
            hh_retirement=mean(hh_retirement),
            hh_ssi=mean(hh_ssi),
            cash_assistance=sum(cash_assistance),
            hh_cash_assistance=mean(hh_cash_assistance),
            food_stamp=sum(food_stamp),
            hh_poverty=mean(hh_poverty),
            hispanic=mean(hispanic),
            black=mean(black)) %>%
  ungroup()

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
  unique()

portland_demographics_tract_wide <-
  left_join(portland_demographics_tract_wide,
            life_expectancy_tract,
            by="tract") %>%
  unique()

save(portland_demographics_tract_wide,
     file=paste0(working_data_dir, "/portland_demographics_tract.RData"))
