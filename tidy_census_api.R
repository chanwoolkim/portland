# Set Access Key
accessKey <- "36d87abb2548485a59e01d3779c2ee90e05d96b5"

# Set Dataset: Current American Community Survey (ACS) Dataset
baseurl <- "https://api.census.gov/data/2021/acs/acs1/profile?get=NAME,"


# Get the ACS Data for All Zip Codes in the United States
if(!"acsByZip" %in% ls()){
  acsByZip <- get_acs(
    geography = "zcta",
    variables=VariableMap$Variable,
    key=accessKey,
    output = "wide"
  )
  # save(acsByZip,file="ACSData.RData")
}

if(!"acsByTract" %in% ls()){
  acsByTract <- get_acs(
    geography = "tract",
    variables=VariableMap$Variable,
    key=accessKey,
    output = "wide",
    state="OR"
  )
  save(acsByZip, acsByTract,file="ACSData.RData")
}

#############################
# if(!"acsByZip" %in% ls()){
#  out <- tryCatch(
#     {load("ACSData.RData")},
#     error=function(e){
#       message("Calling out to census API")
#       message(e)
#       acsByZip <- get_acs(
#         geography = "zcta",
#         variables=VariableMap$Variable,
#         key=accessKey,
#         output = "wide"
#   )
#   save(acsByZip,file="ACSData.RData")
#     }
#   )
#  return(out)
# }

```

```{r Load ACS}
if(!"acsByZip" %in% ls() | !"acsByTract" %in% ls()){
  load("ACSData.RData")
}
```

```{r Function: Evaluate Variable}
EvaluateVariable <- function(variable){
  return(cbind(Variable=as.character(variable), 
               variableMapRaw[[as.character(variable)]] %>% as.data.frame))
}
```

```{r Function: Split Temp Column}
SplitTempColumn <- function(string, index){
  
  tryCatch(
    return(string[[index]]), error=function(e){"N/A"}
  )
  
}
```

```{r Retrieve Dataset Variable Legend}

#Set Variable URL
variableURL <- "https://api.census.gov/data/2021/acs/acs1/profile/variables.json"

#Retrieve Variables & Build Data Frame
variableMapRaw <- fromJSON(rawToChar(
  GET(variableURL)[["content"]]))[["variables"]]
VariableMapList <- lapply(names(variableMapRaw), EvaluateVariable)
list_data <- Map(as.data.frame, VariableMapList)
VariableMap <- rbindlist(list_data, fill=TRUE)
VariableMap <- VariableMap %>% filter(str_detect(Variable, "DP"))
VariableMap <- VariableMap %>% mutate(temp=strsplit(label, "!!"))

#Set Variable Related Fields and Merge
Type <- data.table(lapply(VariableMap$temp, SplitTempColumn, index=1))
Category <- data.table(lapply(VariableMap$temp, SplitTempColumn, index=2))
MetricGroup <- data.table(lapply(VariableMap$temp, SplitTempColumn, index=3))
Metric <- data.table(lapply(VariableMap$temp, SplitTempColumn, index=4))
SubMetric <- data.table(lapply(VariableMap$temp, SplitTempColumn, index=5))
SubSubMetric <- data.table(lapply(VariableMap$temp, SplitTempColumn, index=6))
SubMetric = data.table(SubMetric, SubSubMetric)
names(SubMetric) <- c("Sub", "Sub2")
SubMetric <- SubMetric %>% mutate(SubMetric=paste0(Sub, "::", Sub2)) %>% 
  select(SubMetric)

VariableMapTest <- cbind(VariableMap, Type=Type, Category=Category, 
                         MetricGroup=MetricGroup, Metric=Metric)
names(VariableMapTest)[12:15]<- c("Type", "Category", "MetricGroup", "Metric")

VariableMapTest <- cbind(VariableMapTest, SubMetric=SubMetric)
names(VariableMapTest)[16] <- "SubMetric"


VariableMap <- VariableMapTest

VariableMapTest <- VariableMap %>% 
  mutate(Metric=
           case_when(
             SubMetric=="N/A::N/A" ~ paste0(Metric),
             TRUE ~ paste0(Metric,"::",SubMetric)
           ))

VariableMapTest <- VariableMapTest %>% select(-SubMetric)

VariableMap <- VariableMapTest

save(VariableMap, file="acsVariables.RData")

```

```{r Create Dataset for Portland Tracts}
if(!"acsByZip" %in% ls() | !"acsByTract" %in% ls()){
  load("ACSData.RData")
}

tracts <- unique(GeoCodedAddressInfo$census_tract)

PortlandACS_t <- acsByTract %>% 
  filter(substr(GEOID,6,12) %in% tracts) %>% 
  pivot_longer(
    cols = starts_with("DP"),
    names_to = "Variable"
  ) %>% 
  filter(substr(Variable,5,6)!="PR") %>% 
  mutate(Group=substr(Variable,1,9),
         Stat=substring(Variable, first=10)) %>% 
  filter(Stat!="PM")  %>% 
  pivot_wider(names_from = Stat, values_from = value) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(GEOID, Group) %>% 
  summarise(Estimate=max(E),
            PercentEstimate=max(PE),
            MoE=max(M)
            
  ) %>% 
  mutate(lookup=paste0(Group,"E")) %>% 
  left_join(VariableMap %>% 
              filter(Type=="Estimate") %>% 
              select(Variable, Category, MetricGroup, Metric),
            by=c("lookup" = "Variable")) %>% select(-lookup)

```

```{r Portland Tract Demographics}
DemographicStats <- c("DP02_0001","DP02_0016","DP03_0009", "DP03_0072", "DP03_0074",
                      "DP03_0063","DP03_0065","DP03_0065","DP03_0069",
                      "DP03_0071","DP03_0073", "DP03_0119")

PortlandDemographics_t <- PortlandACS_t %>% 
  filter(Group %in% DemographicStats) 

metriclookup <- data.table(
  Group=c("DP02_0001","DP02_0016","DP03_0009","DP03_0063","DP03_0065",
          "DP03_0069","DP03_0071","DP03_0072","DP03_0073","DP03_0074",
          "DP03_0119"),
  MetricName=c("Total_HH","Avg_HH_Size","Avg_Unemployment","Avg_HH_Income",
               "Avg_HH_Earnings","Avg_HH_Retirement","Avg_HH_SSI",
               "Cash_Assist_Percent","Avg_HH_Cash_Assist","FoodStamp_Percent",
               "Avg_HH_Poverty")
)

PortlandDemographics_t <- PortlandDemographics_t %>% 
  select(-c(Category, MetricGroup, Metric))

PortlandDemographics_t <- PortlandDemographics_t %>% left_join(metriclookup)

PortlandDemographics_t <- PortlandDemographics_t %>% 
  select(GEOID,Group, MetricName, Estimate, PercentEstimate)

PortlandDemographics_t <- PortlandDemographics_t %>% 
  group_by(GEOID, Group, MetricName, Estimate, PercentEstimate) %>% 
  mutate(value=case_when(
    PercentEstimate > 0 ~ PercentEstimate,
    TRUE ~ Estimate
  ))

PortlandDemographicsWide_t <- PortlandDemographics_t %>% 
  pivot_wider(id_cols = c("GEOID"),
              names_from = MetricName,
              values_from = value)

PortlandDemographicsWide_t <- PortlandDemographicsWide_t %>% 
  mutate(tract=substr(GEOID,6,12))

PortlandDemographicsWide_t <- PortlandDemographicsWide_t %>% 
  group_by(tract) %>% 
  summarise(
    Total_HH=sum(Total_HH),
    Avg_HH_Size=mean(Avg_HH_Size),
    Avg_Unemployment=mean(Avg_Unemployment),   
    Avg_HH_Income=mean(Avg_HH_Income),
    Avg_HH_Earnings=mean(Avg_HH_Earnings),
    Avg_HH_Retirement=mean(Avg_HH_Retirement),
    Avg_HH_SSI=mean(Avg_HH_SSI),
    Cash_Assist_Percent=sum(Cash_Assist_Percent),
    Avg_HH_Cash_Assist=mean(Avg_HH_Cash_Assist),
    FoodStamp_Percent=sum(FoodStamp_Percent),
    Avg_HH_Poverty=mean(Avg_HH_Poverty)
  )

save(PortlandDemographicsWide_t, file="PortlandDemographics.RData")

```

###############################################################################


##############################DRAFT#########################################
```{r Create Dataset for Portland Zip Codes}
PortlandACS <- acsByZip %>% 
  filter(GEOID %in% WaterBillData$ZipShort) %>% 
  pivot_longer(
    cols = starts_with("DP"),
    names_to = "Variable"
  ) %>% 
  filter(substr(Variable,5,6)!="PR") %>% 
  mutate(Group=substr(Variable,1,9),
         Stat=substring(Variable, first=10)) %>% 
  filter(Stat!="PM")  %>% 
  pivot_wider(names_from = Stat, values_from = value) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(GEOID, Group) %>% 
  summarise(Estimate=max(E),
            PercentEstimate=max(PE),
            MoE=max(M)
            
  ) %>% 
  mutate(lookup=paste0(Group,"E")) %>% 
  left_join(VariableMap %>% 
              filter(Type=="Estimate") %>% 
              select(Variable, Category, MetricGroup, Metric),
            by=c("lookup" = "Variable")) %>% select(-lookup)

```

```{r Portland Zip Demographics}
DemographicStats <- c("DP02_0001","DP02_0016","DP03_0009", "DP03_0072", "DP03_0074",
                      "DP03_0063","DP03_0065","DP03_0065","DP03_0069",
                      "DP03_0071","DP03_0073", "DP03_0119")

PortlandDemographics <- PortlandACS %>% 
  filter(Group %in% DemographicStats) 

metriclookup <- data.table(
  Group=c("DP02_0001","DP02_0016","DP03_0009","DP03_0063","DP03_0065",
          "DP03_0069","DP03_0071","DP03_0072","DP03_0073","DP03_0074",
          "DP03_0119"),
  MetricName=c("Total_HH","Avg_HH_Size","Avg_Unemployment","Avg_HH_Income",
               "Avg_HH_Earnings","Avg_HH_Retirement","Avg_HH_SSI",
               "Cash_Assist_Percent","Avg_HH_Cash_Assist","FoodStamp_Percent",
               "Avg_HH_Poverty")
)

PortlandDemographics <- PortlandDemographics %>% 
  select(-c(Category, MetricGroup, Metric))

PortlandDemographics <- PortlandDemographics %>% left_join(metriclookup)

PortlandDemographics <- PortlandDemographics %>% 
  select(GEOID,Group, MetricName, Estimate, PercentEstimate)

PortlandDemographics <- PortlandDemographics %>% 
  group_by(GEOID, Group, MetricName, Estimate, PercentEstimate) %>% 
  mutate(value=case_when(
    PercentEstimate > 0 ~ PercentEstimate,
    TRUE ~ Estimate
  ))

PortlandDemographicsWide <- PortlandDemographics %>% 
  pivot_wider(id_cols = c("GEOID"),
              names_from = MetricName,
              values_from = value)

```

```{r DRAFT...Create Portland Summaries}

relevantCategories <- c("EMPLOYMENT STATUS", "OCCUPATION", "CLASS OF WORKER", 
                        "INCOME AND BENEFITS (IN 2021 INFLATION-ADJUSTED DOLLARS)",
                        "PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL")

AvgMetrics <- c("DP03_0063","DP03_0065","DP03_0067","DP03_0069","DP03_0071",
                "DP03_0073","DP03_0087", "DP03_0119")

PortlandACSSummaries <- PortlandACS %>% 
  group_by(Group, Category, MetricGroup, Metric) %>% 
  summarise(Estimate=
              case_when(
                Group %in% AvgMetrics ~ mean(Estimate),
                TRUE ~ sum(Estimate)
              ),
            PercentEstimate=
              case_when(
                Group %in% AvgMetrics ~ mean(PercentEstimate),
                TRUE ~sum(PercentEstimate)
              )
  ) %>%
  filter(Category %in% relevantCategories | MetricGroup=="Total households")

topVariables <- c("DP02_0001","DP02_0153","DP03_0052","DP03_0053","DP03_0054",
                  "DP03_0055","DP03_0056","DP03_0057","DP03_0058","DP03_0059","DP03_0060",
                  "DP03_0061","DP03_0062","DP03_0063","DP03_0064","DP03_0065","DP03_0066",
                  "DP03_0067","DP03_0068","DP03_0069","DP03_0070","DP03_0071","DP03_0072",
                  "DP03_0073","DP03_0074","DP03_0075","DP03_0076","DP03_0077","DP03_0078",
                  "DP03_0079","DP03_0080","DP03_0081","DP03_0082","DP03_0083","DP03_0084",
                  "DP03_0085","DP03_0086","DP03_0087", "DP03_0119")

PortlandACSSummaries <- PortlandACSSummaries %>% filter(Group %in% topVariables)
names(PortlandACSSummaries)[5:6] <- c("PortlandEstimate", "PortlandPercentEstimate")

PortlandAnalysis <- inner_join(PortlandACS, PortlandACSSummaries %>% 
                                 select(Group, PortlandEstimate, PortlandPercentEstimate))


```

```{r DRAFT...Analyze Data}
PortlandAnalysis <- PortlandACS %>% filter(Group %in% topVariables)

IncomeStats <- c("DP03_0052","DP03_0053","DP03_0054","DP03_0055","DP03_0056",
                 "DP03_0057","DP03_0058","DP03_0059","DP03_0060","DP03_0061", "DP03_0119")

IncomeStats2 <- c("DP03_0063","DP03_0065","DP03_0065","DP03_0069",
                  "DP03_0071","DP03_0073")


IncomeAnalysis <- PortlandAnalysis %>% 
  filter(Group %in% IncomeStats) %>% 
  mutate(lowincome=case_when(
    Metric=="$100,000 to $149,999" ~ FALSE,
    Metric=="$150,000 to $199,999" ~ FALSE,
    Metric=="$200,000 or more" ~ FALSE,
    TRUE ~ TRUE
  )) %>% group_by(GEOID, lowincome) %>% 
  summarise(PercentEstimate=sum(PercentEstimate)) 


LowIncomeAnalysis <- IncomeAnalysis %>% 
  pivot_wider(names_from=lowincome, values_from=PercentEstimate) 

names(LowIncomeAnalysis) <- c("GEOID","highincome", "lowincome")

LowIncomeAnalysis <- LowIncomeAnalysis %>% arrange(desc(lowincome))

LowIncomeZips <- LowIncomeAnalysis[1:10,"GEOID"]

PovertyAnalysis <- PortlandAnalysis %>% 
  filter(Group=="DP03_0119") %>% 
  select(GEOID, PercentEstimate) %>% 
  mutate(AboveAvg=PercentEstimate>5.95641)

IncomeAndPoverty <- left_join(LowIncomeAnalysis, PovertyAnalysis)
names(IncomeAndPoverty)[4:5] <- c("PovertyPercentEstimate", "PovertyAboveAverage") 

save(PortlandACS, LowIncomeAnalysis, PovertyAnalysis, IncomeAndPoverty,
     file="LoadPortlandACSStats.RData")

```







```{r DELETE}


stateVariables <- seattleACS %>% filter(Category %in% relevantCategories) 
stateVariables <- unique(stateVariables$Group)

getWAacs <- get_acs(geography = "county",
                    variables = stateVariables,
                    state="WA",
                    key="36d87abb2548485a59e01d3779c2ee90e05d96b5",
                    output="wide")

seattlStats <- getWAacs %>% pivot_longer(
  cols = starts_with("DP"),
  names_to = "Variable") %>% 
  mutate(Group=substr(Variable,1,9),
         Stat=substring(Variable, first=10)) %>% 
  pivot_wider(names_from = Stat, values_from = value) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(GEOID, Group) %>% 
  summarise(StateEstimate=max(E),
            MoE=max(M)) %>% 
  select(Group,StateEstimate)


seattleTest <- inner_join(seattleACS, seattlStats,
                          by=c("Group"="Group"))


seattleACS <-  seattleACS %>% 
  mutate(Metric = case_when(
    is.na(SubMetric) ~ Metric,
    TRUE ~ paste0(Metric,"::",SubMetric)
  )
  ) %>% select(-SubMetric) %>% View()
```





