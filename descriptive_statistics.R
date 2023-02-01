
load(file=paste0(working_data_dir, "/analysis_info.RData"))


#Dependency: PortlandDemographicsWide_t
debt_dataset <- debt_dataset %>% 
  left_join(PortlandDemographicsWide_t %>% 
              select(tract, Total_HH, Avg_HH_Size, Avg_Unemployment, Avg_HH_Income, 
                     Avg_HH_Earnings,Avg_HH_Retirement,Avg_HH_SSI,
                     Cash_Assist_Percent,Avg_HH_Cash_Assist,FoodStamp_Percent,
                     Avg_HH_Poverty),
            by=c("census_tract"="tract"))
