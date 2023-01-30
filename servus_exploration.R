# Servus Data Exloration ####

if(!"WaterBillData" %in% ls()){
  WaterBillData <- read_csv("Portland water bill data.csv")
} 
```

```{r Update Data Set}
WaterBillData <- WaterBillData %>% mutate(
  ClassDescription = case_when(
    ACCOUNT_CLASS == "RESSF" ~ "Residential Single Family",
    ACCOUNT_CLASS == "RESMF" ~ "Residential Multi-Family",
    ACCOUNT_CLASS == "RESSF" ~ "Residential Single Family",
    ACCOUNT_CLASS == "ASST" ~ "Assist",
    ACCOUNT_CLASS == "COMM" ~ "Commercial",
    ACCOUNT_CLASS == "SWRLO" ~ "Sewer Only Owner",
    ACCOUNT_CLASS == "URGNT" ~ "Urgent",
    ACCOUNT_CLASS == "SWRLO" ~ "Sewer Only Owner",
    ACCOUNT_CLASS == "RMNDR" ~ "Reminder",
  ),
  ZipShort=substr(ZIP_CODE,1,5)
)
```
###############################################################################


###########################DRAFT############################################
```{r Key Metrics - Zip Code Analysis}

ZipAnalysis <- WaterBillData %>% group_by(ZipShort) %>% 
  summarize(TotalBalance = sum(BALANCE, na.rm = TRUE)) %>% 
  left_join(
    WaterBillData %>% mutate(PaymentPlans = as.integer(ACTIVE_PA=="Y"),
                             LincEnrollments = case_when(
                               is.na(LINC_ENROLLMENT) ~ as.integer(0),
                               TRUE ~ as.integer(1)
                             ),
                             Statements = as.integer(MSTMTS=="Y"),
                             MeterStatOff = case_when(
                               METER_STAT == "OFF" ~ as.integer(1),
                               TRUE ~ as.integer(0)
                             )) %>%
      group_by(ZipShort) %>% 
      summarise(Customers=n(),
                PaymentPlans=percent(
                  sum(PaymentPlans, na.rm = TRUE)/n(),accuracy=.01),
                LincEnrollments=percent(
                  sum(LincEnrollments, na.rm=TRUE)/n(),accuracy=.01),
                Statements=percent(
                  sum(Statements, na.rm=TRUE)/n(),accuracy=.01),
                MeterStatOff=percent(
                  sum(MeterStatOff, na.rm=TRUE)/n(), accuracy=.01)
      )
  ) %>% mutate(
    AvgDebt_Customer = dollar((TotalBalance/Customers), accuracy=.01)
  )



##Next Steps, housing profiles
##Export to CSV
##Load to Tableau 
##Visualize

```

```{r Import & Merge Income Data}
IncomeData <- read.csv("POR_INC.csv")
names(IncomeData) <- c('ZipShort','Median_HInc','Avg_HInc',
                       'PerCap_Inc','HIH_gr2k')
IncomeData <- IncomeData %>% mutate(ZipShort=as.character(ZipShort))

ZipAnalysis <- ZipAnalysis %>% left_join(IncomeData) %>% mutate(TotalBalance=dollar(TotalBalance))

```

```{r Export Datasets}

if(EnableExport){
  write_csv2(ZipAnalysis, "ZipAnalysisExport.csv")
  EnableExport <- FALSE
}

```