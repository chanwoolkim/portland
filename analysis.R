
#Account information with debt accumulation metrics
load("DebtDataSet.RData")

########## DEPENDENCIES ###############
load("PortlandDemographics.RData")
load("AnalysisInfo.RData")




###### OPTIONAL ############
#Load the ACS Variables Table
load("acsVariables.RData")

#Load the ACS Data
load("ACSData.RData")


####### REMOVED ########
# load("AnalysisInfo.RData")
# load("DebtAccumulationAccounts.RData")
# load("NoDebtData.RData")

# load("LoadPortlandACSStats.RData")
# #load("JoinFrames.RData")
# load("UpdatedJoinFrames.RData")
# tic()
#   load("FinalWorkingSet.RData")
#   load("GeocodedAddressInfo.RData")
# toc()
```

```{r Transform DebtDataSet for Regression v2.0}
#Dependency: AccountInfo
DebtDataSet <- DebtDataSet %>% 
  mutate(AccountKey=as.character(ACCOUNT_NO)) %>% 
  left_join(AccountInfo %>% select(ACCOUNT_NO, 
                                   CREDIT_RATING_CD),
            by=c("AccountKey"="ACCOUNT_NO"))


#Dependency: PortlandDemographicsWide_t
DebtDataSet <- DebtDataSet %>% 
  left_join(PortlandDemographicsWide_t %>% 
              select(tract, Total_HH, Avg_HH_Size, Avg_Unemployment, Avg_HH_Income, 
                     Avg_HH_Earnings,Avg_HH_Retirement,Avg_HH_SSI,
                     Cash_Assist_Percent,Avg_HH_Cash_Assist,FoodStamp_Percent,
                     Avg_HH_Poverty),
            by=c("census_tract"="tract"))


DebtDataSet <- DebtDataSet %>% 
  mutate(IncomeBracket=ntile(Avg_HH_Income,4),
         DebtAccumulator=billsAccumulated>1)

DebtDataSet <- DebtDataSet %>% mutate(
  CreditScore=case_when(
    !CREDIT_RATING_CD %in% (c("EXCEL", "GOOD ", "POOR ", "FAIR ")) ~ "UNKNWN",
    TRUE ~ CREDIT_RATING_CD
  )
)

```

```{r Tract Summaries}
PortlandSummary <- DebtDataSet %>% 
  summarise(Accounts=n(),
            AvgDebt=mean(AR_NET_AFTER_BILL),
            AvgBillsAcc=mean(billsAccumulated),
            TotalShutOffs=sum(hadShutOff),
            TotalPaymentPlans=sum(PaymentPlan)) %>% 
  mutate(ShutOffPercent=percent(TotalShutOffs/Accounts),
         PaymentPlanPercent=percent(TotalPaymentPlans/Accounts))

PortlandTractSummary <- DebtDataSet %>%
  group_by(census_tract) %>% 
  summarise(Accounts=n(),
            AvgDebt=mean(AccumDebt),
            AvgBillsAcc=mean(billsAccumulated),
            TotalShutOffs=sum(hadShutOff),
            TotalPaymentPlans=sum(PaymentPlan)) %>% 
  # filter(Accounts >100) %>% 
  mutate(ShutOffPercent=percent(TotalShutOffs/Accounts),
         PaymentPlanPercent=percent(TotalPaymentPlans/Accounts)) %>% 
  arrange(desc(Accounts))

#########Removed Code##########################
# PortlandZipSummary <- left_join(PortlandZipSummary, IncomeAndPoverty,
#                                 by=c("ZIP"="GEOID"))

# PortlandZipSummary <- PortlandZipSummary %>% 
#   mutate(highincome=highincome/100,
#          lowincome=lowincome/100,
#          PovertyPercentEstimate=PovertyPercentEstimate/100)

# PortlandZipSummary$POSTAL_CODE <- as.character(PortlandZipSummary$POSTAL_CODE)

# PortlandZipSummary <- PortlandZipSummary %>% 
#   select(-c(lowincome, highincome, PovertyPercentEstimate, PovertyAboveAverage))
######################

PortlandTractSummary <- PortlandTractSummary %>%
  left_join(PortlandDemographicsWide_t,
            by=c("census_tract"="tract")) %>% 
  arrange(desc(Accounts))

```

```{r Train & Test Split}
library(caTools)
sample <- sample.split(DebtDataSet$census_tract, SplitRatio = .7)
train <- subset(DebtDataSet, sample==TRUE)
test <- subset(DebtDataSet, sample==FALSE)
```

```{r Neural Net Analysis}
set.seed(1127)
library(nnet)
DebtDataSet_nn <- nnet(DebtAccumulator ~ LOCATION_CLASS  + 
                         hadShutOff + PaymentPlan + Avg_HH_Income + 
                         IncomeBracket + CreditScore + Avg_HH_Poverty, 
                       data=DebtDataSet,
                       size= 5, decay = 0.1, maxit=1000)

DebtDataSet_nn_pred <- predict(DebtDataSet_nn, type="raw")[,1]

DebtDataSet_analysis <- DebtDataSet %>% 
  mutate(score=DebtDataSet_nn_pred)


```

```{r Debt Data Analysis - Linear Regression}


# regressionFormula <- as.formula(billsAccumulated ~  Avg_HH_Size +
#                                   #Avg_Unemployment + 
#                                   Avg_HH_Income + 
#                                   Avg_HH_Earnings + 
#                                   #Avg_HH_Retirement + Avg_HH_SSI +
#                                   Cash_Assist_Percent + 
#                                   #Avg_HH_Cash_Assist + FoodStamp_Percent + 
#                                   Avg_HH_Poverty)



###############REGRESSION###############

regressionFormula <- as.formula(billsAccumulated ~ 
                                  Avg_HH_Income + Avg_Unemployment + Avg_HH_Poverty)



gc()
regression <- lm(regressionFormula, DebtDataSet)

summary(regression)

ggplot(DebtDataSet, aes(x = IncomeBracket, y = billsAccumulated)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")

```

```{r Debt Data Analysis - Logistic Regression}
library(caTools)
set.seed(1)

logitFormula <- as.formula(DebtAccumulator ~ Avg_HH_Income)
sample <- sample.split(DebtDataSet$DebtAccumulator, SplitRatio = .7)
train <- subset(DebtDataSet, sample==TRUE)
test <- subset(DebtDataSet, sample==FALSE)

debtLogit <- glm(logitFormula,
                 data=train,
                 family = "binomial")

test <- test %>% 
  mutate(prediction=predict.glm(
    debtLogit, 
    newdata = test, 
    type = "response"
  )) %>% 
  mutate(predictedDebtAccumulator=prediction>=.5,
         Accuracy=case_when(
           DebtAccumulator==predictedDebtAccumulator ~ "Hit",
           DebtAccumulator==TRUE & predictedDebtAccumulator==FALSE ~ "False Negative",
           TRUE ~ "False Positive"
         ))

test %>% group_by(Accuracy) %>% summarise(count=n()) %>% 
  mutate(pct=percent(count/sum(count))) %>% 
  View()

```

```{r}
plot(vs ~ hp, data=mtcars, col="steelblue")
lines(vs ~ hp, newdata, lwd=2)
```


```{r}
DebtDataSet2 <- DebtDataSet %>% filter(billsAccumulated>1)

sample <- sample.split(DebtDataSet2$PaymentPlan, SplitRatio = .7)
train <- subset(DebtDataSet2, sample==TRUE)
test <- subset(DebtDataSet2, sample==FALSE)

PaymentPlanFormula <- as.formula(billsAccumulated ~ PaymentPlan + IncomeBracket)
PaymentPlanLm <- lm(PaymentPlanFormula, data = train)

summary(PaymentPlanLm)



ggplot(DebtDataSet2, aes(x = PaymentPlan, y = billsAccumulated)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")

```


###########DRAFT#################
```{r}

NonComplianceAsFoFZip <- as.formula(billsAccumulated ~ ZIP)

NonComplianceAsFoFZip_lm <- lm(NonComplianceAsFoFZip, DebtDataSet)

summary(NonComplianceAsFoFZip_lm)


DebtDataSet %>% 
  ggplot(aes(x = Avg_HH_Income, y = AR_NET_AFTER_BILL)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")

```

```{r}
AvgBillsAccumulated <- DebtDataSet %>% filter(billsAccumulated>0) %>% 
  group_by(PaymentPlan) %>% summarise(mean(billsAccumulated ))

FinAssistDT <- data.table(FinancialAssist)
```

```{r}
DebtFormula <- as.formula(AR_NET_AFTER_BILL ~ PovertyPercentEstimate +
                            lowincome + highincome)




ggplot(DebtDataSet, aes(x = lowincome, y = billsAccumulated)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")


ggplot(DebtDataSet, aes(x = PovertyAboveAverage, y = billsAccumulated)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")



ggplot(DebtDataSet %>% filter(billsAccumulated>0), aes(x = PaymentPlan, y = billsAccumulated)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")



ggplot(DebtAccumulationPersons, aes(x = AR_NET_AFTER_BILL, y = billsAccumulated)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")




set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(DebtDataSet), replace=TRUE,
                 prob=c(0.7,0.3))

DebtTrain <- DebtDataSet[sample, ]
DebtTest <- DebtDataSet[!sample, ]  

shutOffLogit <- glm(hadShutOff ~ PovertyAboveAverage +
                      lowincome,
                    data=DebtTrain,
                    family = "binomial")

PaymentPlanLogit <- glm()

summary(shutOffLogit)
pscl::pR2(shutOffLogit)["McFadden"]

# DebtTest <- DebtTest %>% mutate(prediction=


prediction <- data.frame(predict(shutOffLogit, DebtTest, type="response"))
DebtTest <- cbind(DebtTest, prediction)
names(DebtTest)[16]<- "Prediction"



DebtTest <- DebtTest %>% mutate(predShutOff=case_when(
  Prediction >= .5 ~ TRUE,
  TRUE ~ FALSE
)) %>% mutate(result=hadShutOff==predShutOff)


plotROC(DebtTest$hadShutOff, prediction)

```


###########DEPRECATED#################
```{r Location Lookup Data Table}
LocationLookup <- LocationRelation %>% 
  select(LOCATION_NO, PERSON_NO, REL_TP, EFFECTIVE_DT, ACTUAL_END_DT, OWNER_YN,
         ACCT_TO_FRC_CONNECT)

LocationLookup <- LocationLookup %>% 
  mutate(EFFECTIVE_YEAR=as.integer(str_sub(EFFECTIVE_DT,start=-4)),
         END_YEAR=as.integer(str_sub(ACTUAL_END_DT,start=-4))) 

effdates <- unique(LocationLookup$EFFECTIVE_DT)
enddates <- unique(LocationLookup$ACTUAL_END_DT)

effdates <- data.frame(effdates, effRef=mdy(effdates))
enddates <- data.frame(enddates, endRef=mdy(enddates))


LocationLookup <- left_join(LocationLookup, effdates, by=c("EFFECTIVE_DT"="effdates"))
LocationLookup <- left_join(LocationLookup, enddates, by=c("ACTUAL_END_DT"="enddates"))

LocationLookup <- LocationLookup %>% 
  mutate(endRef=case_when(
    is.na(endRef) ~ mdy("1/1/2100"),
    TRUE ~ endRef
  )) %>% filter(EFFECTIVE_YEAR > 1900)

LocationLookup <- LocationLookup %>% 
  mutate(LocationKey=paste0(LOCATION_NO, PERSON_NO))

save(LocationLookup, file="LocationLookup.RData")

```

```{r Location Lookup Frame Gen}
dt <- data.table(DebtAccumulationAccounts, key="PERSON_NO")
dt2 <- data.table(LocationLookup, key="PERSON_NO")

lookupLocation <- function(personNo, accountNo, dateCheck){
  temp <- dt2[PERSON_NO==personNo & ACCT_TO_FRC_CONNECT==accountNo &
                between(dateCheck, effRef, endRef)==TRUE]$LOCATION_NO
  if(!is.null(temp)){
    return(temp)
  }else{
    return("N/A")
  }
  
}

lookupFrame <- data.frame(PERSON_NO=DebtAccumulationAccounts$PERSON_NO,
                          DateCheck=DebtAccumulationAccounts$dcf,
                          ACCOUNT_NO=DebtAccumulationAccounts$ACCOUNT_NO)


NoDebtPersonBills <- data.table(FilteredBillInfo)[!(PERSON_NO %in% unique(DebtAccumulationPersons$PERSON_NO))]


NoDebtPersonAccounts <- NoDebtPersonBills %>% 
  group_by(ACCOUNT_NO, PERSON_NO) %>% 
  summarise(billsAccumulated=0,
            AccumDebt=sum(BILL_GENERATED_CHGS),
            AR_NET_AFTER_BILL=max(AR_NET_AFTER_BILL),
            check=max(PREV_BILL_AMT)+max(BILL_GENERATED_CHGS),
            DateCheck=max(PERIOD_TO_DT)) 


datechecks2 <- data.table(DateCheck=unique(NoDebtPersonAccounts$DateCheck)) %>% 
  mutate(dcf=mdy(DateCheck))

NoDebtPersonAccounts <- left_join(NoDebtPersonAccounts, datechecks2)


lookupFrame2 <- data.frame(PERSON_NO=NoDebtPersonAccounts$PERSON_NO,
                           DateCheck=NoDebtPersonAccounts$dcf,
                           ACCOUNT_NO=NoDebtPersonAccounts$ACCOUNT_NO)

```

```{r Create Join Frames}

#####  THIS BLOCK TAKES ROUGHLY 8 HOURS TO RUN ######
tic()
joinFrame <- data.table(AccountNo=lookupFrame$ACCOUNT_NO,
                        personNo=lookupFrame$PERSON_NO,
                        DateCheck=lookupFrame$DateCheck,
                        LocationNo=mapply(lookupLocation, 
                                          personNo=lookupFrame$PERSON_NO,
                                          dateCheck=lookupFrame$DateCheck,
                                          accountNo=lookupFrame$ACCOUNT_NO,
                                          SIMPLIFY = TRUE))

joinFrame <- joinFrame %>% 
  mutate(Key=paste0(AccountNo, personNo))
toc()



tic()
joinFrame2 <- data.table(AccountNo=lookupFrame2$ACCOUNT_NO,
                         personNo=lookupFrame2$PERSON_NO,
                         DateCheck=lookupFrame2$DateCheck,
                         LocationNo=mapply(lookupLocation, 
                                           personNo=lookupFrame2$PERSON_NO,
                                           dateCheck=lookupFrame2$DateCheck,
                                           accountNo=lookupFrame2$ACCOUNT_NO,
                                           SIMPLIFY = TRUE))

joinFrame2 <- joinFrame2 %>% 
  mutate(Key=paste0(AccountNo, personNo))
toc()

save(joinFrame, joinFrame2, file="JoinFrames.RData")  

```

```{r Debt Accumulation Data}
######REMOVED#########
# DebtAccumulationPersons <- left_join(DebtAccumulationAccounts,
#                    joinFrame %>% select(Key, LocationNo),
#                    by=c("Key"="Key"))

# DebtAccumulationPersons$LocationNo <- as.character(DebtAccumulationPersons$LocationNo)




DebtAccumulationPersons <- DebtAccumulationPersons %>% 
  left_join(unique(AddressInfo %>% 
                     mutate(ZIP=substring(POSTAL_CODE,1,5)) %>% 
                     select(LOCATION_NO, ZIP)),
            by=c("LocationNo"="LOCATION_NO"))

# DebtAccumulationPersons <- left_join(DebtAccumulationPersons,
#                                      LowIncomeAnalysis,
#                                      by=c("ZIP"="GEOID"))
# 
# 
# DebtAccumulationPersons <- DebtAccumulationPersons %>% 
#   arrange(desc(lowincome))
# 
# DebtAccumulationPersons <- DebtAccumulationPersons %>% 
#   left_join(PovertyAnalysis, by=c("ZIP"="GEOID"))

DebtAccumulationPersons <- 
  data.table(DebtAccumulationPersons)[LocationNo !="character(0)"] 

# names(DebtAccumulationPersons)[13] <- "PovertyPercentEstimate"
# names(DebtAccumulationPersons)[14] <- "PovertyAboveAverage"

DebtAccumulationPersons <- DebtAccumulationPersons %>% 
  mutate(hadShutOff=(
    PERSON_NO %in% ShutOffInfo$PERSON_NO
  ))


save(DebtAccumulationPersons, DebtAccumulationAccounts, DebtAccumulationBills, joinFrame,
     file="DebtAccumulationAccounts.RData")
```

```{r No Debt Accumulation Data}
NoDebtPersons <- left_join(NoDebtPersonAccounts,
                           joinFrame2 %>% select(Key,LocationNo),
                           by=c("Key"="Key")) 

NoDebtPersons$LocationNo <- as.character(NoDebtPersons$LocationNo)


NoDebtPersons <- NoDebtPersons %>% 
  left_join(unique(AddressInfo %>% 
                     mutate(ZIP=substring(POSTAL_CODE,1,5)) %>% 
                     select(LOCATION_NO, ZIP)),
            by=c("LocationNo"="LOCATION_NO"))

# NoDebtPersons <- left_join(NoDebtPersons,
#                                      LowIncomeAnalysis,
#                                      by=c("ZIP"="GEOID"))
# 
# 
# NoDebtPersons <- NoDebtPersons %>% 
#   arrange(desc(lowincome))
# 
# NoDebtPersons <- NoDebtPersons %>% 
#   left_join(PovertyAnalysis, by=c("ZIP"="GEOID"))
# 
# NoDebtPersons <- 
#   data.table(NoDebtPersons)[LocationNo !="character(0)"] 
# 
# names(NoDebtPersons)[13] <- "PovertyPercentEstimate"
# names(NoDebtPersons)[14] <- "PovertyAboveAverage"

NoDebtPersons <- NoDebtPersons %>% 
  mutate(hadShutOff=(
    PERSON_NO %in% ShutOffInfo$PERSON_NO
  ))


save(NoDebtPersons, NoDebtPersonAccounts, NoDebtPersonBills, joinFrame2,
     file="NoDebtData.RData")

```

```{r Zip Summaries}
PortlandSummary <- DebtDataSet %>% 
  summarise(Accounts=n(),
            AvgDebt=mean(AR_NET_AFTER_BILL),
            AvgBillsAcc=mean(billsAccumulated),
            TotalShutOffs=sum(hadShutOff),
            TotalPaymentPlans=sum(PaymentPlan)) %>% 
  mutate(ShutOffPercent=percent(TotalShutOffs/Accounts),
         PaymentPlanPercent=percent(TotalPaymentPlans/Accounts))

PortlandZipSummary <- DebtDataSet %>% 
  select(-c(highincome, PovertyPercentEstimate,
            PovertyAboveAverage)) %>%
  group_by(ZIP) %>% 
  summarise(Accounts=n(),
            AvgDebt=mean(AR_NET_AFTER_BILL),
            AvgBillsAcc=mean(billsAccumulated),
            TotalShutOffs=sum(hadShutOff),
            TotalPaymentPlans=sum(PaymentPlan)) %>% 
  filter(Accounts >100) %>% 
  mutate(ShutOffPercent=percent(TotalShutOffs/Accounts),
         PaymentPlanPercent=percent(TotalPaymentPlans/Accounts)) %>% 
  arrange(desc(Accounts))

#########Removed Code##############
# PortlandZipSummary <- left_join(PortlandZipSummary, IncomeAndPoverty,
#                                 by=c("ZIP"="GEOID"))

# PortlandZipSummary <- PortlandZipSummary %>% 
#   mutate(highincome=highincome/100,
#          lowincome=lowincome/100,
#          PovertyPercentEstimate=PovertyPercentEstimate/100)

# PortlandZipSummary$POSTAL_CODE <- as.character(PortlandZipSummary$POSTAL_CODE)

# PortlandZipSummary <- PortlandZipSummary %>% 
#   select(-c(lowincome, highincome, PovertyPercentEstimate, PovertyAboveAverage))
######################

PortlandZipSummary <- PortlandZipSummary %>%
  left_join(PortlandDemographicsWide,
            by=c("ZIP"="GEOID")) %>% 
  arrange(desc(Accounts))

```

```{r Transform DebtDataSet for Regression}
if(!"LocationLookup" %in% ls()){
  load("LocationLookup.RData")
}

DebtDataSet <- DebtDataSet %>% 
  mutate(LocationKey=paste0(LocationNo,PERSON_NO)) %>% 
  left_join(unique(LocationLookup %>% 
                     mutate(LocationKey=paste0(LOCATION_NO, PERSON_NO)) %>% 
                     select(LocationKey, REL_TP)))



DebtDataSet <- DebtDataSet %>% 
  mutate(AccountKey=as.character(ACCOUNT_NO)) %>% 
  left_join(AccountInfo %>% select(ACCOUNT_NO, ACCOUNT_CLASS_DFLT,
                                   CREDIT_RATING_CD),
            by=c("AccountKey"="ACCOUNT_NO"))


if("highincome" %in% names(DebtDataSet)){
  DebtDataSet <- DebtDataSet %>% 
    select(-c(highincome, PovertyPercentEstimate, PovertyAboveAverage, AboveAvg))
}

DebtDataSet <- DebtDataSet %>% 
  left_join(GeoCodedAddressInfo %>% select(LOCATION_NO, census_tract),
            by=c("LocationNo"="LOCATION_NO")) %>% 
  left_join(PortlandDemographicsWide_t %>% 
              select(tract, Total_HH, Avg_HH_Size, Avg_Unemployment, Avg_HH_Income, 
                     Avg_HH_Earnings,Avg_HH_Retirement,Avg_HH_SSI,
                     Cash_Assist_Percent,Avg_HH_Cash_Assist,FoodStamp_Percent,
                     Avg_HH_Poverty),
            by=c("census_tract"="tract"))

DebtDataSet$ACCOUNT_NO <- as.character(DebtDataSet$ACCOUNT_NO)

DebtDataSet <- DebtDataSet %>% 
  left_join(AccountInfo %>% 
              select(ACCOUNT_NO, ACCOUNT_CLASS_DFLT, CREDIT_RATING_CD))




NoTract <- DebtDataSet %>% filter(is.na(census_tract))
NoTract <- NoTract[,1:20]
NoTract <- NoTract %>% 
  left_join(PortlandDemographicsWide,
            by=c("ZIP"="GEOID"))

DebtDataSet <- DebtDataSet %>% filter(!is.na(census_tract))

DebtDataSet <- rbind(DebtDataSet, NoTract)

DebtDataSet <- DebtDataSet %>% 
  mutate(IncomeBracket=ntile(Avg_HH_Income,4),
         DebtAccumulator=billsAccumulated>1)

DebtDataSet <- DebtDataSet %>% mutate(
  CreditScore=case_when(
    !CREDIT_RATING_CD %in% (c("EXCEL", "GOOD ", "POOR ", "FAIR ")) ~ "UNKNWN",
    TRUE ~ CREDIT_RATING_CD
  )
)

```
