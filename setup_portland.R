# Setup - Portland Data ####

data_all <- list.files(path=data_dir,
                       pattern="*.csv",
                       full.names=TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows


DebtAccumulation_5p <- data_all %>% 
  mutate(DebtTier=ntile(NET_BILL_AMT, 20)) %>% 
  filter(DebtTier==20)

LocationRelation <- read.table(file=paste0(data_dir, "/UM00120T_FINAL.txt"),
                               sep=",", quote="", comment.char="",
                               fill=TRUE, header=TRUE)

AddressInfo <- read.table(file=paste0(data_dir, "/UM00100M_FINAL.txt"),
                          sep=",", quote="", comment.char="",
                          fill=TRUE, header=TRUE)


AccountInfo <- read.table(file=paste0(data_dir, "/UM00200M_Redacted FINAL.txt"),
                          sep=",", quote="", comment.char="",
                          fill=TRUE, header=TRUE)

#FinancialInfo <- read.table(file=paste0(data_dir, "/AR00200t_FINAL.txt"),
#                            sep=",", quote="", comment.char="",
#                            fill=TRUE, header=TRUE)

#save(data_all, DebtAccumulation_5p, AccountInfo, AddressInfo, 
#file=paste0(wd, "AnalysisInfo.RData"))

BillInfo <- read.table(file=paste0(data_dir, "/UM00260T_redacted_FINAL.txt"),
                       sep=",", quote="", comment.char="",
                       fill=TRUE, header=TRUE)

FinancialAssist <- read.table(file=paste0(data_dir, "/UM00232T_FINAL.txt"),
                              sep=",", quote="", comment.char="",
                              fill=TRUE, header=TRUE)

ShutOffInfo <- read.table(file=paste0(data_dir, "/RS00200M_CUTOF_redacted_FINAL.txt"),
                          sep=",", quote="", comment.char="",
                          fill=TRUE, header=TRUE)

save(AccountInfo, AddressInfo, GeoCodedAddressInfo_subset, BillInfo, 
     LocationRelation, ShutOffInfo, FinancialAssist, ShutOffInfo,
     file=paste0(working_data_dir, "/AnalysisInfo.RData"))

BillInfo <- BillInfo %>% 
  #filter(BILL_RUN_DT!="") %>%
  #select(-BILL_RUN_TM) %>%
  arrange(ACCOUNT_NO, BILL_RUN_DT) 

FilteredBillInfo <- BillInfo %>% 
  filter(CANCELED_BILL_YN!="Y",
         PERIOD_FROM_DT!="", 
         PERIOD_TO_DT!="",
         #PREV_BILL_AMT!=0,
         ERROR_YN == FALSE,
         AUDIT_OR_LIVE=="L",
         BILL_TP!="REVRS",
         CORRECTED_BILL_YN==FALSE)

DebtAccumulationBills <- FilteredBillInfo %>%
  #Identify the correct criteria to consider it a "Non Paid" bill
  filter(PREV_BILL_AMT+TOTAL_PAYMENTS>0) %>% 
  select(
    ACCOUNT_NO, BILL_RUN_DT, BILL_DT, BILL_TP, PERSON_NO, PERIOD_FROM_DT, PERIOD_TO_DT, 
    BILLING_DAYS, PREV_BILL_AMT, DUE_DT, DISCOUNT_AMT_DUE, DISCOUNT_DUE_DT, 
    DISCOUNT_PCT, TOTAL_PAYMENTS, BILL_GENERATED_CHGS, AR_NET_AFTER_BILL) %>% 
  arrange(ACCOUNT_NO)

DebtAccumulationAccounts <- FilteredBillInfo %>%
  filter(ACCOUNT_NO %in% DebtAccumulationBills$ACCOUNT_NO) %>% 
  group_by(ACCOUNT_NO) %>% 
  summarise(billsAccumulated=n(),
            AccumDebt=sum(PREV_BILL_AMT)+sum(TOTAL_PAYMENTS),
            AR_NET_AFTER_BILL=max(AR_NET_AFTER_BILL),
            check=max(PREV_BILL_AMT)+max(BILL_GENERATED_CHGS),
            DateCheck=max(PERIOD_TO_DT)) 

DateChecks <- data.table(dc=unique(DebtAccumulationAccounts$DateCheck)) %>% 
  mutate(dcf=mdy(dc))

DebtAccumulationAccounts <- DebtAccumulationAccounts %>% 
  left_join(DateChecks, by=c("DateCheck"="dc"))

LocationRelation$ACCT_TO_FRC_CONNECT <- as.double(LocationRelation$ACCT_TO_FRC_CONNECT)

DebtAccumulationAccounts <- DebtAccumulationAccounts %>% 
  left_join(LocationRelation %>%
              select(ACCT_TO_FRC_CONNECT, LOCATION_NO),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT"))

DebtAccumulationAccounts <- DebtAccumulationAccounts %>% 
  left_join(unique(GeoCodedAddressInfo_subset))

DebtAccumulationAccounts_withValidLocNo <- 
  data.table(DebtAccumulationAccounts)[!is.na(LOCATION_NO)]

DebtAccumulationAccounts_withValidLocNo <- 
  DebtAccumulationAccounts_withValidLocNo %>% 
  mutate(hadShutOff=(ACCOUNT_NO %in% ShutOffInfo$ACCOUNT_NO))

NoDebtPersonBills <- 
  data.table(FilteredBillInfo)[!(ACCOUNT_NO %in% unique(DebtAccumulationAccounts$ACCOUNT_NO))]

NoDebtPersonAccounts <- NoDebtPersonBills %>% 
  group_by(ACCOUNT_NO) %>% 
  summarise(billsAccumulated=0,
            AccumDebt=sum(PREV_BILL_AMT)+sum(TOTAL_PAYMENTS),
            AR_NET_AFTER_BILL=max(AR_NET_AFTER_BILL),
            check=max(PREV_BILL_AMT)+max(BILL_GENERATED_CHGS),
            DateCheck=max(PERIOD_TO_DT)) 

datechecks2 <- data.table(DateCheck=unique(NoDebtPersonAccounts$DateCheck)) %>% 
  mutate(dcf=mdy(DateCheck))

NoDebtAccounts <- left_join(NoDebtPersonAccounts, datechecks2)

NoDebtAccounts <- NoDebtAccounts %>% 
  left_join(LocationRelation %>%
              select(ACCT_TO_FRC_CONNECT, LOCATION_NO),
            by=c("ACCOUNT_NO"="ACCT_TO_FRC_CONNECT"))

NoDebtAccounts <- NoDebtAccounts %>% 
  left_join(unique(GeoCodedAddressInfo_subset))

NoDebtAccounts_withValidLocNo <- 
  data.table(NoDebtAccounts)[!is.na(LOCATION_NO)]

NoDebtAccounts_withValidLocNo <- 
  NoDebtAccounts_withValidLocNo %>% 
  mutate(hadShutOff=(ACCOUNT_NO %in% ShutOffInfo$ACCOUNT_NO))

DebtDataSet <- 
  rbind(DebtAccumulationAccounts_withValidLocNo, 
        NoDebtAccounts_withValidLocNo) %>% 
  filter(match_indicator=="Match") %>% 
  mutate(PaymentPlan=ACCOUNT_NO %in% FinancialAssist$ACCOUNT_NO)

save(DebtDataSet, file=paste0(working_data_dir, "/DebtDataSet.RData"))

#DebtDataSet <- left_join(DebtDataSet, AddressInfo %>% 
#                            select(LOCATION_NO, POSTAL_CODE),
#by=c("LocationNo"="LOCATION_NO"))

#DebtDataSet <- DebtDataSet %>% 
#  left_join(PortlandDemographicsWide,
#            by=c("ZIP"="GEOID"))

#DebtDataSet <- DebtDataSet %>% 
#  select(-c(highincome, PovertyPercentEstimate, PovertyAboveAverage, AboveAvg))

save(accessKey,
     AccountInfo, acsByZip, AddressInfo, baseurl, BillInfo, DebtDataSet,
     DemographicStats, EvaluateVariable, FinancialAssist, joinFrame, joinFrame2,
     LocationRelation, newSession, PortlandACS, PortlandDemographics,
     PortlandDemographicsWide_t, PortlandSummary, PortlandZipSummary, ShutOffInfo,
     SplitTempColumn, VariableMap, variableURL, WaterBillData, FilteredBillInfo, 
     BillData, LocationLookup,
     file="FinalWorkingSet.RData")
