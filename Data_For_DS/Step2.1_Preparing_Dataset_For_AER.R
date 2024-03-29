
## library


library(dplyr)
library(haven)



## Importing Data HIES 2016


HIES2010_AER <- read_dta("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/rt002.dta")



## Organizing dataset HIES 2016


AER <- HIES2010_AER
AER$psu <- as.numeric(AER$psu)
AER$hhold <- as.numeric(AER$hhold)

AER$hhid <- (AER$psu*1000) + AER$hhold

AER <- AER[, c(150, 6, 8)]

AER <- rename(AER, hhold=hhid)

write.csv(AER, file = "HIES2010_AER.csv", row.names = F)



rm(list = ls())





## Importing Data HIES 2005


HIES2005_AER <- read_sav("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s1a.sav")



## Organizing dataset HIES 2005


AER <- HIES2005_AER
AER <- AER[, c(4, 7, 9)]

AER <- rename(AER, hhold=HHOLD)

write.csv(AER, file = "HIES2005_AER.csv", row.names = F)

rm(list = ls())




## Importing Data HIES 2000


HIES2000_AER <- read_sav("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES 2000-20210719T073610Z-001/HIES 2000/HIES2000/S1a.sav")



## Organizing dataset HIES 2000


AER <- HIES2000_AER
AER <- AER[, c(3, 6, 8)]

AER$s1aq01 <- AER$Q02_1A
AER$s1aq03 <- AER$Q04_1A

AER <- AER[,c(1,4,5)]

AER <- rename(AER, hhold=HHOLD)
AER$hhold <- as.numeric(AER$hhold)

sum(is.na(AER$s1aq01))

AER <- na.omit(AER)






write.csv(AER, file = "HIES2000_AER.csv", row.names = F)

rm(list = ls())





























