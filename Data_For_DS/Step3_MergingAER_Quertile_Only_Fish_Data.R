

### We are not using Dataset created in Step 1. We only require dataset from previous version named "compiled data" that contains only fish consumption data. 


# Library


library(dplyr)
library(haven)
library(tidyverse)
library(tidyr)
library(readxl)



## Upload HIES datasets


HIES_2016 <- read_excel("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Compiled_Data/Compiled_Fish_Data/fish_HIES2016.xlsx")

HIES_2010 <- read_excel("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Compiled_Data/Compiled_Fish_Data/fish_HIES2010.xlsx")

HIES_2005 <- read_excel("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Compiled_Data/Compiled_Fish_Data/fish_HIES2005.xlsx")

HIES_2000 <- read_excel("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Compiled_Data/Compiled_Fish_Data/fish_HIES2000.xlsx")


## Finding any infinite or NA values

# a <- apply(HIES_2016, 2, function(x) any(is.na(x)|is.infinite(x)))




## Upload AER (Adult Equivalent Ratio) datasets


AER_2016 <- read_excel("AER_Data/AER_2016.xlsx")
AER_2010 <- read_excel("AER_Data/AER_2010.xlsx")
AER_2005 <- read_excel("AER_Data/AER_2005.xlsx")
AER_2000 <- read_excel("AER_Data/AER_2000.xlsx")



## Marging AER with HIES


HIES_2016 <- HIES_2016 %>% merge(AER_2016, by="hhold")

HIES_2010 <- rename(HIES_2010, hhold=hhid)
HIES_2010 <- HIES_2010 %>% merge(AER_2010, by="hhold")

HIES_2005$hhold <- as.numeric(HIES_2005$hhold)
HIES_2005 <- HIES_2005 %>% merge(AER_2005, by="hhold")

HIES_2000$hhold <- as.numeric(HIES_2000$hhold)
HIES_2000 <- HIES_2000 %>% merge(AER_2000, by="hhold")




rm(AER_2000, AER_2005, AER_2010, AER_2016)



## Adding Rural and Arban areas


area_2016 <- read_dta("C:/Users/proka/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/hh_sec_a.dta")
area_2016 <- area_2016[,c(3,14)]
area_2016$area <- ifelse(area_2016$ruc==1, "rural", "urban")
area_2016 <- area_2016[,c(1,3)]

area_2010 <- read_dta("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/rt001.dta")
area_2010$psu <- as.numeric(area_2010$psu)
area_2010$hhold <- as.numeric(area_2010$hhold)
area_2010$hhid <- (area_2010$psu*1000)+area_2010$hhold
area_2010 <- area_2010[,c(152, 11)]
area_2010 <- rename(area_2010, hhold=hhid)
area_2010$area <- ifelse(area_2010$spc=="Rural", "rural", "urban")
area_2010 <- area_2010[,c(1,3)]

area_2005 <- read_sav("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s0.sav")
area_2005$hhold <- as.numeric(area_2005$HHOLD)
area_2005 <- area_2005[,c(15, 9)]
area_2005$area <- ifelse(area_2005$RMO==1, "rural", "urban")
area_2005 <- area_2005[,c(1,3)]

area_2000 <- read_sav("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES 2000-20210719T073610Z-001/HIES 2000/HIES2000/s0.sav")
area_2000$hhold <- as.numeric(area_2000$HHOLD)
area_2000 <- area_2000[,c(14, 9)]
area_2000$area <- ifelse(area_2000$RMO==1, "rural", "urban")
area_2000 <- area_2000[,c(1,3)]



## Merging the rural urban locaion to full dataframe


HIES_2016 <- HIES_2016 %>% merge(area_2016, by="hhold")
HIES_2010 <- HIES_2010 %>% merge(area_2010, by="hhold")
HIES_2005 <- HIES_2005 %>% merge(area_2005, by="hhold")
HIES_2000 <- HIES_2000 %>% merge(area_2000, by="hhold")



rm(area_2000, area_2005, area_2010, area_2016)





## Cleaning the data: quantity


HIES_2016 <- subset(HIES_2016, c(HIES_2016$`41_qnt`<15 & HIES_2016$`42_qnt`<20 & HIES_2016$`43_qnt`<20 &
                                   HIES_2016$`45_qnt`<5 & HIES_2016$`47_qnt`<10 & HIES_2016$`48_qnt`<10 &
                                   HIES_2016$`49_qnt`<20 & HIES_2016$`51_qnt`<10 & HIES_2016$`52_qnt`<10 & 
                                   HIES_2016$`55_qnt`<5 & HIES_2016$`56_qnt`<10 & 
                                   HIES_2016$total_food_expenditure<20000 & 
                                   HIES_2016$total_fish_expenditure<5000 & 
                                   HIES_2016$total_2week_expenditure<3000 & 
                                   HIES_2016$total_monthly_nonfood_expenditure<100000))

HIES_2010 <- subset(HIES_2010, c(HIES_2010$`46_qnt`<10 & HIES_2010$`49_qnt`<8 & 
                                   HIES_2010$total_food_expenditure<15000 & 
                                   HIES_2010$total_fish_expenditure<3000 & 
                                   HIES_2010$total_monthly_nonfood_expenditure<40000))

HIES_2005 <- subset(HIES_2005, c(HIES_2005$`046_qnt`<8 & HIES_2005$`049_qnt`<6 & HIES_2005$`054_qnt`<10 & 
                                   HIES_2005$total_food_expenditure<8000 & 
                                   HIES_2005$total_monthly_nonfood_expenditure<40000))

HIES_2000 <- subset(HIES_2000, c(HIES_2000$`48_qnt`<8 & HIES_2000$`49_qnt`<6 & 
                                   HIES_2000$total_food_expenditure<8000 & 
                                   HIES_2000$total_monthly_nonfood_expenditure<20000))


plot.ts(HIES_2016$`41_qnt`)





## Converting Yearly data: consumption quantity, total food expenditure


a <- 365/14

temp_2016 <- HIES_2016[,-c(22:38)]
temp_2010 <- HIES_2010[,-c(20:34)]
temp_2005 <- HIES_2005[,-c(20:34)]
temp_2000 <- HIES_2000[,-c(20:34)]

# making yearly data

temp_2016 <- temp_2016 %>% mutate(across(.cols = c('41_qnt':'total_2week_expenditure'), .fns=~.x*a))
temp_2010 <- temp_2010 %>% mutate(across(.cols = c('41_qnt':'total_2week_expenditure'), .fns=~.x*a))
temp_2005 <- temp_2005 %>% mutate(across(.cols = c('041_qnt':'total_2week_expenditure'), .fns=~.x*a))
temp_2000 <- temp_2000 %>% mutate(across(.cols = c('41_qnt':'total_2week_expenditure'), .fns=~.x*a))

temp_2016$total_monthly_nonfood_expenditure <- temp_2016$total_monthly_nonfood_expenditure*12
temp_2010$total_monthly_nonfood_expenditure <- temp_2010$total_monthly_nonfood_expenditure*12
temp_2005$total_monthly_nonfood_expenditure <- temp_2005$total_monthly_nonfood_expenditure*12
temp_2000$total_monthly_nonfood_expenditure <- temp_2000$total_monthly_nonfood_expenditure*12

# total yearly food expenditure

temp_2016$total_food_expenditure_peryear <- temp_2016$total_food_expenditure + temp_2016$total_2week_expenditure

temp_2010$total_food_expenditure_peryear <- temp_2010$total_food_expenditure + temp_2010$total_2week_expenditure

temp_2005$total_food_expenditure_peryear <- temp_2005$total_food_expenditure + temp_2005$total_2week_expenditure

temp_2000$total_food_expenditure_peryear <- temp_2000$total_food_expenditure + temp_2000$total_2week_expenditure

# total yearly nonfood expenditure

temp_2016$total_nonfood_expenditure_peryear <- temp_2016$total_monthly_nonfood_expenditure + temp_2016$total_yearly_nonfood_expenditure

temp_2010$total_nonfood_expenditure_peryear <- temp_2010$total_monthly_nonfood_expenditure + temp_2010$total_yearly_nonfood_expenditure

temp_2005$total_nonfood_expenditure_peryear <- temp_2005$total_monthly_nonfood_expenditure + temp_2005$total_yearly_nonfood_expenditure

temp_2000$total_nonfood_expenditure_peryear <- temp_2000$total_monthly_nonfood_expenditure + temp_2000$total_yearly_nonfood_expenditure

## removing unnecessary columns

temp_2016 <- temp_2016[, -c(20, 22, 23, 24)]
temp_2010 <- temp_2010[, -c(18, 20:22)]
temp_2005 <- temp_2005[, -c(18, 20:22)]
temp_2000 <- temp_2000[, -c(18, 20:22)]

# adding total expenditure column

temp_2016$total_expenditure_peryear <- temp_2016$total_food_expenditure_peryear+temp_2016$total_nonfood_expenditure_peryear

temp_2010$total_expenditure_peryear <- temp_2010$total_food_expenditure_peryear+temp_2010$total_nonfood_expenditure_peryear

temp_2005$total_expenditure_peryear <- temp_2005$total_food_expenditure_peryear+temp_2005$total_nonfood_expenditure_peryear

temp_2000$total_expenditure_peryear <- temp_2000$total_food_expenditure_peryear+temp_2000$total_nonfood_expenditure_peryear

# making data per person by dividing all columns by adult equivalent ratio

temp_2016 <- temp_2016 %>% mutate(across(.cols = c('41_qnt':'58_qnt'), .fns=~.x/temp_2016$nad))
temp_2010 <- temp_2010 %>% mutate(across(.cols = c('41_qnt':'56_qnt'), .fns=~.x/temp_2010$nad))
temp_2005 <- temp_2005 %>% mutate(across(.cols = c('041_qnt':'056_qnt'), .fns=~.x/temp_2005$nad))
temp_2000 <- temp_2000 %>% mutate(across(.cols = c('41_qnt':'56_qnt'), .fns=~.x/temp_2000$nad))

# renaming the columns

temp_2016 <- rename(temp_2016, c(hilsa = '41_qnt', 'rohu/katla/mrigel/kali_baush' = '42_qnt', pangasius = '43_qnt', 'boal/air' = '44_qnt', 'magur/shing' = '45_qnt', koi = '46_qnt', 'silver_carp/grass_carp/miror_carp' = '47_qnt', 'shoal/gajar/taki' = '48_qnt', 'puti/big_puti/tilapia' = '49_qnt', 'mala_kachi/chala_chapila/khalsha' = '51_qnt', 'small_fish(with_tangra)' = '52_qnt', shrimp = '53_qnt', dried_fish = '54_qnt', eel_fish = '55_qnt', sea_fish = '56_qnt', baila = '57_qnt', other = '58_qnt'))

temp_2010 <- rename(temp_2010, c(hilsa = '41_qnt', 'rohu/katal/mrigel/kali_baush' = '42_qnt', 'pangash/boal/air' = '43_qnt', 'magur/shinghi/khalisha' = '44_qnt', koi = '45_qnt', 'silver_carp/grass_carp/miror_carp' = '46_qnt', 'shoal/gajar/taki' = '47_qnt', 'puti/big_puti/tilapia' = '48_qnt', 'mala_kachi/chala_chapila' = '49_qnt', shrimp = '51_qnt', dried_fish = '52_qnt', 'tangra/eelfish' = '53_qnt', sea_fish = '54_qnt', 'baila/tapashi' = '55_qnt', other = '56_qnt'))

temp_2005 <- rename(temp_2005, c(hilsa = '041_qnt', 'rohu/katal/mrigel/kali_baush' = '042_qnt', 'pangash/boal/air' = '043_qnt', 'magur/shinghi/khalisha' = '044_qnt', koi = '045_qnt', 'silver_carp/grass_carp/miror_carp' = '046_qnt', 'shoal/gajar/taki' = '047_qnt', 'puti/big_puti/tilapia' = '048_qnt', 'mala_kachi/chala_chapila' = '049_qnt', shrimp = '051_qnt', dried_fish = '052_qnt', 'tangra/eelfish' = '053_qnt', sea_fish = '054_qnt', 'baila/tapashi' = '055_qnt', other = '056_qnt'))

temp_2000 <- rename(temp_2000, c(hilsa = '41_qnt', 'rohu/katal/mrigel/kali_baush' = '42_qnt', 'pangash/boal/air' = '43_qnt', 'magur/shinghi/khalisha' = '44_qnt', koi = '45_qnt', 'silver_carp/grass_carp/miror_carp' = '46_qnt', 'shoal/gajar/taki' = '47_qnt', 'puti/big_puti/tilapia' = '48_qnt', 'mala_kachi/chala_chapila' = '49_qnt', shrimp = '51_qnt', dried_fish = '52_qnt', 'tangra/eelfish' = '53_qnt', sea_fish = '54_qnt', 'baila/tapashi' = '55_qnt', other = '56_qnt'))

# Removing unnecessary variables

temp_2016 <- temp_2016[,-c(21,22)]
temp_2010 <- temp_2010[,-c(19,20)]
temp_2005 <- temp_2005[,-c(19,20)]
temp_2000 <- temp_2000[,-c(19,20)]

## Adding Price variables

price_2016 <- HIES_2016[,c(1, 22:38)]
price_2010 <- HIES_2010[,c(1, 20:34)]
price_2005 <- HIES_2005[,c(1, 20:34)]
price_2000 <- HIES_2000[,c(1, 20:34)]

price_2016 <- rename(price_2016, c(hilsa_price = '41_price', 'rohu/katla/mrigel/kali_baush_price' = '42_price', pangasius_price = '43_price', 'boal/air_price' = '44_price', 'magur/shing_price' = '45_price', koi_price = '46_price', 'silver_carp/grass_carp/miror_carp_price' = '47_price', 'shoal/gajar/taki_price' = '48_price', 'puti/big_puti/tilapia_price' = '49_price', 'mala_kachi/chala_chapila/khalsha_price' = '51_price', 'small_fish(with_tangra)_price' = '52_price', shrimp_price = '53_price', dried_fish_price = '54_price', eel_fish_price = '55_price', sea_fish_price = '56_price', baila_price = '57_price', other_price = '58_price'))

price_2010 <- rename(price_2010, c(hilsa_price = '41_price', 'rohu/katal/mrigel/kali_baush_price' = '42_price', 'pangash/boal/air_price' = '43_price', 'magur/shinghi/khalisha_price' = '44_price', koi_price = '45_price', 'silver_carp/grass_carp/miror_carp_price' = '46_price', 'shoal/gajar/taki_price' = '47_price', 'puti/big_puti/tilapia_price' = '48_price', 'mala_kachi/chala_chapila_price' = '49_price', shrimp_price = '51_price', dried_fish_price = '52_price', 'tangra/eelfish_price' = '53_price', sea_fish_price = '54_price', 'baila/tapashi_price' = '55_price', other_price = '56_price'))

price_2005 <- rename(price_2005, c(hilsa_price = '041_price', 'rohu/katal/mrigel/kali_baush_price' = '042_price', 'pangash/boal/air_price' = '043_price', 'magur/shinghi/khalisha_price' = '044_price', koi_price = '045_price', 'silver_carp/grass_carp/miror_carp_price' = '046_price', 'shoal/gajar/taki_price' = '047_price', 'puti/big_puti/tilapia_price' = '048_price', 'mala_kachi/chala_chapila_price' = '049_price', shrimp_price = '051_price', dried_fish = '052_price', 'tangra/eelfish_price' = '053_price', sea_fish_price = '054_price', 'baila/tapashi_price' = '055_price', other_price = '056_price'))

price_2000 <- rename(price_2000, c(hilsa_price = '41_price', 'rohu/katal/mrigel/kali_baush_price' = '42_price', 'pangash/boal/air_price' = '43_price', 'magur/shinghi/khalisha_price' = '44_price', koi_price = '45_price', 'silver_carp/grass_carp/miror_carp_price' = '46_price', 'shoal/gajar/taki_price' = '47_price', 'puti/big_puti/tilapia_price' = '48_price', 'mala_kachi/chala_chapila_price' = '49_price', shrimp_price = '51_price', dried_fish_price = '52_price', 'tangra/eelfish_price' = '53_price', sea_fish_price = '54_price', 'baila/tapashi_price' = '55_price', other_price = '56_price'))

## merging the price data with compiled (temp) data

temp_2016 <- temp_2016 %>% merge(price_2016, by="hhold")
temp_2010 <- temp_2010 %>% merge(price_2010, by="hhold")
temp_2005 <- temp_2005 %>% merge(price_2005, by="hhold")
temp_2000 <- temp_2000 %>% merge(price_2000, by="hhold")


## subset rural and urban

rural_2016 <- subset(temp_2016, temp_2016$area=="rural")
rural_2010 <- subset(temp_2010, temp_2010$area=="rural")
rural_2005 <- subset(temp_2005, temp_2005$area=="rural")
rural_2000 <- subset(temp_2000, temp_2000$area=="rural")

urban_2016 <- subset(temp_2016, temp_2016$area=="urban")
urban_2010 <- subset(temp_2010, temp_2010$area=="urban")
urban_2005 <- subset(temp_2005, temp_2005$area=="urban")
urban_2000 <- subset(temp_2000, temp_2000$area=="urban")

rm(HIES_2016, HIES_2010, HIES_2005, HIES_2000, temp_2000, temp_2005, temp_2010, temp_2016, a)

# Calculate quantile from total expenditure per year column

quantile(rural_2016$total_expenditure_peryear)
rural_2016 <- rural_2016 %>% mutate(quantile=case_when(total_expenditure_peryear <= 82754.107 ~ 1,
                                                         total_expenditure_peryear <= 119031.482 ~ 2,
                                                         total_expenditure_peryear <= 171992.393 ~ 3,
                                                         total_expenditure_peryear <= 4104584 ~ 4,
                                                         TRUE ~ 0))

quantile(rural_2010$total_expenditure_peryear)
rural_2010 <- rural_2010 %>% mutate(quantile=case_when(total_expenditure_peryear <= 66661.875 ~ 1,
                                                         total_expenditure_peryear <= 95769.821 ~ 2,
                                                         total_expenditure_peryear <= 137407.518 ~ 3,
                                                         total_expenditure_peryear <= 1831450  ~ 4,
                                                         TRUE ~ 0))

quantile(rural_2005$total_expenditure_peryear)
rural_2005 <- rural_2005 %>% mutate(quantile=case_when(total_expenditure_peryear <= 33817.066 ~ 1,
                                                         total_expenditure_peryear <= 49718.571 ~ 2,
                                                         total_expenditure_peryear <= 75577.330 ~ 3,
                                                         total_expenditure_peryear <= 1492484  ~ 4,
                                                         TRUE ~ 0))

quantile(rural_2000$total_expenditure_peryear)
rural_2000 <- rural_2000 %>% mutate(quantile=case_when(total_expenditure_peryear <= 22983.259 ~ 1,
                                                         total_expenditure_peryear <= 32185.857 ~ 2,
                                                         total_expenditure_peryear <= 46057.009 ~ 3,
                                                         total_expenditure_peryear <= 361525  ~ 4,
                                                         TRUE ~ 0))



quantile(urban_2000$total_expenditure_peryear)
urban_2000 <- urban_2000 %>% mutate(quantile=case_when(total_expenditure_peryear <= 31777.375 ~ 1,
                                                         total_expenditure_peryear <= 45888.821 ~ 2,
                                                         total_expenditure_peryear <= 65455.147 ~ 3,
                                                         total_expenditure_peryear <= 361534  ~ 4,
                                                         TRUE ~ 0))

quantile(urban_2005$total_expenditure_peryear)
urban_2005 <- urban_2005 %>% mutate(quantile=case_when(total_expenditure_peryear <= 41029.344 ~ 1,
                                                         total_expenditure_peryear <= 62619.614 ~ 2,
                                                         total_expenditure_peryear <= 103540.025 ~ 3,
                                                         total_expenditure_peryear <= 1180679  ~ 4,
                                                         TRUE ~ 0))

quantile(urban_2010$total_expenditure_peryear)
urban_2010 <- urban_2010 %>% mutate(quantile=case_when(total_expenditure_peryear <= 84344.571 ~ 1,
                                                         total_expenditure_peryear <= 128227.414 ~ 2,
                                                         total_expenditure_peryear <= 199410.286 ~ 3,
                                                         total_expenditure_peryear <= 1636814  ~ 4,
                                                         TRUE ~ 0))

quantile(urban_2016$total_expenditure_peryear)
urban_2016 <- urban_2016 %>% mutate(quantile=case_when(total_expenditure_peryear <= 108034.75 ~ 1,
                                                         total_expenditure_peryear <= 155125.07 ~ 2,
                                                         total_expenditure_peryear <= 229610.27 ~ 3,
                                                         total_expenditure_peryear <= 4393472  ~ 4,
                                                         TRUE ~ 0))



## Exporting the compiled dataset


write.csv(rural_2000, file = "HIES_2000_Rural.csv", row.names = F)
write.csv(rural_2005, file = "HIES_2005_Rural.csv", row.names = F)
write.csv(rural_2010, file = "HIES_2010_Rural.csv", row.names = F)
write.csv(rural_2016, file = "HIES_2016_Rural.csv", row.names = F)

write.csv(urban_2000, file = "HIES_2000_Urban.csv", row.names = F)
write.csv(urban_2005, file = "HIES_2005_Urban.csv", row.names = F)
write.csv(urban_2010, file = "HIES_2010_Urban.csv", row.names = F)
write.csv(urban_2016, file = "HIES_2016_Urban.csv", row.names = F)
































