
# Library


library(dplyr)
library(haven)
library(tidyverse)
library(tidyr)
library(tidyr)



## Importing Data for Price and Quantity


consumption <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/section 9_HIES 2010/section 9A.dta")



## consumption quantity


con_quant <- consumption[,c('hhid', 'item', 's09a1d01', 's09a1d02', 's09a1d03', 's09a1d04', 's09a1d05', 's09a1d06', 's09a1d07', 's09a1d08', 's09a1d09', 's09a1d10', 's09a1d11', 's09a1d12', 's09a1d13', 's09a1d14')]

con_quant <- con_quant %>% rowwise() %>% 
            mutate(qnt = sum(c_across(3:16)))

con_quant1 <- con_quant %>% group_by(hhid, item) %>% 
  summarise(qnt = sum(qnt)) %>%  ungroup() %>%  
  spread(item, qnt)

con_quant1[is.na(con_quant1)] <- 0

con_quant1 <- select(con_quant1, -c('10', '30', '40', '60', '70', '80', '100', '110', '120', '130', '150', '160', '170', '180', '200'))


## consumption expenditure


con_exp <- consumption[,c('hhid', 'item', 's09a1d_2', 's09a1d_5', 's09a1d_8', 's09a1_11', 's09a1_14', 's09a1_17', 's09a1_20', 's09a1_23', 's09a1_26', 's09a1_29', 's09a1_32', 's09a1_35', 's09a1_38', 's09a1_41')]

con_exp <- con_exp %>% rowwise() %>% 
            mutate(exp = sum(c_across(3:16)))

con_exp1 <- con_exp %>% group_by(hhid, item) %>% 
  summarise(exp = sum(exp)) %>%  ungroup() %>%  
  spread(item, exp)

con_exp1[is.na(con_exp1)] <- 0

con_exp1 <- select(con_exp1, -c('10', '30', '40', '60', '70', '80', '100', '110', '120', '130', '150', '160', '170', '180', '200'))

con_exp1 <- con_exp1 %>% rowwise() %>% 
            mutate(total_food_expenditure = sum(c_across(-c(hhid))))

con_exp1 <- con_exp1 %>% rowwise() %>% 
            mutate(total_fish_expenditure = sum(c_across(21:35)))



## consumption per unit price


con_exp2 <- con_exp[,c(1,2,17)]
con_quant2 <- con_quant[,c(1,2,17)]

con <- con_quant2 %>% merge(con_exp2, by=c("hhid", "item"))

con$qnt_kg <- con$qnt/1000
con$price_kg <- (con$exp/con$qnt_kg)

con_price1 <- con %>% group_by(hhid, item) %>% 
  summarise(price = mean(price_kg)) %>%  ungroup() %>%  spread(item, price)

con_price1[is.na(con_price1)] <- 0

con_price1 <- select(con_price1, -c('10', '30', '40', '60', '70', '80', '100', '110', '120', '130', '150', '160', '170', '180', '200'))

rm(con_exp, con_exp2, con, con_quant, con_quant2)



## Creating main consumption dataframe


Con_Quant_Fish <- con_quant1[, c(1,21:35)]
Con_Quant_Fish <- Con_Quant_Fish %>% mutate(across(c(2:16), .fns = ~./1000))
Con_Quant_Fish <- Con_Quant_Fish %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant_Fish <- rename(Con_Quant_Fish, c(hhid=hhid_qnt))

Con_Quant_Fish <- Con_Quant_Fish %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(2:16)))




con_exp1 <- con_exp1[, c(1,128,129)]
con_main <- Con_Quant_Fish %>% right_join(con_exp1, by="hhid")



con_price1 <- con_price1[, c(1, 21:35)]
con_price1 <- con_price1 %>%  rename_with(~ paste0(.x, "_price"))
con_price1 <- rename(con_price1, c(hhid=hhid_price))



con_main <- con_main %>% right_join(con_price1, by="hhid")

rm(con_exp1, con_price1, con_quant1, Con_Quant_Fish)



## write.csv(con_main, file = "con_main_2010.csv", row.names = F)



## Adding weekly food expenditure 


con_week <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/section 9_HIES 2010/section 9B.dta")

con_week_temp <- con_week[,c('hhid', 'item', 's09b1w_2', 's09b1w_5')]

con_week_temp <- con_week_temp %>% rowwise() %>% 
            mutate(total_2week_expenditure = sum(c_across(3:4)))

con_week_temp1 <- con_week_temp %>% group_by(hhid, item) %>% 
  summarise(total_2week_expenditure = sum(total_2week_expenditure)) %>% 
  ungroup() %>%  
  spread(item, total_2week_expenditure)

con_week_temp1[is.na(con_week_temp1)] <- 0

con_week_temp1 <- select(con_week_temp1, -c('210', '230'))

con_week_temp1 <- con_week_temp1 %>% rowwise() %>% 
            mutate(total_2week_expenditure = sum(c_across(-c(hhid))))

con_week_temp1 <- con_week_temp1[,c(1,21)]

con_main <- con_main %>% merge(con_week_temp1, by="hhid")

rm(con_week, con_week_temp, con_week_temp1)



## Adding Monthly non-food expenditure


con_month <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/section 9_HIES 2010/section 9C.dta")

con_month_temp <- con_month[,c('hhid', 'item', 's09c1__2')]

con_month_temp <- rename(con_month_temp, total_monthly_nonfood_expenditure=s09c1__2)

con_month_temp1 <- con_month_temp %>% group_by(hhid, item) %>% 
  summarise(total_monthly_nonfood_expenditure =
              sum(total_monthly_nonfood_expenditure)) %>% 
  ungroup() %>%  
  spread(item, total_monthly_nonfood_expenditure)

con_month_temp1[is.na(con_month_temp1)] <- 0

con_month_temp1 <- select(con_month_temp1, -c('240', '250', '260', '270'))

con_month_temp1 <- con_month_temp1 %>% rowwise() %>% 
            mutate(total_monthly_nonfood_expenditure =
                     sum(c_across(-c(hhid))))

con_month_temp1 <- con_month_temp1[,c(1,47)]

con_main <- con_main %>% merge(con_month_temp1, by="hhid")

rm(con_month, con_month_temp, con_month_temp1)



## Adding yearly non-food expenditure


con_year <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/section 9_HIES 2010/section 9D1.dta")

con_year_temp <- con_year[,c('hhid', 'item', 's09d1__1')]

con_year_temp1 <- con_year_temp %>% group_by(hhid, item) %>% 
  summarise(year_total =sum(s09d1__1)) %>% 
  ungroup() %>%  
  spread(item, year_total)

con_year_temp1[is.na(con_year_temp1)] <- 0

con_year_temp1 <- select(con_year_temp1, -c('300', '330', '340'))

con_year_temp1 <- con_year_temp1 %>% rowwise() %>% 
            mutate(total_yearly_nonfood_expenditure1 =
                     sum(c_across(-c(hhid))))

con_year_temp1 <- con_year_temp1[,c(1,46)]

rm(con_year, con_year_temp)



con_year1 <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/section 9_HIES 2010/section 9D2-5.dta")

con_year_temp2 <- con_year1[,c('hhid', 'item', 's09d2_q0')]

con_year_temp2 <- con_year_temp2 %>% group_by(hhid, item) %>% 
  summarise(year_total =sum(s09d2_q0)) %>% 
  ungroup() %>%  
  spread(item, year_total)

con_year_temp2[is.na(con_year_temp2)] <- 0

con_year_temp2 <- select(con_year_temp2, -c('360', '380', '390', '410', '430', '450', '440', '470', '490', '520', '500', '510', '530', '550'))

con_year_temp2 <- con_year_temp2 %>% rowwise() %>% 
            mutate(total_yearly_nonfood_expenditure2 =
                     sum(c_across(-c(hhid))))

con_year_temp2 <- con_year_temp2[,c(1,128)]

rm(con_year1)



con_year_temp1 <- con_year_temp1 %>% merge(con_year_temp2, by="hhid")

con_year_temp1$total_yearly_nonfood_expenditure <- con_year_temp1$total_yearly_nonfood_expenditure1 + con_year_temp1$total_yearly_nonfood_expenditure2

con_year_temp1 <- con_year_temp1[,c(1,4)]

con_main <- con_main %>% merge(con_year_temp1, by="hhid")

rm(con_year_temp2, con_year_temp1)



## Adding income from fisheries


fish_production <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/rt009.dta")

fish_production$psu <- as.numeric(fish_production$psu)
fish_production$hhold <- as.numeric(fish_production$hhold)

fish_production$hhid <- (fish_production$psu*1000)+fish_production$hhold

fish_production1 <- fish_production[,c('hhid', 'fish_act', 's07c_q_2')]

fish_production1 <- fish_production1 %>% group_by(hhid, fish_act) %>% 
  summarise(year_total =sum(s07c_q_2)) %>% 
  ungroup() %>%  
  spread(fish_act, year_total)

fish_production1[is.na(fish_production1)] <- 0

fish_production1 <- select(fish_production1, -c('230'))

fish_production1 <- fish_production1 %>% rowwise() %>% 
            mutate(total_yearly_sell_fishproduction =
                     sum(c_across(-c(hhid))))

fish_production1 <- fish_production1[,c(1,10)]

con_main <- con_main %>% left_join(fish_production1, by="hhid")

con_main[is.na(con_main)] <- 0


# Exporting data


write.csv(con_main, file = "fish_HIES2010.csv", row.names = F)













