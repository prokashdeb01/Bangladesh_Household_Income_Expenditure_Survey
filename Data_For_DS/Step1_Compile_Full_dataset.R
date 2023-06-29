
# Library


library(dplyr)
library(haven)
library(tidyverse)
library(tidyr)
library(tidyr)



# This is for 2016 Dataset

## Importing Data for Price and Quantity


con <- read_dta("C:/Users/proka/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/HH_SEC_9A2.dta")
con1 <- rename(con, c(qnt=s9a2q02, unit=s9a2q03, tk=s9a2q04, source=s9a2q05))
consumption <- con1[, -9]
rm(con, con1)


# Making wide dataframe


Con_Quant1 <- consumption %>% group_by(id, hhold, item) %>% 
  summarise(qnt = sum(qnt)) %>%  ungroup() %>%  spread(item, qnt)

Con_Quant <- Con_Quant1[-1, ]
Con_Quant[is.na(Con_Quant)] <- 0

rm(Con_Quant1)




Con_exp1 <- consumption %>% group_by(id, hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)
Con_exp <- Con_exp1[-1, ]
Con_exp[is.na(Con_exp)] <- 0

Con_exp <- Con_exp %>% rowwise() %>% 
            mutate(total_food_expenditure = sum(c_across(-c(id, hhold))))

Con_exp <- Con_exp %>% rowwise() %>% 
            mutate(total_fish_expenditure = sum(c_across(23:39)))
rm(Con_exp1)




con1 <- transform(consumption, qnt_kg = `qnt`/1000 )
con2 <- transform(con1, price_kg = `tk` / `qnt_kg`)




Con_price1 <- con2 %>% group_by(id, hhold, item) %>% 
  summarise(price = mean(price_kg)) %>%  ungroup() %>%  spread(item, price)

Con_price <- Con_price1[-1, ]
Con_price[is.na(Con_price)] <- 0

rm(con1, con2, Con_price1)



## Creating main consumption dataframe


Con_Quant <- Con_Quant %>% mutate(across(c(3:135), .fns = ~./1000))
Con_Quant <- Con_Quant %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant <- rename(Con_Quant, c(id=id_qnt, hhold=hhold_qnt))

Con_Quant <- Con_Quant %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(23:39)))
Con_Quant <- Con_Quant[,-1]



Con_exp <- Con_exp[, c(2,136,137)]
con_main <- Con_Quant %>% right_join(Con_exp, by="hhold")



Con_price <- Con_price[, -1]
Con_price <- Con_price %>%  rename_with(~ paste0(.x, "_price"))
Con_price <- rename(Con_price, c(hhold=hhold_price))

con_main <- con_main %>% right_join(Con_price, by="hhold")


## Exporting HIES 2016 Data


write.csv(con_main, file = "HIES_2016_full.csv", row.names = F)

rm(list = ls())







# This is for 2010 Dataset

## Importing Data for Price and Quantity


consumption <- read_dta("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES-Data-2010-20210719T073920Z-001/HIES-Data-2010/HIES-2010-STATA/section 9_HIES 2010/section 9A.dta")



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


Con_Quant <- con_quant1
Con_Quant <- Con_Quant %>% mutate(across(c(2:127), .fns = ~./1000))
Con_Quant <- Con_Quant %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant <- rename(Con_Quant, c(hhid=hhid_qnt))

Con_Quant <- Con_Quant %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(21:35)))




con_exp1 <- con_exp1[, c(1,128,129)]
con_main <- Con_Quant %>% right_join(con_exp1, by="hhid")



con_price1 <- con_price1 %>%  rename_with(~ paste0(.x, "_price"))
con_price1 <- rename(con_price1, c(hhid=hhid_price))



con_main <- con_main %>% right_join(con_price1, by="hhid")

rm(con_exp1, con_price1, con_quant1, Con_Quant)



## Exporting HIES 2010 Data


write.csv(con_main, file = "HIES_2010_full.csv", row.names = F)

rm(list = ls())








# This is for 2005 Dataset

## Importing Data for Price and Quantity


consumption <- read_sav("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s9a2.sav")

consumption <- rename(consumption, c(hhold=HHOLD, item=CODE, qnt=QUAN, tk=VALU))



# Making wide dataframe


Con_Quant1 <- consumption %>% group_by(hhold, item) %>% 
  summarise(qnt = sum(qnt)) %>%  ungroup() %>%  spread(item, qnt)

Con_Quant1[is.na(Con_Quant1)] <- 0

Con_Quant1 <- select(Con_Quant1, -c('010', '060', '080', '100', '120', '150', '180'))





Con_exp1 <- consumption %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

Con_exp1[is.na(Con_exp1)] <- 0

Con_exp1 <- select(Con_exp1, -c('010', '060', '080', '100', '120', '150', '180'))

Con_exp1 <- Con_exp1 %>% rowwise() %>% 
            mutate(total_food_expenditure = sum(c_across(-c(hhold))))

Con_exp1 <- Con_exp1 %>% rowwise() %>% 
            mutate(total_fish_expenditure = sum(c_across(22:36)))




con1 <- transform(consumption, qnt_kg = `qnt`/1000 )
con2 <- transform(con1, price_kg = `tk` / `qnt_kg`)




Con_price1 <- con2 %>% group_by(hhold, item) %>% 
  summarise(price = mean(price_kg)) %>%  ungroup() %>%  spread(item, price)

Con_price1[is.na(Con_price1)] <- 0

Con_price1 <- select(Con_price1, -c('010', '060', '080', '100', '120', '150', '180'))

rm(con1, con2)



## Creating main consumption dataframe


Con_Quant <- Con_Quant1
Con_Quant <- Con_Quant %>% mutate(across(c(2:129), .fns = ~./1000))
Con_Quant <- Con_Quant %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant <- rename(Con_Quant, c(hhold=hhold_qnt))

Con_Quant <- Con_Quant %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(22:36)))




Con_exp1 <- Con_exp1[, c(1,130,131)]
con_main <- Con_Quant %>% right_join(Con_exp1, by="hhold")




Con_price1 <- Con_price1 %>%  rename_with(~ paste0(.x, "_price"))
Con_price1 <- rename(Con_price1, c(hhold=hhold_price))




con_main <- con_main %>% right_join(Con_price1, by="hhold")

rm(Con_exp1, Con_price1, Con_Quant1, Con_Quant)



## Exporting HIES 2005 Data


write.csv(con_main, file = "HIES_2005_full.csv", row.names = F)

rm(list = ls())






# This is for 2000 Dataset

## Importing Data for Price and Quantity


consumption <- read_sav("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Raw_Data/HIES 2000-20210719T073610Z-001/HIES 2000/HIES2000/s9a2.sav")

consumption <- rename(consumption, c(hhold=HHOLD, item=CODE, qnt=QUAN, tk=VALU))



# Making wide dataframe


Con_Quant1 <- consumption %>% group_by(hhold, item) %>% 
  summarise(qnt = sum(qnt)) %>%  ungroup() %>%  spread(item, qnt)

Con_Quant1[is.na(Con_Quant1)] <- 0

Con_Quant2 <- select(Con_Quant1, -c('0', '10', '40', '60', '70', '80', '100', '110', 
                                    '120', '130', '150', '160', '170', '180', '190'))

rm(Con_Quant1)




Con_exp1 <- consumption %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

Con_exp1[is.na(Con_exp1)] <- 0

Con_exp1 <- select(Con_exp1, -c('0', '10', '40', '60', '70', '80', '100', '110', 
                                    '120', '130', '150', '160', '170', '180', '190'))

Con_exp1 <- Con_exp1 %>% rowwise() %>% 
            mutate(total_food_expenditure = sum(c_across(-c(hhold))))

Con_exp1 <- Con_exp1 %>% rowwise() %>% 
            mutate(total_fish_expenditure = sum(c_across(22:36)))




con1 <- transform(consumption, qnt_kg = `qnt`/1000)
con2 <- transform(con1, price_kg = `tk` / `qnt_kg`)




Con_price1 <- con2 %>% group_by(hhold, item) %>% 
  summarise(price = mean(price_kg)) %>%  ungroup() %>%  spread(item, price)

Con_price1 <- select(Con_price1, -c('0', '10', '40', '60', '70', '80', '100', '110', 
                                    '120', '130', '150', '160', '170', '180', '190'))

Con_price1[is.na(Con_price1)] <- 0

rm(con1, con2)



## Creating main consumption dataframe


Con_Quant <- Con_Quant2
Con_Quant <- Con_Quant %>% mutate(across(c(2:121), .fns = ~./1000))
Con_Quant <- Con_Quant %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant <- rename(Con_Quant, c(hhold=hhold_qnt))

Con_Quant <- Con_Quant %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(22:36)))




Con_exp1 <- Con_exp1[, c(1,122,123)]
con_main <- Con_Quant %>% right_join(Con_exp1, by="hhold")




Con_price1 <- Con_price1 %>%  rename_with(~ paste0(.x, "_price"))
Con_price1 <- rename(Con_price1, c(hhold=hhold_price))




con_main <- con_main %>% right_join(Con_price1, by="hhold")

rm(Con_exp1, Con_price1, Con_Quant2, Con_Quant)



## Exporting HIES 2000 Data


write.csv(con_main, file = "HIES_2000_full.csv", row.names = F)

rm(list = ls())

























