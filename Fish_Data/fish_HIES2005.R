
# Library


library(dplyr)
library(haven)
library(tidyverse)
library(tidyr)
library(tidyr)


## Importing Data for Price and Quantity


consumption <- read_sav("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s9a2.sav")

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


Con_Quant_Fish <- Con_Quant1[, c(1,22:36)]
Con_Quant_Fish <- Con_Quant_Fish %>% mutate(across(c(2:16), .fns = ~./1000))
Con_Quant_Fish <- Con_Quant_Fish %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant_Fish <- rename(Con_Quant_Fish, c(hhold=hhold_qnt))

Con_Quant_Fish <- Con_Quant_Fish %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(2:16)))




Con_exp1 <- Con_exp1[, c(1,130,131)]
con_main <- Con_Quant_Fish %>% right_join(Con_exp1, by="hhold")




Con_price1 <- Con_price1[, c(1, 22:36)]
Con_price1 <- Con_price1 %>%  rename_with(~ paste0(.x, "_price"))
Con_price1 <- rename(Con_price1, c(hhold=hhold_price))




con_main <- con_main %>% right_join(Con_price1, by="hhold")

rm(Con_exp1, Con_price1, Con_Quant1, Con_Quant_Fish)




## Adding weekly food expenditure


con_week <- read_sav("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s9b.sav")

con1_week <- rename(con_week, c(hhold=HHOLD, item=CODE, qnt=QUAN, tk=VALU))
con1_week <- con1_week[, 2:7]
rm(con_week)

con1_week_wide <- con1_week %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con1_week_wide[is.na(con1_week_wide)] <- 0

con1_week_wide <- select(con1_week_wide, -c('210', '230'))

con1_week_wide <- con1_week_wide %>% rowwise() %>% 
            mutate(total_2week_expenditure = sum(c_across(2:20)))

con1_week_wide <- con1_week_wide[,c(1,21)]




con_main <- con_main %>% merge(con1_week_wide, by="hhold")

n_occur <- data.frame(table(con_main$hhold))
n_occur[n_occur$Freq>1, ]
rm(con1_week, con1_week_wide, n_occur)



## Adding Monthly non-food expenditure



con_month <- read_sav("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s9c.sav")

con1_month <- rename(con_month, c(hhold=HHOLD, item=CODE, tk=Q03_9C))

con1_month_wide <- con1_month %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con1_month_wide[is.na(con1_month_wide)] <- 0

con1_month_wide <- select(con1_month_wide, -c('123', '240', '250', '260', '270'))

con1_month_wide <- con1_month_wide %>% rowwise() %>% 
            mutate(total_monthly_nonfood_expenditure = sum(c_across(2:46)))

con1_month_wide <- con1_month_wide[,c(1,47)]



con_main <- con_main %>% merge(con1_month_wide, by="hhold")

rm(con_month, con1_month, n_occur, con1_month_wide)



## Adding yearly non-food expenditure


con_year <- read_sav("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s9d1.sav")

con1_year <- rename(con_year, c(hhold=HHOLD, item=CODE, tk=Q02_9D1))

con1_year_wide <- con1_year %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con1_year_wide[is.na(con1_year_wide)] <- 0

con1_year_wide <- select(con1_year_wide, -c('300', '330', '340'))

con1_year_wide <- con1_year_wide %>% rowwise() %>% 
            mutate(total_yearly_nonfood_expenditure = sum(c_across(2:45)))

con1_year_wide <- con1_year_wide[,c(1,46)]

rm(con_year, con1_year)




con_year <- read_sav("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s9d2.sav")

con1_year <- rename(con_year, c(hhold=HHOLD, item=CODE, tk=Q01_9D2))

con2_year_wide <- con1_year %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con2_year_wide[is.na(con2_year_wide)] <- 0

con2_year_wide <- select(con2_year_wide, -c('36', '360', '390', '380', '410', '430',
                                            '450', '440', '470', '490', '500', '510',
                                            '520', '530', '550'))

con2_year_wide <- con2_year_wide %>% rowwise() %>% 
            mutate(total_yearly_nonfood_expenditure = sum(c_across(2:127)))

con2_year_wide <- con2_year_wide[,c(1,128)]

rm(con_year, con1_year)




con1_year_wide <- con1_year_wide %>% merge(con2_year_wide, by="hhold")

con1_year_wide$total_yearly_nonfood_expenditure <- con1_year_wide$total_yearly_nonfood_expenditure.x + con1_year_wide$total_yearly_nonfood_expenditure.y

con1_year_wide <- con1_year_wide[,c(1,4)]

rm(con2_year_wide)




con_main <- con_main %>% merge(con1_year_wide, by="hhold")

n_occur <- data.frame(table(con_main$hhold))
n_occur[n_occur$Freq>1, ]

rm(con1_year_wide, n_occur)



## Adding income from fisheries


fish_production <- read_sav("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Collaboration/TxState_BAU/Data/HIES-2005-20210719T073832Z-001/HIES-2005/SPSS files/s7c3.sav")

fish_production <- rename(fish_production, c(hhold=HHOLD, item=SOC, tk=Q02B_7C3))

fish_production_wide <- fish_production %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

fish_production_wide <- fish_production_wide[,-11]

fish_production_wide[is.na(fish_production_wide)] <- 0

fish_production_wide <- fish_production_wide %>% rowwise() %>% 
            mutate(total_yearly_sell_fishproduction = sum(c_across(2:10)))

fish_production_wide <- fish_production_wide[,c(1,11)]




con_main1 <- con_main %>% left_join(fish_production_wide, by="hhold")

con_main1[is.na(con_main1)] <- 0

rm(fish_production, fish_production_wide, n_occur)



# Exporting data


write.csv(con_main1, file = "fish_HIES2005.csv", row.names = F)




























