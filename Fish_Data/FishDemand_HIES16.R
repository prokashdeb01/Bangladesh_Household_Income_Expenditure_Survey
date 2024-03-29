
# Library


library(dplyr)
library(haven)
library(tidyverse)
library(tidyr)
library(tidyr)



## Importing Data for Price and Quantity


con <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/HH_SEC_9A2.dta")
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


Con_Quant_Fish <- Con_Quant[, c(1,2,23:39)]
Con_Quant_Fish <- Con_Quant_Fish %>% mutate(across(c(3:19), .fns = ~./1000))
Con_Quant_Fish <- Con_Quant_Fish %>% rename_with(~ paste0(.x, "_qnt"))
Con_Quant_Fish <- rename(Con_Quant_Fish, c(id=id_qnt, hhold=hhold_qnt))

Con_Quant_Fish <- Con_Quant_Fish %>%  rowwise() %>% 
                    mutate(total_fish_quantity = sum(c_across(3:19)))
Con_Quant_Fish <- Con_Quant_Fish[,-1]



Con_exp <- Con_exp[, c(2,136,137)]
con_main <- Con_Quant_Fish %>% right_join(Con_exp, by="hhold")



Con_price <- Con_price[, -1]
Con_price <- Con_price[, c(1, 22:38)]
Con_price <- Con_price %>%  rename_with(~ paste0(.x, "_price"))
Con_price <- rename(Con_price, c(hhold=hhold_price))



con_main <- con_main %>% right_join(Con_price, by="hhold")



# Finding dubplicate hhold id


n_occur <- data.frame(table(con_main$hhold))
n_occur[n_occur$Freq>1, ]
con_main1 <- con_main[!duplicated(con_main$hhold), ]
rm(Con_exp, con_main, Con_price, Con_Quant, Con_Quant_Fish, n_occur)




## Adding weekly food expenditure



con_week <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/HH_SEC_9B2.dta")

con1_week <- rename(con_week, c(item=s9bq01, qnt=s9bq02, 
                                unit=s9b2q03, tk=s9bq04, source=s9bbq05))
con1_week <- con1_week[, 4:9]
rm(con_week)

con1_week_wide <- con1_week %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con1_week_wide[is.na(con1_week_wide)] <- 0

con1_week_wide <- con1_week_wide[,-21]

con1_week_wide <- con1_week_wide %>% rowwise() %>% 
            mutate(total_2week_expenditure = sum(c_across(2:20)))

con1_week_wide <- con1_week_wide[,c(1,21)]




con_main2 <- con_main1 %>% merge(con1_week_wide, by="hhold")

n_occur <- data.frame(table(con_main2$hhold))
n_occur[n_occur$Freq>1, ]
rm(con1_week, con1_week_wide, con_main1)



## Adding Monthly non-food expenditure



con_month <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/HH_SEC_9C.dta")

con1_month <- rename(con_month, c(item=s9cq00, tk=s9cq03))

con1_month_wide <- con1_month %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con1_month_wide[is.na(con1_month_wide)] <- 0

con1_month_wide <- con1_month_wide[,-51]

con1_month_wide <- con1_month_wide %>% rowwise() %>% 
            mutate(total_monthly_nonfood_expenditure = sum(c_across(2:50)))

con1_month_wide <- con1_month_wide[,c(1,51)]



con_main2 <- con_main2 %>% merge(con1_month_wide, by="hhold")

n_occur <- data.frame(table(con_main2$hhold))
n_occur[n_occur$Freq>1, ]

rm(con_month, con1_month, n_occur, con1_month_wide)



## Adding yearly non-food expenditure


con_year <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/HH_SEC_9D2.dta")

con1_year <- rename(con_year, c(item=s9d2q00, tk=s9d2q01))

con1_year_wide <- con1_year %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

con1_year_wide[is.na(con1_year_wide)] <- 0

con1_year_wide <- con1_year_wide[,-136]

con1_year_wide <- con1_year_wide %>% rowwise() %>% 
            mutate(total_yearly_nonfood_expenditure = sum(c_across(2:135)))

con1_year_wide <- con1_year_wide[,c(1,136)]




con_main2 <- con_main2 %>% merge(con1_year_wide, by="hhold")

n_occur <- data.frame(table(con_main2$hhold))
n_occur[n_occur$Freq>1, ]

rm(con_year, con1_year, con1_year_wide, n_occur)



## Adding income from fisheries


fish_production <- read_dta("C:/Users/pzd0035/OneDrive - Auburn University/TxState/RESEARCH/Data/HIES 2016/HH_SEC_7C3.dta")

fish_production <- rename(fish_production, c(item=s7c3q00, tk=s7c3q11b))

fish_production_wide <- fish_production %>% group_by(hhold, item) %>% 
  summarise(tk = sum(tk)) %>%  ungroup() %>%  spread(item, tk)

fish_production_wide <- fish_production_wide[,-c(10,11)]

fish_production_wide[is.na(fish_production_wide)] <- 0

fish_production_wide <- fish_production_wide %>% rowwise() %>% 
            mutate(total_yearly_sell_fishproduction = sum(c_across(2:9)))

fish_production_wide <- fish_production_wide[,c(1,10)]




con_main2 <- con_main2 %>% merge(fish_production_wide, by="hhold")

n_occur <- data.frame(table(con_main2$hhold))
n_occur[n_occur$Freq>1, ]

rm(fish_production, fish_production_wide, n_occur)




# Exporting data


write.csv(con_main2, file = "fish_HIES2016.csv", row.names = F)










































