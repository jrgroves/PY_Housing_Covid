#Regressions for Hawaii Housing Data and Covid
#Jeremy R. Groves
#July 9, 2023

rm(list=ls())

library(tidyverse)
library(estimatr)
library(openxlsx)
library(stargazer)

#Functions
models <- c("mod6", "mod5", "mod4", "mod3", "mod2", "mod1")

rob.out<-function(x){
  temp <- x %>% 
    tidy() %>%
    select(term, estimate, std.error,statistic, p.value)
  return(temp)
}

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

main <- main %>%
  filter(PropertyType == "Condo_Townhouse") %>%
  select(-Duplex)

#Regression Models Full Sample


mod1 <- lm(ln.r.close ~ Covid, data = main) 

mod2 <- lm(ln.r.close ~ Covid + DOM + factor(year),data = main) 

mod3 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond),data=main)

mod4 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
             Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
             factor(LUC) + Parking + HOA + remod + Elevator, data = main)

mod5 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
             Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
             factor(LUC) + Parking + HOA + remod + Elevator +
             beach + park + hospital + airport  + elem_sch + 
             mid_sch + high_sch + lat + lon, 
           data=main)

mod6 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
             Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
             factor(LUC) + Parking + HOA + remod + Elevator +
             beach + park + hospital + airport  + elem_sch + 
             mid_sch + high_sch + lat + lon + 
             per_black + per_asian + per_hawaian + per_owner + per_occupied, 
           data=main)

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "text", 
          out="./Analysis/Output/Reg1ct.txt")
save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Reg1ct.RData")

#Full Model with clustered standard errors

mod1 <- lm_robust(ln.r.close ~ Covid, 
                  cluster= PostalCode,
                  data = main) 

mod2 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year),
                  cluster= PostalCode,
                  data = main) 

mod3 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                  cluster= PostalCode,
                  data=main)

mod4 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator, 
                  cluster= PostalCode,
                  data=main)

mod5 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator +
                    beach + park + hospital + airport  + elem_sch + 
                    mid_sch + high_sch + lat + lon, 
                  cluster= PostalCode,
                  data=main)

mod6 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator +
                    beach + park + hospital + airport  + elem_sch + 
                    mid_sch + high_sch + lat + lon + 
                    per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                  cluster= PostalCode,
                  data=main)


j<-1
for(i in models){
  TEMP <- rob.out(get(i))
  ifelse(j==1, OUT <- TEMP, OUT<-left_join(OUT, TEMP, by= "term"))
  j <- j+1
}

save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Reg1ct_Robust.RData")
rob.xlsx <- write.xlsx(OUT, file = "./Analysis/Output/Reg1_Robct.xlsx", asTable = FALSE)

#Regressions with smaller, one-sale data (last sell in set)
main <- main.s %>%
  filter(PropertyType == "Condo_Townhouse")

mod1 <- lm_robust(ln.r.close ~ Covid, 
                  cluster= PostalCode,
                  data = main) 

mod2 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year),
                  cluster= PostalCode,
                  data = main) 

mod3 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                  cluster= PostalCode,
                  data=main)

mod4 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator, 
                  cluster= PostalCode,
                  data=main)

mod5 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator +
                    beach + park + hospital + airport  + elem_sch + 
                    mid_sch + high_sch + lat + lon, 
                  cluster= PostalCode,
                  data=main)

mod6 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator +
                    beach + park + hospital + airport  + elem_sch + 
                    mid_sch + high_sch + lat + lon + 
                    per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                  cluster= PostalCode,
                  data=main)

save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Reg2ct_Robust.RData")

j<-1
for(i in models){
  TEMP <- rob.out(get(i))
  ifelse(j==1, OUT <- TEMP, OUT<-left_join(OUT, TEMP, by= "term"))
  j <- j+1
}

rob.xlsx <- write.xlsx(OUT, file = "./Analysis/Output/Reg2ct_Rob.xlsx", asTable = FALSE)

#Regressions with smaller, one-sale data (first sell in set)
main <- main.s2 %>%
  filter(PropertyType == "Condo_Townhouse")

mod1 <- lm_robust(ln.r.close ~ Covid, 
                  cluster= PostalCode,
                  data = main) 

mod2 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year),
                  cluster= PostalCode,
                  data = main) 

mod3 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                  cluster= PostalCode,
                  data=main)

mod4 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator, 
                  cluster= PostalCode,
                  data=main)

mod5 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator +
                    beach + park + hospital + airport  + elem_sch + 
                    mid_sch + high_sch + lat + lon, 
                  cluster= PostalCode,
                  data=main)

mod6 <- lm_robust(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                    Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                    Split + PUD + LowRise + HighRise + Townhouse + Condotel +  WalkUP + SingleFam +
                    factor(LUC) + Parking + HOA + remod + Elevator +
                    beach + park + hospital + airport  + elem_sch + 
                    mid_sch + high_sch + lat + lon + 
                    per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                  cluster= PostalCode,
                  data=main)

save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Reg2ct2_Robust.RData")

j<-1
for(i in models){
  TEMP <- rob.out(get(i))
  ifelse(j==1, OUT <- TEMP, OUT<-left_join(OUT, TEMP, by= "term"))
  j <- j+1
}

rob.xlsx <- write.xlsx(OUT, file = "./Analysis/Output/Reg2cts_Rob.xlsx", asTable = FALSE)
