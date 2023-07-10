#Regressions for Hawaii Housing Data and Covid
#Jeremy R. Groves
#July 9, 2023

rm(list=ls())

library(tidyverse)
library(estimatr)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

#Regression Models

mod1 <- lm_robust(ln.r.close ~ Covid, 
                  cluster= City,
                  data = main) 

mod2 <- lm(ln.r.close ~ Covid + DOM + factor(year), 
           cluster = City,
           data = main) 

mod3 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
           cluster = City,
           data=main)

mod4 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
             Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
             factor(LUC) + Parking + HOA + remod + Elevator, 
           cluster = City,
           data=main)

mod5 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
             Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
             factor(LUC) + Parking + HOA + remod + Elevator +
             beach + park + hospital + airport  + elem_sch + 
             mid_sch + high_sch + lat + lon, 
           cluster = City,
           data=main)

mod6 <- lm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
             Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
             Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
             factor(LUC) + Parking + HOA + remod + Elevator +
             beach + park + hospital + airport  + elem_sch + 
             mid_sch + high_sch + lat + lon + 
             per_black + per_asian + per_hawaian + per_owner + per_occupied, 
           Cluster = City,
           data=main)

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "text", 
          file="./Analysis/Output/Reg1.txt")