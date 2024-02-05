#Regressions for Price Index
#Jeremy R. Groves
#January 23, 2024

rm(list=ls())

library(tidyverse)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

#Regression Models Full Sample

core<- main %>%
  filter(PropertyType == "Single Family") %>%
  filter(year < 2023)

mod1 <- lm(ln.r.close ~ factor(mon.yr) - 1, data = core) 
  temp<-as.data.frame(mod1$coefficients)
  
  index <- temp %>%
    mutate(mon.yr = gsub("[()]", "", row.names(.)),
           mon.yr = as.character(gsub("factormon.yr", "", mon.yr)))  %>%
    rename("index" = "mod1$coefficients") %>%
    mutate(index2 = index/index[1],
           time = 1:n(),
           covid = case_when(time < 51 ~ "Pre",
                             TRUE ~ "Post")) %>%
    remove_rownames()
  
  ggplot(index, aes(time, index2, color = covid))+
    geom_point() +
    geom_vline(xintercept = 51) +
    geom_smooth(method = lm)
  
   mod6 <- lm(ln.r.close ~ DOM + factor(mon.yr) + BedsTotal + BathsFull + BathsHalf + 
               Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
               factor(LUC) + Parking + HOA + remod + Elevator +
               beach + park + hospital + airport  + elem_sch + 
               mid_sch + high_sch + lat + lon + 
               per_black + per_asian + per_hawaian + per_owner + per_occupied - 1, 
             data=core)
  temp<-as.data.frame(mod6$coefficients)
  
  index <- temp %>%
    mutate(mon.yr = gsub("[()]", "", row.names(.))) %>%
    filter(., grepl("factormon.yr", mon.yr)) %>%
    mutate(mon.yr = as.character(gsub("factormon.yr", "", mon.yr)))  %>%
    rename("index" = "mod6$coefficients") %>%
    mutate(index2 = index/index[1],
           time = 1:n(),
           covid = case_when(time < 51 ~ "Pre",
                             TRUE ~ "Post"))
  
  ggplot(index, aes(time, index2, color = covid))+
    geom_point() +
    geom_vline(xintercept = 51) +
    geom_smooth(method = lm)
  
  mod6 <- lm(ln.r.close ~ Covid + DOM + factor(mon.yr) + BedsTotal + BathsFull + BathsHalf + 
               Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
               factor(LUC) + Parking + HOA + remod + Elevator +
               beach + park + hospital + airport  + elem_sch + 
               mid_sch + high_sch + lat + lon + 
               per_black + per_asian + per_hawaian + per_owner + per_occupied - 1, 
             data=main)
  temp<-as.data.frame(mod6$coefficients)
  
  index <- temp %>%
    mutate(mon.yr = gsub("[()]", "", row.names(.))) %>%
    filter(., grepl("factormon.yr", mon.yr)) %>%
    mutate(mon.yr = as.character(gsub("factormon.yr", "", mon.yr)))  %>%
    rename("index" = "mod6$coefficients") %>%
    mutate(index2 = index/index[1],
           time = 1:n(),
           covid = case_when(time < 51 ~ "Pre",
                             TRUE ~ "Post"))
  
  ggplot(index, aes(time, index2, color = covid))+
    geom_point() +
    geom_vline(xintercept = 51) +
    geom_smooth(method = lm)
  