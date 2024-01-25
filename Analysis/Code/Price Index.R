#Regressions for Price Index
#Jeremy R. Groves
#January 23, 2024

rm(list=ls())

library(tidyverse)
library(stargazer)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

#Regression Models Full Sample


mod1 <- lm(ln.r.close ~ factor(year), data = main) 
  temp<-as.data.frame(mod1$coefficients)
  
  index <- temp %>%
    mutate(year = row.names(.)) %>%
    rename("index" = "mod1$coefficients") %>%
    filter(., grepl("factor", year)) %>%
    mutate_at("year", str_replace, "factor(year)", "")
