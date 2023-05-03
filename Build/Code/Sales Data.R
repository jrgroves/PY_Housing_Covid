#Import and Process Housing Sale Price Data

#Jeremy R. Groves
#May 3, 2023

rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)

#Read in Raw Sales Data from PY and Others
raw.data<-read_excel("./Build/Input/Housing sales data.xlsx")
cpi <- read.csv("./Build/Input/CPI.csv") 


data <- raw.data %>%
  mutate(TMK = substr(gsub("-","",ParcelNumb),2,9),
         TMK.condo = substr(gsub("-","",ParcelNumb),10,13),
         TMK = paste0("1",TMK)) %>%
  filter(Bathrooms != 0)


price.data <- data %>%
  select(TMK, TMK.condo, ClosePrice, CloseDate, ListingCon, ListPrice, year) %>%
  mutate(ID = paste0(TMK, TMK.condo,".",year),
         CloseDate = as.Date(CloseDate),
         ListingCon = as.Date(ListingCon),
         dom = as.numeric(CloseDate - ListingCon),
         Clo.mon = month(as.POSIXlt(CloseDate, format="%Y/%m/%d")),
         Series = case_when(Clo.mon < 7 ~ "HALF1",
                            Clo.mon > 6 ~ "HALF2",
                            TRUE ~ "0"),
         Series = paste(Series, year, sep=" ")) %>%
  filter(dom >= 0) %>%
  select(-Clo.mon) %>%
  left_join(., cpi, by="Series")

