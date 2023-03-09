#Import Housing Data, Geocode for lat and long, and pull census info
#Jeremy R. Groves
#Feb 27, 2023

rm(list=ls())

library(tidyverse)
library(readxl)
library(tidygeocoder)
library(tidycensus)

raw.data<-read_excel("./Build/Input/Housing sales data.xlsx")


address <- raw.data %>%
  select(ParcelNumb, Address) %>%
  mutate(Address = str_replace(Address, "(\\d) (\\d)","\\1-\\2")) %>%
  distinct(Address, .keep_all = TRUE) %>%
  slice(1:10000) %>%
  geocode(Address, method = 'census')

address2 <- raw.data %>%
  select(ParcelNumb, Address) %>%
  mutate(Address = str_replace(Address, "(\\d) (\\d)","\\1-\\2")) %>%
  distinct(Address, .keep_all = TRUE) %>%
  slice(10001:20000) %>%
  geocode(Address, method = 'census')

address3 <- raw.data %>%
  select(ParcelNumb, Address) %>%
  mutate(Address = str_replace(Address, "(\\d) (\\d)","\\1-\\2")) %>%
  distinct(Address, .keep_all = TRUE) %>%
  slice(20001:24104) %>%
  geocode(Address, method = 'census')

address <- address %>%
  bind_rows(address2) %>%
  bind_rows(address3)

b1<-address %>%
  filter(is.na(lat))




