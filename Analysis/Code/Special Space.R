#Analysis and Regressions for Hawaii Housing Data and Covid
#Jeremy R. Groves
#June 22, 2023
#NOTES:
#Only use Elementary Schools because there is too much overlap with middle and High Schools causing NAs

rm(list=ls())

library(tidyverse)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

temp <- main %>%
  select(TMK, TMK.condo, ParcelNumber, PropertyType, lat, lon) %>%
  distinct(., ParcelNumber, .keep_all = TRUE) %>%
  mutate(unit.id = paste0("UN", seq_len(n())))

temp2 <- main %>%
  select(TMK, TMK.condo, ParcelNumber, PropertyType, lat, lon) %>%
  distinct(., TMK, .keep_all = TRUE) %>%
  mutate(build.id = paste0("BLD", seq_len(n()))) %>%
  select(ParcelNumber, build.id)

core <- main %>%
  full_join(., temp, by="ParcelNumber") %>%
  full_join(., temp2, by="ParcelNumber")
