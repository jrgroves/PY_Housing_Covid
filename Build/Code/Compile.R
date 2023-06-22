#Compiles sales, map, and covid data and preps for analysis
#Jeremy R. Groves
#June 22, 2023

rm(list=ls())

library(tidyverse)
library(readxl)

#Read in Pre-processed data

  load("./Build/Output/core.RData")
  load("./Build/Output/MapData.RData")

#Load Covid Data from https://public.tableau.com/app/profile/docd.epi/viz/HawaiiCOVID-19WorkbooksandData/EpidemicCurves

  raw.data<-read_excel("./Build/Input/Hawaii_Covid.xlsx")
  
  covid <- raw.data %>%
    filter(County=="Honolulu") %>%
    rename("Cases" = "New Cases",
           "P.Tests" = "New Positive Tests",
           "Tests" = "Total Test Encounters")

#Merge Covid and Map Data with Core Data
  core<-core %>%
    mutate(Date = CloseDate) %>%
    left_join(., covid, by="Date") %>%
    left_join(., map.data, by="TMK") %>%
    filter(!is.na(lat))  #removes about 111 units not in map data 
  
#Remove data not used and clean usable data for analysis
  core.2 <- core %>%
    select(-c(AssociationFee2Includes, AssociationFeeIncludes, AssociationFeeTotal, Easements,
              ElementarySchool, FeeOptions, FeePurchase, HighSchool, Inclusions, LandTenure, Location,
              LotFeatures, MaintenanceExpense, MiddleOrJuniorSchool, MLSAreaMajor, ParkingFeatures, RoadFrontage,
              Roof, SQFTLanaiCovered, SQFTLanaiOpen, SQFTRoofedLiving, StreetName, StreetNumber, StreetSuffix,
              TaxAmount, TaxAssessedValue, TaxAssessedValueLand, TaxAssessedValueImprovements, Utilities, dom, 
              BuildingName, BuildingNa, County)) %>%
    filter(Age >=0) %>%
    mutate(Covid = case_when(Cases >=0 ~ 1,
                             TRUE ~ 0),
           HOA = case_when(AssociationFee >= 0 ~ AssociationFee,
                           TRUE ~ 0),
           Elevator = case_when(ElevatorsNumberOf >= 0 ~ ElevatorsNumberOf,
                                TRUE ~ 1)) %>%
    filter(PostalCode > 0,
           !is.na(PostalCode)) %>%
    mutate(mon.yr = format(as.Date(CloseDate), "%Y-%m"))
  
save(core.2, file="./Build/Output/CoreData.RData")

