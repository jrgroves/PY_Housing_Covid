#Compiles sales, map, and covid data and preps for analysis
#Jeremy R. Groves
#June 22, 2023

rm(list=ls())

library(tidyverse)
library(readxl)


#Read in Pre-processed data

  load("./Build/Output/core.RData")
  load("./Build/Output/MapData.RData")
  
    map.data2 <- map.data %>%
          sf::st_drop_geometry()

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
    left_join(., covid, by="Date")  %>%
    left_join(., map.data2, by="TMK") %>%
    filter(!is.na(lat))  #removes about 111 units not in map data 
  
#Remove data not used and clean usable data for analysis
  main <- core %>%
    select(-c(AssociationFee2Includes, AssociationFeeIncludes, AssociationFeeTotal, Easements,
              ElementarySchool, FeeOptions, FeePurchase, HighSchool, Inclusions, LandTenure, Location,
              LotFeatures, MaintenanceExpense, MiddleOrJuniorSchool, MLSAreaMajor, ParkingFeatures, RoadFrontage,
              Roof, SQFTLanaiCovered, SQFTLanaiOpen, StreetName, StreetNumber, StreetSuffix,
              TaxAmount, TaxAssessedValue, TaxAssessedValueLand, TaxAssessedValueImprovements, Utilities, dom, 
              BuildingName, BuildingNa, County, Stories1, FloodZone)) %>%
    mutate(Covid = case_when(Cases >=0 ~ 1,
                             TRUE ~ 0),
           HOA = case_when(AssociationFee >= 0 ~ AssociationFee,
                           TRUE ~ 0),
           Elevator = case_when(ElevatorsNumberOf >= 0 ~ ElevatorsNumberOf,
                                TRUE ~ 1)) %>%
    filter(PostalCode > 0,
           !is.na(PostalCode)) %>%
    mutate(mon.yr = format(as.Date(CloseDate), "%Y-%m"),
           Stories = factor(Stories, levels=c("One","Two","Multi")),
           LUC = relevel(factor(LUC), ref="Residential"),
           cond = factor(cond, levels=c("Average","Excellent","Above Average","Fair")),
           fld_zone = relevel(factor(fld_zone), ref = "AE"),
           remod = case_when(YearRemodeled == 0 ~ 0,  #Dummy for remodel
                             TRUE ~ 1),
           Covid = Covid + 0,
           par_area = as.numeric(par_area)*0.000247105,
           beach = as.numeric(beach)/1000,
           park = as.numeric(park)/1000,
           hospital = as.numeric(hospital)/1000,
           airport = as.numeric(airport)/1000,
           elem_sch = as.numeric(elem_sch)/1000,
           mid_sch = as.numeric(mid_sch)/1000,
           high_sch = as.numeric(high_sch)/1000,
           Covid2 = case_when(CloseDate <= "2020-03-01" ~ 0,
                             CloseDate >  "2022-06-30" ~ 0,
                             TRUE ~ 1),
           Age2 = Age2 / 1000,
           City = toupper(City),
           City = case_when(City == "EWA BEAHC" ~ "EWA BEACH",
                            City == "HON" ~ "HONOLULU",
                            City == "HON." ~ "HONOLULU",
                            City == "HONH" ~ "HONOLULU",
                            City == "HONOKUKU" ~ "HONOLULU",
                            City == "HONOLULLU" ~ "HONOLULU",
                            City == "HONOLULU," ~ "HONOLULU",
                            City == "HONOLUU" ~ "HONOLULU",
                            City == "HONOLUULU" ~ "HONOLULU",
                            City == "HONOULU" ~ "HONOLULU",
                            City == "KANOEHE" ~ "KANEOHE",
                            City == "PEARL" ~ "PEARL CITY",
                            City == "PEARL  CITY" ~ "PEARL CITY",
                            City == "EVA BEACH" ~ "EWA BEACH",
                            City == "EWA" ~ "EWA BEACH",
                            City == "KONEOHE" ~ "KANEOHE",
                            City == "WAIHPAHU" ~ "WAIPAHU",
                            City == "MILIANI" ~ "MILILANI",
                            City == "MILILANI MAUKA" ~ "MILILANI",
                            TRUE ~ City)) %>%
    rename("ListDate" = "ListingContractDate") %>%
    select(-c(AssociationFee, ElevatorsNumberOf, ParkingTotal, Topography, PropertyFrontage,SQFTRoofedOther,
              UnitFeatures)) %>%
    filter(!is.na(fld_zone), #Removes two observations with no flood zone
           TMK != 123001127) #Removes 171 observations causing a spike in 11/2016 for sale price. Likely coded wrong and all in same building.
  
  
#Create Single Observations set for Spatial Weights for Spatial Regressions
  main.s <- main %>%
    filter(CloseDate == max(CloseDate), .by=ParcelNumber) %>%
    filter(lnClose == max(lnClose), .by=ParcelNumber) %>%
    distinct(ParcelNumber, .keep_all = TRUE)  
  
  main.s2 <- main %>%
    filter(CloseDate == min(CloseDate), .by=ParcelNumber) %>%
    filter(lnClose == max(lnClose), .by=ParcelNumber) %>%
    distinct(ParcelNumber, .keep_all = TRUE)  
           
 save(main,main.s, main.s2, file="./Build/Output/CoreData.RData")

