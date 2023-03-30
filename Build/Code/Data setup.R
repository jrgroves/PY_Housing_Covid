#Import Housing Data,  and pull census info
#Jeremy R. Groves
#Feb 27, 2023

rm(list=ls())

library(tidyverse)
library(readxl)
library(sf)
library(tidycensus)

#Read in Raw Sales Data from PY and Others
    raw.data<-read_excel("./Build/Input/Housing sales data.xlsx")
   
    data <- raw.data %>%
      mutate(TMK = substr(gsub("-","",ParcelNumb),2,9),
             TMK.condo = substr(gsub("-","",ParcelNumb),10,13)) %>%
      filter(Bathrooms != 0)

    
    map.1<-st_read(dsn="./Build/Input/Maps/oahtmk.shp")
    
    map <- map.1 %>%
      select(TMK9TXT, TAXPIN, REC_AREA_S, TYPE, STREET_PAR, GISAcres) %>%
      rename("TMK" = "TMK9TXT")

#Download ACS data and process maps
    census <- get_acs(geography = "block group",
                      variables = c(population = "B01001_001",
                                    white = "B02001_002",
                                    black = "B02001_003",
                                    asian = "B02001_005",
                                    hawaian = "B02001_006",
                                    households = "B25002_001",
                                    occupied = "B25002_002",
                                    vacant = "B25002_003",
                                    owner = "B25003_002",
                                    renter = "B25003_003"),
                      year = 2021,
                      state = 15,
                      geometry = TRUE)

#Create maps for visualizations and to obtain Census link
    map.data <- data %>%
      mutate(Sale = 1) %>%
      select(TMK, Sale) %>%
      distinct() 
    
    map.sale <- map %>%
      full_join(., map.data, by="TMK") %>%
      mutate(Sale = case_when(Sale == 1 ~ 1,
                              TRUE ~ 0)) %>%
      filter(!is.na(TAXPIN))
    
    cen.map <- census %>%
      select(GEOID, geometry) %>%
      distinct() %>%
      st_transform(., crs=st_crs(map.sale))
    
    sale.geo <- map.sale %>%
      filter(Sale == 1) %>%
      st_intersection(., cen.map) %>%
      select(TMK, GEOID)

#Create core data set
    cen.data <- census %>%
      st_drop_geometry() %>%
      select(GEOID, variable, estimate) %>%
      pivot_wider(names_from = "variable", values_from = "estimate", id_cols = "GEOID") %>%
      filter(population != 0,
             households != 0) %>%
      mutate(per_white = white/population,
             per_black = black/population,
             per_asian = asian/population,
             per_hawaian = hawaian/population,
             per_occupied = occupied/households,
             per_vacant = vacant/households,
             per_owner = owner/occupied,
             per_renter = owner/occupied)
    
    core <- data %>%
      left_join(., sale.geo, by = "TMK") %>%
      left_join(., cen.data, by = "GEOID") %>%
      filter(!is.na(population))

#Setup House Data Fields
    
    char.data.sfh <- data %>%
      filter(PropertyTy=="Single Family",
             Bedrooms != 0) %>%
      select(ParcelNumb, BathsFull, BathsHalf, Bathrooms, Bedrooms, SqftTotal, FloodZone,YearBuilt,
             StoriesTyp, Zoning, PropertyTy, Flooring, Roof, SQFTRoofed, YearRemode, )
    
    price.data <- data %>%
      select(TMK, TMK.condo, ClosePrice, ListingCon, ListPrice, year, )