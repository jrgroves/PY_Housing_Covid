#Import and clean maps and get census data
#Jeremy R. Groves
#June 20, 2023

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Read in GIS maps
  map<-st_read(dsn="./Build/Input/Maps/oahtmk.shp")
  #map.s<-st_read(dsn="./Build/Input/Maps/Elementary_School_Areas.shp")

#Setup maps
  map <- map %>%
    select(TMK9TXT, TAXPIN, REC_AREA_S, TYPE, STREET_PAR, GISAcres) %>%
    rename("TMK" = "TMK9TXT") %>%
    mutate(par_area = st_area(map),
           lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

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

cen.map <- census %>%
  select(GEOID, geometry) %>%
  distinct() %>%
  st_transform(., crs=st_crs(map)) %>%
  st_intersection(., map)  %>%
  select(TMK, GEOID, par_area, lat, lon) %>%
  distinct()

cen.map <- cen.map %>%
  mutate(in_area = st_area(cen.map),
         weight = in_area / par_area)
  
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
         per_renter = owner/occupied) %>%
  distinct()

cen.data2<-cen.map %>%
  left_join(., cen.data, by="GEOID") %>%
  filter(!is.na(per_white)) %>%
  select(-c(GEOID, par_area, in_area)) %>%
  mutate(weight = as.numeric(weight)) %>%
  mutate(across(population:per_renter, function(x) x*weight)) %>%
  st_drop_geometry() %>%
  aggregate(. ~ TMK, ., sum) %>%
  filter(weight <1.1) %>%
  distinct()

load("./Build/Output/core.RData")

link <- core %>%
  left_join(., cen.data2, by="TMK") %>%
  filter(!is.na(YearBuilt)) %>%
  filter(!is.na(per_white))
