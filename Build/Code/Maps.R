#Import and clean maps and get census data
#Jeremy R. Groves
#June 20, 2023

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Read in core data and pull TMK for limiting of parcel maps

  load("./Build/Output/core.RData")
  
  tmk <- core %>%
    select(TMK) %>%
    distinct() %>%
    mutate(cdata = 1)
  
  rm(core)

#Read in GIS maps
  map<-st_read(dsn="./Build/Input/Maps/oahtmk.shp")
 
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

#Limit Parcel map and create centroid map

  map <- map %>%
    select(TMK9TXT, TAXPIN, REC_AREA_S, TYPE, STREET_PAR, GISAcres) %>%
    rename("TMK" = "TMK9TXT") 
  
  m.data <- map %>%
    filter(TMK %in% tmk$TMK) %>%
    mutate(par_area = st_area(.),
           lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
  
  m.cen<-m.data %>%
    select(TMK, lon, lat) %>%
    st_drop_geometry()
  
  m.cen <- st_as_sf(m.cen, crs = st_crs(map), coords = c("lon", "lat"))
  
#Create Distance to "beach" defined as edge of map

    map2 <- st_read(dsn="./Build/Input/Maps/Hawaii_State_Senate_Districts_2022.shp")
    
    map2 <- map2 %>%
      st_transform(., crs=st_crs(map)) %>%
      filter(county=="OAHU") %>%
      select(county) %>%
      st_union() %>%
      st_cast(to = "POLYGON") %>%
      st_cast(to ="LINESTRING")

    dist<-as.data.frame(st_distance(m.cen, map2[1])) #This calculates the min distance from point to edge of island
    
    dist <- dist %>%
      rename("Beach" = 'st_distance(m.cen, map2[1])') 
    
    m.data<-cbind(m.data, dist)
 
#Create Distance to Parks
    
    map2 <- st_read(dsn="./Build/Input/Maps/Park_Types_Data.shp")
    
    map2 <- map2 %>%
      select(objectid) %>%
      st_transform(., crs=st_crs(map)) %>%
      mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
      st_drop_geometry() %>%
      st_as_sf(., crs = st_crs(map), coords = c("lon", "lat"))
      
    dist<-as.data.frame(st_distance(m.cen, map2))
    
    dist <- dist %>%
      mutate(rowname = m.cen$TMK) %>%
      pivot_longer(-rowname) %>%
      group_by(rowname) %>%
      summarize(park = min(value)) %>%
      rename("TMK" = "rowname")
    
    m.data <- m.data %>%
      left_join(., dist, by="TMK")

#Create Distance to Hospitals
    
    map2 <- st_read(dsn="./Build/Input/Maps/Hospitals.shp")
    
    map2 <- map2 %>%
      filter(island == "OAHU") %>%
      select(objectid) %>%
      st_transform(., crs=st_crs(map)) %>%
      mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
      st_drop_geometry() %>%
      st_as_sf(., crs = st_crs(map), coords = c("lon", "lat"))
    
    dist<-as.data.frame(st_distance(m.data, map2))
    
    dist <- dist %>%
      mutate(rowname = m.cen$TMK) %>%
      pivot_longer(-rowname) %>%
      group_by(rowname) %>%
      summarize(hospital = min(value)) %>%
      rename("TMK" = "rowname")
    
    m.data <- m.data %>%
      left_join(., dist, by="TMK")
    
#Create Distance to Airports
    
    map2 <- st_read(dsn="./Build/Input/Maps/Airports.shp")
    
    map2 <- map2 %>%
      select(objectid) %>%
      st_transform(., crs=st_crs(map)) %>%
      mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
      st_drop_geometry() %>%
      st_as_sf(., crs = st_crs(map), coords = c("lon", "lat"))
    
    dist<-as.data.frame(st_distance(m.data, map2))
    
    dist <- dist %>%
      mutate(rowname = m.cen$TMK) %>%
      pivot_longer(-rowname) %>%
      group_by(rowname) %>%
      summarize(airport = min(value)) %>%
      rename("TMK" = "rowname")
    
    m.data <- m.data %>%
      left_join(., dist, by="TMK") %>%
      distinct()
    
#Get School Names
    map2 <- st_read(dsn="./Build/Input/Maps/Elementary_School_Areas.shp")
    
    map2 <- map2 %>%
      select(elem_desc) %>%
      st_transform(., crs=st_crs(map)) %>%
      st_intersection(., m.cen) %>%
      st_drop_geometry() %>%
      filter(!duplicated(TMK))
    
    m.data<-m.data %>%
      left_join(., map2, by="TMK")

    map2 <- st_read(dsn="./Build/Input/Maps/Middle_School_Areas.shp")    
    
    map2 <- map2 %>%
      select(int_desc) %>%
      rename("mid_desc" = "int_desc") %>%
      st_transform(., crs=st_crs(map)) %>%
      st_intersection(., m.cen) %>%
      st_drop_geometry() %>%
      filter(!duplicated(TMK))
    
    m.data<-m.data %>%
      left_join(., map2, by="TMK")
    
    map2 <- st_read(dsn="./Build/Input/Maps/High_School_Areas.shp")    
    
    map2 <- map2 %>%
      select(high_desc) %>%
      st_transform(., crs=st_crs(map)) %>%
      st_intersection(., m.cen) %>%
      st_drop_geometry() %>%
      filter(!duplicated(TMK))
    
    m.data<-m.data %>%
      left_join(., map2, by="TMK")
    
#Correct Flood Zone with Current Map
    
    map2 <- st_read(dsn="./Build/Input/Maps/Flood_Zones.shp")    
    
    map2 <- map2 %>%
      select(fld_zone) %>%
      st_transform(., crs=st_crs(map)) %>%
      st_intersection(., m.cen) %>%
      st_drop_geometry() %>%
      filter(!duplicated(TMK))
    
    m.data<-m.data %>%
      left_join(., map2, by="TMK")
#Create maps for visualizations and to obtain Census link

cen.map <- census %>%
  select(GEOID, geometry) %>%
  distinct() %>%
  st_transform(., crs=st_crs(map)) %>%
  st_intersection(., m.data)  %>%
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
  distinct() %>%
  select(-c(white, black, asian, hawaian, occupied, households, vacant, owner, renter))

#Merge Census Data and other Map Data

  map.data <- m.data %>%
    st_drop_geometry() %>%
    select(-c(TAXPIN, REC_AREA_S, TYPE, STREET_PAR, GISAcres, lon, lat)) %>%
    left_join(., cen.data2, by="TMK") %>%
    filter(!is.na(per_white))

save(map.data, file="./Build/Output/MapData.RData")
