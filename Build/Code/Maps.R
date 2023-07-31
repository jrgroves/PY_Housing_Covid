#Import and clean maps and get census data
#Jeremy R. Groves
#June 20, 2023

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)


#Read in core data and pull TMK for limiting of parcel maps. This is created by the 
#Data setup.R file

  load("./Build/Output/core.RData")
  
  tmk.year <- core %>%
    select(ParcelNumber, TMK, year) %>%
    mutate(year.c = case_when(year==2022 ~ 2021,
                              year==2023 ~ 2021,
                              TRUE ~ year))
  
  tmk <- core %>%
    select(TMK) %>%
    distinct()
  
  rm(core)

#Read in GIS maps
  map<-st_read(dsn="./Build/Input/Maps/oahtmk.shp")
 
#Limit Parcel map and create centroid map

  map <- map %>%
    select(TMK9TXT, TAXPIN, REC_AREA_S, TYPE, STREET_PAR, GISAcres) %>%
    rename("TMK" = "TMK9TXT") 
  
  m.data <- map %>%
    filter(TMK %in% tmk$TMK) %>%
    group_by(TMK) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    mutate(par_area = st_area(.),
           lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
 
#Create Distance to "beach" defined as edge of map

    map2 <- st_read(dsn="./Build/Input/Maps/Hawaii_State_Senate_Districts_2022.shp")
    
    map2 <- map2 %>%
      st_transform(., crs=st_crs(map)) %>%
      filter(county=="OAHU") %>%
      select(county) %>%
      st_union() %>%
      st_cast(to = "POLYGON") %>%
      st_cast(to ="LINESTRING")

    dist<-as.data.frame(st_distance(m.data, map2[1])) #This calculates the min distance from point to edge of island
                                                      #Map2 has three objects and the main island outline is object 1
    
    dist <- dist %>%
      rename("beach" = 'st_distance(m.data, map2[1])') 
    
    m.data<-cbind(m.data, dist)
 
#Create Distance to Parks
    
    map2 <- st_read(dsn="./Build/Input/Maps/Park_Types_Data.shp")
    
    map2 <- map2 %>%
      select(objectid) %>%
      st_transform(., crs=st_crs(map))
    
    dist<-as.data.frame(st_distance(m.data, st_centroid(map2)))
    
    
    dist <- dist %>%
      mutate(rowname = m.data$TMK) %>%
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
      st_transform(., crs=st_crs(map)) 
    
    dist<-as.data.frame(st_distance(m.data, map2))
    
    dist <- dist %>%
      mutate(rowname = m.data$TMK) %>%
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
      st_transform(., crs=st_crs(map))
    
    dist<-as.data.frame(st_distance(m.data, st_centroid(map2)))
    
    dist <- dist %>%
      mutate(rowname = m.data$TMK) %>%
      pivot_longer(-rowname) %>%
      group_by(rowname) %>%
      summarize(airport = min(value)) %>%
      rename("TMK" = "rowname")
    
    m.data <- m.data %>%
      left_join(., dist, by="TMK") 
    
#Get School Names
    map3 <- st_read(dsn="./Build/Input/Maps/Public_Schools.shp")
    
    map3 <- map3 %>%
      st_transform(., crs=st_crs(map))%>%
      select(sch_code)
    
    schbrid<-read.csv("./Build/Input/School Bridge.csv")
    
    brid <- schbrid %>%
      filter(grepl("Elementary",sch_type),
             Name != "") %>%
      rename("elem_desc" = "Name")
    
    map2 <- st_read(dsn="./Build/Input/Maps/Elementary_School_Areas.shp") %>%
      st_transform(., crs=st_crs(map)) 
    
    map2 <- map2 %>%
        left_join(., brid, by="elem_desc", relationship = "many-to-many")
    
    map2 <- map2 %>%
      select(sch_code) %>%
      filter(!is.na(sch_code)) %>%
      st_intersection(., m.data)%>%
      st_drop_geometry() %>%
      select(sch_code, TMK) %>%
      filter(!duplicated(TMK)) %>%
      left_join(., map3, by="sch_code") %>%
      mutate(lon.sch = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat.sch = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
      select(-geometry)
    
    m.data<-m.data %>%
      left_join(., map2, by="TMK") %>%
      mutate(sch_code = case_when( TMK == "173012014" ~ 412,
                                   TMK == "173010007" ~ 207,
                                   TRUE ~ sch_code),
             lon.sch = case_when( TMK == "173012014" ~ 778129.3,
                                  TMK == "173010007" ~ 597722.,
                                  TRUE ~ lon.sch),
             lat.sch = case_when( TMK == "173012014" ~ 2298021,
                                  TMK == "173010007" ~ 2377676,
                                  TRUE ~ lat.sch),
             elem_sch = (sqrt(((lon.sch - lon)^2)+((lat.sch - lat)^2)))) %>%
      select(-c(lon.sch, lat.sch, sch_code))

    #Middle Schools
    
    brid<- schbrid %>%
      filter(grepl("Intermediate" ,sch_type) | grepl("Middle", sch_type),
             Name != "")    %>%
      rename("int_desc" = "Name")
    
    map2 <- st_read(dsn="./Build/Input/Maps/Middle_School_Areas.shp")  %>%  
      st_transform(., crs=st_crs(map)) %>%
      left_join(., brid, by="int_desc", relationship = "many-to-many")
    
    map2 <- map2 %>%
      select(sch_code) %>%
      filter(!is.na(sch_code)) %>%
      st_intersection(., m.data) %>%
      st_drop_geometry() %>%
      select(sch_code, TMK) %>%
      filter(!duplicated(TMK)) %>%
      left_join(., map3, by="sch_code") %>%
      mutate(lon.sch = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat.sch = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
      select(-geometry)
      
    m.data<-m.data %>%
      left_join(., map2, by="TMK") %>%
      mutate(mid_sch = (sqrt(((lon.sch - lon)^2)+((lat.sch - lat)^2)))) %>%
      select(-c(lon.sch, lat.sch, sch_code))
    
    #High Schools
    
    brid<- schbrid %>%
      distinct() %>%
      filter(grepl("High" ,sch_type) | grepl("Waialua High and Intermediate", Name) |
               grepl("Kahuku High and Intermediate", Name) |
               grepl("Nanakuli High and Intermediate", Name),
             Name != "")    %>%
      distinct() %>%
      rename("high_desc" = "Name")
    
    map2 <- st_read(dsn="./Build/Input/Maps/High_School_Areas.shp")%>%
      st_transform(., crs=st_crs(map)) %>%
      left_join(., brid, by="high_desc", relationship = "many-to-many")
    
    map2 <- map2 %>%
      select(sch_code) %>%
      filter(!is.na(sch_code)) %>%
      st_intersection(., m.data) %>%
      st_drop_geometry() %>%
      select(sch_code, TMK) %>%
      filter(!duplicated(TMK)) %>%
      left_join(., map3, by="sch_code") %>%
      mutate(lon.sch = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat.sch = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
      select(-geometry)
     
    m.data<-m.data %>%
      left_join(., map2, by="TMK") %>%
      mutate(high_sch = (sqrt(((lon.sch - lon)^2)+((lat.sch - lat)^2)))) %>%
      select(-c(lon.sch, lat.sch, sch_code))
    
#Correct Flood Zone with Current Map
    
    map2 <- st_read(dsn="./Build/Input/Maps/Flood_Zones.shp")    
    
    map2 <- map2 %>%
      select(fld_zone) %>%
      st_transform(., crs=st_crs(map)) %>%
      st_intersection(., m.data)%>%
      st_drop_geometry() %>%
      select(TMK, fld_zone) %>%
      filter(!duplicated(TMK))
    
    m.data<-m.data %>%
      left_join(., map2, by="TMK")
    
#Pull Down Census Data from ACS 5 year files for each year
 for(y in seq(2016, 2021, 1)){
       
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
                      year = y,
                      state = 15,
                      geometry = TRUE)
    
    census<-st_transform(census, crs=st_crs(map))
    
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
             per_renter = renter/occupied) %>%
      distinct()
    

  cen.map <- census %>%
    select(GEOID, geometry) %>%
    distinct() %>%
    st_transform(., crs=st_crs(map)) %>%
    st_intersection(., m.data)%>%
    select(TMK, GEOID, par_area) %>%
    distinct()
  
  cen.map <- cen.map %>%
    mutate(in_area = st_area(cen.map),
           weight = in_area / par_area)
    
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
    select(-c(white, black, asian, hawaian, occupied, households, vacant, owner, renter)) %>%
    mutate(year.c = y)
  
  ifelse(y==2016,CEN <- cen.data2, CEN <- rbind(CEN, cen.data2))
 }
    
#Merge Census Data and other Map Data
 
  map.data <- m.data %>%
    left_join(., tmk.year, by="TMK", relationship = "one-to-many")  %>%
    left_join(., CEN, by=c("TMK", "year.c")) %>%
    filter(!is.na(per_white),
           population > 10) %>%
    select(-year.c)

save(map.data, file="./Build/Output/MapData.RData")
