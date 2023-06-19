#Import Housing Data,  and pull census info
#Jeremy R. Groves
#Feb 27, 2023

rm(list=ls())

library(tidyverse)
library(readxl)
library(sf)
library(tidycensus)

#Read in Raw Sales Data from PY and Others
    raw.data<-read_excel("./Build/Input/Sales Data 1.xlsx")
    
    raw.data$CloseDate<-as.POSIXlt(raw.data$CloseDate, format="%m_%d_%Y",tz="UTC")
    raw.data$ListingContractDate<-as.POSIXlt(raw.data$ListingContractDate,format="%m_%d_%Y",tz="UTC")
    
    raw.data2<-read_excel("./Build/Input/Sales Data 2.xlsx")
    
  #cpi <- read.csv("./Build/Input/CPI.csv") 
   #   cpi <- cpi %>%
   #     mutate(Series = gsub("\\\n", "", Series.ID)) %>%
   #     select(-Series.ID)
      
    map.1<-st_read(dsn="./Build/Input/Maps/oahtmk.shp")
    map.s<-st_read(dsn="./Build/Input/Maps/Elementary_School_Areas.shp")

#Prepare Data
           
    data <- raw.data %>%
      bind_rows(raw.data2) %>%
      mutate(TMK = substr(gsub("-","",ParcelNumber),2,9),
             TMK.condo = substr(gsub("-","",ParcelNumber),10,13),
             TMK = paste0("1",TMK)) %>%
      filter(BathsTotal != 0)

    map <- map.1 %>%
      select(TMK9TXT, TAXPIN, REC_AREA_S, TYPE, STREET_PAR, GISAcres) %>%
      rename("TMK" = "TMK9TXT")
    
    
#Price Data####
    
    data <- data %>%
      mutate(dom = as.numeric((CloseDate - ListingContractDate))/86400,
             year = year(CloseDate),
             Clo.mon = month(as.POSIXlt(CloseDate, format="%Y/%m/%d")),
             Series = case_when(Clo.mon < 7 ~ "HALF1",
                                Clo.mon > 6 ~ "HALF2",
                                TRUE ~ "0"),
             Series = paste0(Series, year)) %>%
      filter(dom >= 0) %>%
      select(-Clo.mon) 
    
    %>%
      left_join(., cpi, by="Series") %>%
      mutate(real.hw.price = (ListPrice*342.499)/Hawii,
             real.wr.price = (ListPrice*356.733)/Western,
             lnList = log(ListPrice),
             lnreal.hw.list = log(real.hw.price),
             lnreal.wr.list = log(real.wr.price),
             real.hw.price = (ClosePrice*342.499)/Hawii,
             real.wr.price = (ClosePrice*356.733)/Western,
             lnreal.hw.close = log(real.hw.price),
             lnreal.wr.close = log(real.wr.price),
             diff.price = ListPrice - ClosePrice) %>%
      select(-Series)
    
#Characteristic Data####    

    data <- data %>%
      distinct() %>%
      filter(!is.na(StoriesType),
             !is.na(SqftTotal),
             !is.na(YearBuilt)) %>%
      rename("Stories" = "StoriesType") %>%
      mutate(Basement = case_when(grepl("Basement", Stories, fixed = TRUE) ~ 1,
                                  TRUE ~ 0 ),
             Split = case_when(grepl("Split Level", Stories, fixed=TRUE) ~ 1,
                               TRUE ~ 0),
             One = case_when(grepl("One", Stories, fixed=TRUE) ~ 1,
                             TRUE ~ 0),
             Two = case_when(grepl("Two", Stories, fixed=TRUE) ~ 1,
                             TRUE ~ 0),
             Three = case_when(grepl("Three+", Stories, fixed=TRUE) ~ 1,
                               grepl("Three", Stories, fixed=TRUE) ~ 1,
                               TRUE ~ 0),
             Number = case_when(grepl("[[:digit:]]+", Stories) ~ 1,
                                TRUE ~ 0),
             Stories1 = Stories,
             Stories = case_when(Three == 1 & Number == 0 ~ "Three+",
                                 Two == 1 & Three == 0 & Number == 0 ~ "Two",
                                 One == 1 & Two ==0 & Three == 0 & Number == 0 ~ "One",
                                 Number == 1 ~ "Multi",
                                 TRUE ~ "Other")) %>%
      select(-c("One", "Two", "Three", "Number")) %>%
      mutate(BuildType = ArchitecturalStyle,
             BuildType = gsub("No Unit Above or Below, ", "", BuildType),
             BuildType = gsub("No Unit Above or Below,", "", BuildType),
             BuildType = gsub(", No Unit Above or Below", "", BuildType),
             PUD = case_when(grepl("PUD", BuildType, fixed=TRUE) ~ 1,
                             TRUE ~ 0),
             LowRise = case_when(grepl("Low-Rise", BuildType, fixed=TRUE) ~ 1,
                                 TRUE ~ 0),
             HighRise = case_when(grepl("High-Rise", BuildType, fixed=TRUE) ~ 1,
                                  TRUE ~ 0),
             Townhouse = case_when(grepl("Townhouse", BuildType, fixed=TRUE) ~ 1,
                                   TRUE ~ 0),
             Condotel = case_when(grepl("Condotel", BuildType, fixed = TRUE) ~ 1,
                                  TRUE ~ 0),
             SingleFam = case_when(grepl("Single Family", BuildType, fixed=TRUE) ~ 1,
                                   TRUE ~ 0),
             Duplex = case_when(grepl("Duplex", BuildType, fixed=TRUE) ~ 1,
                                TRUE ~ 0),
             MultiDwell = case_when(grepl("Multiple Dwellings", BuildType, fixed = TRUE) ~ 1,
                                    TRUE ~ 0),
             WalkUP = case_when(grepl("Walk-Up", BuildType, fixed=TRUE) ~ 1,
                                TRUE ~ 0)) %>%
      select(-BuildType, -ArchitecturalStyle) %>%
      mutate(LUC = case_when(grepl("Residential", Zoning, fixed = TRUE) ~ "Residential",
                             grepl("Indust", Zoning, fixed = TRUE) ~ "Industrial",
                             grepl("Busin", Zoning, fixed = TRUE) ~ "Business",
                             grepl("Apart", Zoning, fixed = TRUE) &  grepl("Low", Zoning, fixed = TRUE)~ "Apartment-LD",
                             grepl("Apt", Zoning, fixed = TRUE) &  grepl("Low", Zoning, fixed = TRUE)~ "Apartment-LD",
                             grepl("Apart", Zoning, fixed = TRUE) &  grepl("Medium", Zoning, fixed = TRUE)~ "Apartment-MD",
                             grepl("Apt", Zoning, fixed = TRUE) &  grepl("Medium", Zoning, fixed = TRUE)~ "Apartment-MD", 
                             grepl("Apart", Zoning, fixed = TRUE) &  grepl("High", Zoning, fixed = TRUE)~ "Apartment-HD",
                             grepl("Apt", Zoning, fixed = TRUE) &  grepl("High", Zoning, fixed = TRUE)~ "Apartment-HD",
                             grepl("Resort", Zoning, fixed = TRUE) ~ "Resort",
                             grepl("Agricul", Zoning, fixed = TRUE) ~ "Agriculture",
                             grepl("Presev", Zoning, fixed = TRUE) ~ "Preserve",
                             TRUE ~ "Other")) %>%
      select(-Zoning) %>%
      mutate(PropertyCo = gsub("Above Average", "Blah", PropertyCondition),
             cond = case_when(grepl("Tear Down", PropertyCo, fixed = TRUE) ~ "Tear Down",
                              TRUE ~ PropertyCo),
             cond = case_when(grepl("Tear Down", PropertyCo, fixed = TRUE) ~ "Tear Down",
                              grepl("Major", PropertyCo, fixed = TRUE) ~ "Major Repair",
                              TRUE ~ PropertyCo),
             cond = case_when(grepl("Tear Down", PropertyCo, fixed = TRUE) ~ "Tear Down",
                              grepl("Major", PropertyCo, fixed = TRUE) ~ "Major Repair",
                              grepl("Fair", PropertyCo, fixed = TRUE) ~ "Fair",
                              TRUE ~ PropertyCo),
             cond = case_when(grepl("Tear Down", PropertyCo, fixed = TRUE) ~ "Tear Down",
                              grepl("Major", PropertyCo, fixed = TRUE) ~ "Major Repair",
                              grepl("Fair", PropertyCo, fixed = TRUE) ~ "Fair",
                              grepl("Average", PropertyCo, fixed = TRUE) ~ "Average",
                              TRUE ~ PropertyCo),
             cond = case_when(grepl("Tear Down", PropertyCo, fixed = TRUE) ~ "Tear Down",
                              grepl("Major", PropertyCo, fixed = TRUE) ~ "Major Repair",
                              grepl("Fair", PropertyCo, fixed = TRUE) ~ "Fair",
                              grepl("Average", PropertyCo, fixed = TRUE) ~ "Average",
                              grepl("Blah", PropertyCo, fixed = TRUE) ~ "Above Average",
                              TRUE ~ PropertyCo)) %>%
      select(-PropertyCo, -PropertyCondition) %>%
      mutate(pool = case_when(is.na(PoolFeatures) ~ 0,
                              TRUE ~ 1)) %>%
      select(-PoolFeatures, -Amenities, -Flooring, -ConstructionMaterials) %>%
      filter(YearBuilt > 20) %>%
      mutate(YearBuilt = case_when(YearBuilt == 984 ~ 1984,
                                   YearBuilt == 1070 ~ 1970,
                                   YearBuilt == 197 ~ 1970,
                                   TRUE ~ YearBuilt),
             YearRemodeled = case_when(YearRemodeled == 1016 ~ 1916,
                                    TRUE ~ YearRemodeled)) %>%
      filter(YearBuilt < YearRemodeled| YearRemodeled == 0) %>%
      mutate(BuildingNa = str_to_title(BuildingName),
             BuildingNa = case_when(BuildingNa == 'Eden At Haiku Woods, A*' ~ 'Eden At Haiku Woods A',
                                    grepl("Hawaii Kai Cp", BuildingNa, fixed=TRUE) ~ "Hawaii Kai Condo",
                                    grepl("Hawaii Kai Condo", BuildingNa, fixed=TRUE) ~ "Hawaii Kai Condo",
                                    BuildingNa == 'Ke Aina Kai Townhome*' ~ 'Ke Aina Kai Townhomes*',
                                    BuildingNa == 'Ke Noho Kai*' ~ "Ke Noho Kai Townhomes*",
                                    BuildingNa == 'Lalea At Hawaii Kai Iii' ~ 'Lalea At Hawaii Kai 3',
                                    BuildingNa == 'Montecito/Tuscany*' ~ "Montecito/Tuscany",
                                    grepl("@Mililani", BuildingNa, fixed=TRUE) ~ "At Mililani",
                                    BuildingNa == 'Northpointe Ii' ~ 'Northpointe 2',
                                    grepl('Parkview Village', BuildingNa, fixed=TRUE) ~ "Parkview Village",
                                    BuildingNa == "Parkview Village*" ~ "Parkview Village",
                                    BuildingNa == "Shores At Suncrest I" ~ "Shores At Suncrest 1",
                                    BuildingNa == "Spruce Ridge Villas*" ~ "Spruce Ridge Villas",
                                    BuildingNa == "Terrazza" ~ "Terraza",
                                    BuildingNa == "Town Homes @ Frwys Edge*" ~ "Town Homes At Frwys Edge",
                                    BuildingNa == "Vineyard*" ~ "Vineyard Apts",
                                    BuildingNa == "Waikiki Marina Condo*" ~ "Waikiki Marina Condominium",
                                    BuildingNa == "Waikiki Parkway*" ~ "Waikiki Parkway Apts",
                                    TRUE ~ BuildingNa)) %>%
      filter(SQFTGarageCarport < 5001) %>%
      mutate(Age = year - YearBuilt,
             Age2 = Age * Age,
             Age.r = year - YearRemodeled)
    
    b<-data %>%
      count(ParcelNumber)  %>%
      mutate(num_sale = n) %>%
      select(-n) 
    data <- b %>%
      right_join(., data)
    
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
      full_join(., map.data, by="TMK")  %>%
      filter(!is.na(Sale))
    
    cen.map <- census %>%
      select(GEOID, geometry) %>%
      distinct() %>%
      st_transform(., crs=st_crs(map.sale))  %>%
      st_intersection(., map.sale)  %>%
      select(TMK, GEOID, Sale)

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
      left_join(., cen.map, by = "TMK") %>%
      left_join(., cen.data, by = "GEOID") %>%
      filter(!is.na(population)) %>%
      distinct()

#Setup House Data Fields
    

    
    land.data <- data %>%
      select(TMK, TMK.condo, Zoning, View, Topography, RoadFronta, PropertyFr, FloodZone, ParkingFea,
             ParkingTot, Parking, PostalCode)
    

    
    school.data <- data %>%
      select(TMK, TMK.condo, Elementary, HighSchool, MiddleOrJu)
    
    user.data <- data %>%
      select(TMK, TMK.condo, rail, '1mile', lnrail, golf, lngolf, preschool, lnpreschool, privateschool,
             lnprivate, publicschool, lnpublic, hospital, lnhospital, park, lnpark, lnocean,
             airport, lnairport, wetland, lnwetland)
    
    assoc.data <- data %>%
      select(TMK, TMK.condo, Associatio, Associat_1, Associat_2, Associat_3)
    