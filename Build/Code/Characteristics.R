#Import and Process Housing Characteristic Data
#Jeremy R. Groves
#May 2, 2023

rm(list=ls())

library(tidyverse)
library(readxl)

#Read in Raw Sales Data from PY and Others
raw.data<-read_excel("./Build/Input/Housing sales data.xlsx")

data <- raw.data %>%
  mutate(TMK = substr(gsub("-","",ParcelNumb),2,9),
         TMK.condo = substr(gsub("-","",ParcelNumb),10,13),
         TMK = paste0("1",TMK)) %>%
  filter(Bathrooms != 0)


char.data <- data %>%
  select(TMK, TMK.condo, BuildingNa, Constructi, BathsFull, BathsHalf, Bedrooms, SqftTotal, 
         YearBuilt, YearRemode, StoriesTyp, Zoning, PropertyTy, Flooring, Roof, SQFTRoofed,
         ElevatorsN, PropertyCo, SQFTGarage, PoolFeatur, Amenities, Architectu, year) %>%
  mutate(ID = paste0(TMK, TMK.condo, ".", year)) %>%
  distinct() %>%
  filter(!is.na(StoriesTyp),
         !is.na(SqftTotal),
         !is.na(YearBuilt)) %>%
  rename("Stories" = "StoriesTyp") %>%
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
  mutate(BuildType = Architectu,
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
  select(-BuildType, -Architectu) %>%
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
  mutate(PropertyCo = gsub("Above Average", "Blah", PropertyCo),
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
  select(-PropertyCo) %>%
  mutate(pool = case_when(is.na(PoolFeatur) ~ 0,
                          TRUE ~ 1)) %>%
  select(-PoolFeatur, -Amenities, -Flooring, -Constructi) %>%
  filter(YearBuilt > 20) %>%
  mutate(YearBuilt = case_when(YearBuilt == 1070 ~ 1970,
                               YearBuilt == 197 ~ 1970,
                               TRUE ~ YearBuilt),
         YearRemode = case_when(YearRemode == 1016 ~ 1916,
                                TRUE ~ YearRemode)) %>%
  filter(YearBuilt < YearRemode | YearRemode == 0) %>%
  mutate(BuildingNa = str_to_title(BuildingNa),
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
  filter(SQFTGarage < 5001)

save(char.data, file="./Build/Output/Char.RData")

