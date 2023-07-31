#Import Housing Data and clean sales data
#Jeremy R. Groves
#Feb 27, 2023

rm(list=ls())

library(tidyverse)
library(openxlsx)

#Read in Raw Sales Data from PY and Others

    raw.data<-read.xlsx("./Build/Input/Sales Data 1.xlsx", detectDates=TRUE)
    
    raw.data$CloseDate<-as.POSIXlt(raw.data$CloseDate, format="%m_%d_%Y",tz="UTC")
    raw.data$ListingContractDate<-as.POSIXlt(raw.data$ListingContractDate,format="%m_%d_%Y",tz="UTC")
    
    raw.data2<-read.xlsx("./Build/Input/Sales Data 2.xlsx", detectDates=TRUE)
    
    raw.data2$CloseDate<-as.POSIXlt(raw.data2$CloseDate, format="%Y-%m-%d",tz="UTC")
    raw.data2$ListingContractDate<-as.POSIXlt(raw.data2$ListingContractDate,format="%Y-%m-%d",tz="UTC")
    
    cpi<-read.xlsx("./Build/Input/cpi.xlsx")
      cpi <- cpi %>%
        select(Series2, CPI)
      
     #List of ParcelNumbers where Condo / Single Family is changed between sales fixed using property lookup 
     propid1<-c('1-2-2-004-003-0009','1-3-1-036-004-0004','1-3-5-024-009-0077','1-3-9-008-036-0039',
               '1-4-2-101-016-0009','1-8-7-001-012-0005',
               '1-8-7-010-022-0028','1-8-7-010-022-0090','1-8-7-010-022-0091','1-9-1-010-069-0005',
               '1-9-1-010-078-0009','1-9-1-010-116-0005','1-9-1-012-040-0020','1-9-1-102-025-0005',
               '1-9-1-102-034-0007','1-9-1-132-001-0015','1-9-1-132-010-0040','1-9-1-132-010-0090',
               '1-9-1-149-034-0002','1-9-1-160-051-0025','1-9-1-164-028-0015','1-9-2-019-044-0043',
               '1-9-2-019-060-0001','1-9-2-019-060-0016','1-9-2-019-071-0003','1-9-2-019-071-0010',
               '1-9-4-007-012-0152','1-9-4-035-083-0002','1-9-5-002-004-0006',
               '1-9-7-049-019-0001')
     propid2 <-c('1-9-5-029-005-0000','1-9-4-070-021-0000','1-4-6-033-037-0000',
                 '1-4-1-032-114-0000','1-8-7-036-034-0000')
               
#Prepare Data
           
    data <- raw.data %>%
      bind_rows(raw.data2) %>%
      mutate(TMK = substr(gsub("-","",ParcelNumber),2,9),
             TMK.condo = substr(gsub("-","",ParcelNumber),10,13),
             TMK = paste0("1",TMK)) %>%
      filter(BathsTotal != 0)

   
#Price Data####
    
    data <- data %>%
      mutate(dom = as.numeric((CloseDate - ListingContractDate))/86400,
             year = year(CloseDate),
             Clo.mon = month(as.POSIXlt(CloseDate, format="%Y/%m/%d")),
             Series = case_when(Clo.mon < 7 ~ "HALF1",
                                Clo.mon > 6 ~ "HALF2",
                                TRUE ~ "0"),
             Clo.mon = str_pad(as.character(Clo.mon), 2, side="left",pad="0"),
             Series = paste0(Series, year),
             Series2 = paste(year,Clo.mon,sep=".")) %>%
      filter(dom >= 0) %>%
      select(-Clo.mon) %>%
      left_join(., cpi, by="Series2") %>%
      mutate(real.list.price = (ListPrice*303.294)/CPI,
             real.close.price = (ClosePrice*303.294)/CPI,
             lnList = log(ListPrice),
             lnClose = log(ClosePrice),
             ln.r.list = log(real.list.price),
             ln.r.close = log(real.close.price),
             diff.price = ListPrice - ClosePrice,
             r.diff.price = real.list.price - real.close.price) %>%
      select(-c(Series, CPI, Series2))
    
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
             Stories = case_when(Three == 1 & Number == 0 ~ "Multi",
                                 Two == 1 & Three == 0 & Number == 0 ~ "Two",
                                 One == 1 & Two ==0 & Three == 0 & Number == 0 ~ "One",
                                 Number == 1 ~ "Multi",
                                 TRUE ~ "Multi")) %>%
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
             Age2 = Age^2,
             Age.r = year - YearRemodeled,
             Age.r = case_when(YearRemodeled == 0 ~ 0,
                               TRUE ~ Age.r),
             Parking = case_when(ParkingTotal > 4 ~ "5 or More",
                                 TRUE ~ as.character(ParkingTotal))) %>%
      mutate(livSQFT = SQFTRoofedLiving) %>%
      filter(livSQFT > 99,
             Age >= 0) %>%
      select(-SQFTRoofedLiving) %>%
      mutate(PropertyType = gsub("/", "_", PropertyType),
             PropertyType = case_when(ParcelNumber %in% propid1 ~ "Condo_Townhouse",
                                      ParcelNumber %in% propid2 ~ "Single Family",
                                      TRUE ~ PropertyType))
    
    #Add Sales Frequency
    
    b<-data %>%
      count(ParcelNumber)  %>%
      mutate(num_sale = n) %>%
      select(-n) 
    
    data <- b %>%
      right_join(., data)
    
core <- data %>%
  filter(#DOM <= 365,  #Removes about 143 observations with DOM greater than one year.
         real.list.price >= 50000, #Removes single low listing price which is likely typo
         LUC != "Industrial", #Removes 94 observations
         cond != "Tear Down", #Removes 310 observations
         cond != "Major Repair") #Removes 816 units
         


save(core, file="./Build/Output/core.RData")

    