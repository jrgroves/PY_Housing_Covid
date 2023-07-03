rm(list=ls())

library(spatialreg)
library(tidyverse)
library(spdep)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")
map<-st_read(dsn="./Build/Input/Maps/oahtmk.shp")

#Create New Variables for Analysis
main <- core.2 %>%
  mutate(Covid2 = case_when(CloseDate <= "2020-03-01" ~ 0,
                            CloseDate >  "2022-06-30" ~ 0,
                            TRUE ~ 1),
         Age2 = Age2 / 1000) %>%
  filter(!is.na(fld_zone))  #Removes two observations with no flood zone

#Create Spatial Weights for Spatial Regressions

main.2<-main %>%
  select(TMK, lat, lon) %>%
  distinct() %>%
  st_as_sf(., coords=c("lon", "lat"), crs=st_crs(map))

W<-knn2nb(knearneigh(main.2, k=5))
W2 <- nb2listw(W)


mod.1<-lagsarlm(ln.r.close~Covid,data=main,W2,
                method="eigen")
head(W)