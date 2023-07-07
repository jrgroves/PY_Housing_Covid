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
  filter(!is.na(fld_zone)) #Removes two observations with no flood zone

#Create Spatial Weights for Spatial Regressions
work<-core.2 %>%
  distinct(TMK, .keep_all = TRUE) %>%
  arrange(CloseDate) 

coords <- cbind(work$lon, work$lat)
k1 <- knn2nb(knearneigh(coords, k = 7))

critical.threshold <- max(unlist(nbdists(k1,coords)))
critical.threshold

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
nb<-nb.dist.band

distances <- nbdists(nb.dist.band,coords)

invd1 <- lapply(distances, function(x) ((1/x)*100))

invd.weights <- nb2listw(k1,style = "W")

N<-fit.lag<-lagsarlm(lnClose ~ Covid+Age,
                  data = work, 
                  listw = invd.weights) 

#Conversion to only previous sales######

as.nb.sgbp <- function(x) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

nb<-k1


for(i in seq(1,length(nb),1)){
  c<-lapply(nb[[i]], function(x) x > i)
  nb[[i]][unlist(c)] <- NA
}

c<-lapply(nb, function(x) x[!is.na(x)])
############

c<-as.nb.sgbp(c)
invd.weights2 <- nb2listw(c, style = "B", zero.policy = TRUE)

N2<-fit.lag<-lagsarlm(lnClose ~ Covid+Age,
                     data = work, 
                     listw = invd.weights2,
                     zero.policy=TRUE) 
