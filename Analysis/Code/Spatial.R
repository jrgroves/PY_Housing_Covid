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
work<-core.2 %>%
  distinct(TMK, .keep_all = TRUE) %>%
  arrange(CloseDate) %>%
  slice(3000:nrow(core.2))

coords <- cbind(work$lon, work$lat)
k1 <- knn2nb(knearneigh(coords))

critical.threshold <- max(unlist(nbdists(k1,coords)))
critical.threshold

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
nb<-nb.dist.band

#Conversion to only previous sales######

as.nb.sgbp <- function(x) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

for(i in seq(1,length(nb),1)){
  c<-lapply(nb[[i]], function(x) x > i)
  nb[[i]][unlist(c)] <- NA
}

c<-lapply(nb, function(x) x[!is.na(x)])
############

dd<-as.nb.sgbp(c)

distances <- nbdists(nb.dist.band,coords)
d.c <- nbdists(dd,coords)

invd1 <- lapply(distances, function(x) (1/x))
invd1a <- lapply(d.c, function(x) (1/x))

length(invd1)
length(invd1a)

invd.weights <- nb2listw(nb.dist.band,glist = invd1,style = "B")
invd.weights2 <- nb2listw(dd,glist = invd1a, style = "B", zero.policy = TRUE)

N<-fit.lag<-lagsarlm(lnClose ~ Covid+Age,
                  data = work, 
                  listw = invd.weights) 


