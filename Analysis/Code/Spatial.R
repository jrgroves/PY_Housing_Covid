rm(list=ls())

library(spatialreg)
library(tidyverse)
library(spdep)
library(spgwr)

#Read in Cleaned Data

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")
  rm(main)
  
#Functions
  as.nb.sgbp <- function(x) {
    attrs <- attributes(x)
    x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
    attributes(x) <- attrs
    class(x) <- "nb"
    x
  }

#Create Weieghts for Distance Based W
  work<-main.s
  
  coords <- cbind(work$lon, work$lat)
  
  k1 <- knn2nb(knearneigh(coords))
  
    critical.threshold <- max(unlist(nbdists(k1,coords)))
    critical.threshold
  
  nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
  
    distances <- nbdists(nb.dist.band,coords)
    invd1 <- lapply(distances, function(x) ((1/x)*100))

  invd.weights <- nb2listw(nb.dist.band, glist = invd1, style = "B", zero.policy = TRUE)
    
#Spatial AR Regressions
  
  mod1 <- gstsls(lnClose ~ Covid,
                   data = work, 
                   listw = invd.weights,
                 verbose = TRUE)
 
  mod2 <- gstsls(ln.r.close ~ Covid + DOM + factor(year),
                   data = work, 
                   listw = invd.weights,
                 verbose = TRUE)
  
  mod3 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                   data = work, 
                   listw = invd.weights,
                 verbose = TRUE)
  
  mod4 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                      factor(LUC) + Parking + HOA + remod + Elevator, 
                   data = work, 
                   listw = invd.weights)
  
  mod5 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                      factor(LUC) + Parking + HOA + remod + Elevator +
                      beach + park + hospital + airport  + elem_sch + 
                      mid_sch + high_sch, 
                   data = work, 
                   listw = invd.weights)
  
  mod6 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                      factor(LUC) + Parking + HOA + remod + Elevator +
                      beach + park + hospital + airport  + elem_sch + 
                      mid_sch + high_sch +  
                      per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                   data = work, 
                   listw = invd.weights)

  save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Spat1.RData")
 
#Create Weights for Distance Based W (First Sale)
  work<-main.s2
  
  coords <- cbind(work$lon, work$lat)
  
  k1 <- knn2nb(knearneigh(coords))
  
    critical.threshold <- max(unlist(nbdists(k1,coords)))
    critical.threshold
  
  nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
  
    distances <- nbdists(nb.dist.band,coords)
    invd1 <- lapply(distances, function(x) ((1/x)*100))
  
  invd.weights <- nb2listw(nb.dist.band, glist = invd1, style = "B", zero.policy = TRUE)
  
  #Spatial AR Regressions
  
  mod1 <- gstsls(lnClose ~ Covid,
                   data = work, 
                   listw = invd.weights)
  
  mod2 <- gstsls(ln.r.close ~ Covid + DOM + factor(year),
                   data = work, 
                   listw = invd.weights)
  
  mod3 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                   data = work, 
                   listw = invd.weights)
  
  mod4 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                     factor(LUC) + Parking + HOA + remod + Elevator, 
                   data = work, 
                   listw = invd.weights)
  
  mod5 <- gstsls(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                     factor(LUC) + Parking + HOA + remod + Elevator +
                     beach + park + hospital + airport  + elem_sch + 
                     mid_sch + high_sch, 
                   data = work, 
                   listw = invd.weights)
  
  mod6 <- ML_modles(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                     factor(LUC) + Parking + HOA + remod + Elevator +
                     beach + park + hospital + airport  + elem_sch + 
                     mid_sch + high_sch +  
                     per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                   data = work, 
                   listw = invd.weights,
                   Durbin)
  
  save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Spat2.RData")



