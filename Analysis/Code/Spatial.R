rm(list=ls())

library(spatialreg)
library(tidyverse)
library(spdep)
library(spgwr)

#Read in Cleaned Data

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")
  rm(main)

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
  
  mod1 <- lagsarlm(lnClose ~ Covid,
                   data = work, 
                   listw = invd.weights)
 
  mod2 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year),
                   data = work, 
                   listw = invd.weights)
  
  mod3 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                   data = work, 
                   listw = invd.weights)
  
  mod4 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                      factor(LUC) + Parking + HOA + remod + Elevator, 
                   data = work, 
                   listw = invd.weights)
  
  mod5 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                      factor(LUC) + Parking + HOA + remod + Elevator +
                      beach + park + hospital + airport  + elem_sch + 
                      mid_sch + high_sch, 
                   data = work, 
                   listw = invd.weights)
  
  mod6 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                      factor(LUC) + Parking + HOA + remod + Elevator +
                      beach + park + hospital + airport  + elem_sch + 
                      mid_sch + high_sch +  
                      per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                   data = work, 
                   listw = invd.weights)

  save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Spat1.RData")
  

#Create Weieghts for Distance Based W (First Sale)
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
  
  mod1 <- lagsarlm(lnClose ~ Covid,
                   data = work, 
                   listw = invd.weights)
  
  mod2 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year),
                   data = work, 
                   listw = invd.weights)
  
  mod3 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                   data = work, 
                   listw = invd.weights)
  
  mod4 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                     factor(LUC) + Parking + HOA + remod + Elevator, 
                   data = work, 
                   listw = invd.weights)
  
  mod5 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                     factor(LUC) + Parking + HOA + remod + Elevator +
                     beach + park + hospital + airport  + elem_sch + 
                     mid_sch + high_sch, 
                   data = work, 
                   listw = invd.weights)
  
  mod6 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + 
                     factor(LUC) + Parking + HOA + remod + Elevator +
                     beach + park + hospital + airport  + elem_sch + 
                     mid_sch + high_sch +  
                     per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                   data = work, 
                   listw = invd.weights)
  
  save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Spat1.RData")

  
  
  
  
  
k1 <- knn2nb(knearneigh(coords, k = 7))

invd.weights <- nb2listw(k1,style = "W")













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


GWRbandwidth <- gwr.sel(ln.r.close ~ Covid + DOM + factor(year), data=main.s, 
                        coords=cbind(main.s$lon, main.s$lat),adapt=T)

gwr.model = gwr(ln.r.close ~ Covid + DOM + factor(year), data=main.s,  coords=cbind(main.s$lon, main.s$lat), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
