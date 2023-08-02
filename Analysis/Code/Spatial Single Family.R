rm(list=ls())

library(spatialreg)
library(tidyverse)
library(spdep)
library(spgwr)
library(stargazer)

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
  work<-main.s %>%
    filter(PropertyType == "Single Family")
  
  coords <- cbind(work$lon, work$lat)
  
  #Critical Distance Weight Matrix
  #  k1 <- knn2nb(knearneigh(coords))
  #  
  #    critical.threshold <- max(unlist(nbdists(k1,coords)))
  #    critical.threshold
  #  
  #  nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
  #  
  #    distances <- nbdists(nb.dist.band,coords)
  #    invd1 <- lapply(distances, function(x) ((1/x)*100))
  #
  #  invd.weights <- nb2listw(nb.dist.band, glist = invd1, style = "W", zero.policy = TRUE)
  
  #Creates Nearest Neighbor Weight Matrix
      knn <- knn2nb(knearneigh(coords, k=20))
      invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
  
      
  #ev<-eigenw(invd.weights) 
  W<-as(invd.weights,"CsparseMatrix") 
  trMatc<-trW(W,type="MC")
    
#Spatial AR Regressions
  
  mod1 <- lagsarlm(lnClose ~ Covid,
                   data = work, 
                   listw = invd.weights,
                   quiet = FALSE,
                   method = "LU",
                   Durbin = TRUE,
                   control = list(small=25),
                   trs=trMatc)
  
  
  mod2 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year),
                   data = work, 
                   listw = invd.weights,
                   quiet = FALSE,
                   method = "LU",
                   Durbin = TRUE,
                   control = list(small=25),
                   trs=trMatc)
 
  mod3 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                   data = work, 
                   listw = invd.weights,
                   quiet = FALSE,
                   method = "LU",
                   Durbin = TRUE,
                   control = list(small=25),
                   trs=trMatc)
  
  
  mod4 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD +  Duplex +  SingleFam +
                      factor(LUC) + Parking + HOA + remod + Elevator, 
                   data = work, 
                   listw = invd.weights,
                   quiet = FALSE,
                   method = "LU",
                   Durbin = TRUE,
                   control = list(small=25),
                   trs=trMatc)
  
  
  mod5 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + Duplex + SingleFam +
                      factor(LUC) + Parking + HOA + remod + Elevator +
                      beach + park + hospital + airport  + elem_sch + 
                      mid_sch + high_sch, 
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  
  
  mod6 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                      Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                      Split + PUD + Duplex + SingleFam +
                      factor(LUC) + Parking + HOA + remod + Elevator +
                      beach + park + hospital + airport  + elem_sch + 
                      mid_sch + high_sch +  
                      per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  

  save(mod1, mod2, mod3, mod4, mod5, mod6, trMatc, file="./Analysis/Output/Spat1sf.RData")
  stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type="text", out="./Analysis/OUtput/spat1sf.txt")
  
 
#Create Weights for Distance Based W (First Sale)
  work<-main.s2 %>%
    filter(PropertyType == "Single Family")
  
  coords <- cbind(work$lon, work$lat)
  
  #Critical Distance Weight Matrix
  #  k1 <- knn2nb(knearneigh(coords))
  #  
  #    critical.threshold <- max(unlist(nbdists(k1,coords)))
  #    critical.threshold
  #  
  #  nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
  #  
  #    distances <- nbdists(nb.dist.band,coords)
  #    invd1 <- lapply(distances, function(x) ((1/x)*100))
  #
  #  invd.weights <- nb2listw(nb.dist.band, glist = invd1, style = "W", zero.policy = TRUE)
  
  #Creates Nearest Neighbor Weight Matrix
  knn <- knn2nb(knearneigh(coords, k=20))
  invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
  
  
  #ev<-eigenw(invd.weights) 
  W<-as(invd.weights,"CsparseMatrix") 
  trMatc<-trW(W,type="MC")
  
  
  #Spatial AR Regressions
  
  mod1 <- lagsarlm(lnClose ~ Covid,
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  
  
  mod2 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year),
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  
  
  mod3 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond), 
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  
  
  mod4 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + Duplex + SingleFam +
                     factor(LUC) + Parking + HOA + remod + Elevator, 
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  
  mod5 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + Duplex + SingleFam +
                     factor(LUC) + Parking + HOA + remod + Elevator +
                     beach + park + hospital + airport  + elem_sch + 
                     mid_sch + high_sch, 
                 data = work, 
                 listw = invd.weights,
                 quiet = FALSE,
                 method = "LU",
                 Durbin = TRUE,
                 control = list(small=25),
                 trs=trMatc)
  
  
  mod6 <- lagsarlm(ln.r.close ~ Covid + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                     Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                     Split + PUD + Duplex + SingleFam +
                     factor(LUC) + Parking + HOA + remod + Elevator +
                     beach + park + hospital + airport  + elem_sch + 
                     mid_sch + high_sch +  
                     per_black + per_asian + per_hawaian + per_owner + per_occupied, 
                    data = work, 
                    listw = invd.weights,
                    quiet = FALSE,
                    method = "LU",
                    Durbin = TRUE,
                    trs=trMatc)
  
  
  save(mod1, mod2, mod3, mod4, mod5, mod6, trMatc, file="./Analysis/Output/Spat2sf.RData")

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type="text", out="./Analysis/OUtput/spat2sf.txt")
