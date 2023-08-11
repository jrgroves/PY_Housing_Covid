rm(list=ls())

#Spatial Covid 2 estimates the main spatial model replacing the Covid variable with the Covid2 variable
#to test the robustness of the positive effects

#Jeremy R. Groves

library(spatialreg)
library(tidyverse)
library(spdep)
library(stargazer)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")
  rm(main)

#Functions
  
impout<-function(y,trM){
    x<-summary(impacts(y, tr=trM, R=5000), zstats=TRUE)
    
    a<-data.frame(x$res$direct)
    names(a) <- "Direct"
    a$variable<-row.names(a)
    row.names(a)<-NULL
    b<-data.frame(x$res$indirect)
    names(b) <- "Indirect"
    b$variable<-row.names(b)
    row.names(b)<-NULL
    c<-data.frame(x$res$total)
    names(c) <- "Total"
    c$variable<-row.names(c)
    row.names(c)<-NULL
    
    temp <- a %>%
      full_join(., b, by="variable") %>%
      full_join(., c, by="variable") %>%
      select(., variable, Direct, Indirect, Total)
    rm(a,b,c)
    
    a<-data.frame(x$pzmat)
    names(a)<-c("Direct P-Val", "Indirect P-Val", "Total P-Val")
    a$variable<-row.names(a)
    row.names(a)<-NULL
    
    temp <- temp %>%
      full_join(., a, by="variable") %>%
      select(variable, Direct, 'Direct P-Val', Indirect, 'Indirect P-Val', Total, 'Total P-Val')
    
    return(temp)
  }

#Regression of Full Sample One

  #Creates Nearest Neighbor Weight Matrix
  work<-main.s
    coords <- cbind(work$lon, work$lat)
  
    knn <- knn2nb(knearneigh(coords, k=20))
    invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
  
    W<-as(invd.weights,"CsparseMatrix") 
    trMatc<-trW(W,type="MC")
    
  #Model
    mod1 <- lagsarlm(ln.r.close ~ Covid2 + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                       Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                       Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
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
  #Results Output
    spat1<-impout(mod1, trMatc)
  
#Regresssion on Full Sample Two
    
    #Creates Nearest Neighbor Weight Matrix
    work<-main.s2
    coords <- cbind(work$lon, work$lat)
    
    knn <- knn2nb(knearneigh(coords, k=20))
    invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
    
    W<-as(invd.weights,"CsparseMatrix") 
    trMatc<-trW(W,type="MC")
    
    #Model
    mod2 <- lagsarlm(ln.r.close ~ Covid2 + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                       Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                       Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
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
    #Results Output
    spat2<-impout(mod2, trMatc)

#Regresssion on Full Sample One - Condo/Townhomes
    
    #Creates Nearest Neighbor Weight Matrix
    work<-main.s %>%
      filter(PropertyType == "Condo_Townhouse")
    
    coords <- cbind(work$lon, work$lat)
    
    knn <- knn2nb(knearneigh(coords, k=20))
    invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
    
    W<-as(invd.weights,"CsparseMatrix") 
    trMatc<-trW(W,type="MC")
    
    #Model
    mod3 <- lagsarlm(ln.r.close ~ Covid2 + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                       Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                       Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
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
    #Results Output
    spat1ct<-impout(mod3, trMatc)
    
#Regresssion on Full Sample Two - Condo/Townhomes
    
    #Creates Nearest Neighbor Weight Matrix
    work<-main.s2 %>%
      filter(PropertyType == "Condo_Townhouse")
    
    coords <- cbind(work$lon, work$lat)
    
    knn <- knn2nb(knearneigh(coords, k=20))
    invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
    
    W<-as(invd.weights,"CsparseMatrix") 
    trMatc<-trW(W,type="MC")
    
    #Model
    mod4 <- lagsarlm(ln.r.close ~ Covid2 + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                       Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                       Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
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
    #Results Output
    spat2ct<-impout(mod4, trMatc)
    
#Regresssion on Full Sample One - Single Family
    
    #Creates Nearest Neighbor Weight Matrix
    work<-main.s %>%
      filter(PropertyType == "Single Family")
    
    coords <- cbind(work$lon, work$lat)
    
    knn <- knn2nb(knearneigh(coords, k=20))
    invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
    
    W<-as(invd.weights,"CsparseMatrix") 
    trMatc<-trW(W,type="MC")
    
    #Model
    mod5 <- lagsarlm(ln.r.close ~ Covid2 + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                       Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                       Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
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
    #Results Output
    spat1sf<-impout(mod5, trMatc)
    
#Regresssion on Full Sample One - Single Family
    
    #Creates Nearest Neighbor Weight Matrix
    work<-main.s %>%
      filter(PropertyType == "Single Family")
    
    coords <- cbind(work$lon, work$lat)
    
    knn <- knn2nb(knearneigh(coords, k=20))
    invd.weights <- nb2listw(knn, style = "W", zero.policy = TRUE)
    
    W<-as(invd.weights,"CsparseMatrix") 
    trMatc<-trW(W,type="MC")
    
    #Model
    mod6 <- lagsarlm(ln.r.close ~ Covid2 + DOM + factor(year) + BedsTotal + BathsFull + BathsHalf + 
                       Stories + SqftTotal + Age + Age2 + Basement + factor(cond) +
                       Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP + SingleFam +
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
    #Results Output
    spat2sf<-impout(mod6, trMatc)
    
save(mod1, mod2, mod3, mod4, mod5, mod6, file="./Analysis/Output/Spat_Cov2.RData")
save(spat1, spat2, spat1ct, spat2ct, spat1sf, spat2sf, file="./Analysis/Output/Spat_Imp_Cov2.RData")