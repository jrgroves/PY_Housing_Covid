#Program to create the temporal weight matrix for data based on Pace, et. al. (98)

#Jeremy R. Groves
#February 23, 2024

rm(list=ls())

library(spdep)
library(tidyverse)
library(sf)
library(spatialreg)
library(reshape)
library(dbscan)
library(Matrix)

#Create for the Single Family Housing Units only

load("./Build/Output/CoreData.RData")
load("./Build/Pacer.RData")

rm(main.s, main.s2)

core <- main %>%
  filter(PropertyType == "Single Family") %>%
  arrange(CloseDate) %>%
  mutate(ID = 1:n()) 

time <- core$CloseDate
c <- as.data.frame(table(factor(time))) #Determines the number of sales each day
sale_win <- 61  #sets the sales window for the creates of the T matrix, should be one day larger than the desired time frame.

#Create Sparse Temporal Matrix 
    
    TT <- list()
  
  #Creation of the current sales matrix
      
    for(i in seq(1:nrow(c))){
      temp<-Matrix(nrow = c[i,2], ncol = c[i,2], data = 1, sparse = FALSE)
      TT <- append(TT, temp)
    }
    D0 <- bdiag(TT) 
    
  #Creation of the remaining sales matrices  
    
    for(j in seq(2, sale_win)){
      TT <- list()
      Dc <- Matrix(nrow = nrow(core), ncol = nrow(core), data = 0, sparse = TRUE)
    
      for(i in seq(j,nrow(c))){
          temp <- Matrix(nrow = c[i,2], ncol = c[(i-1),2], data = 1, sparse = FALSE)
          TT <- append(TT, temp)
    
      }
      D <- bdiag(TT)
      x <- dim(Dc)[1] - dim(D)[1] + 1
      y <- dim(D)[2]
      r <- dim(Dc)[1]
    
      Dc[x:r, 1:y]  <- D
      
      assign(paste0("D",(j-1)), Dc)
    }

rm(core, c, TT, time, dist, ID, temp, i, j, x, y, r, main, D)
    
save.image("./Build/Output/Time.RData")