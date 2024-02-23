#Creates a Nearest Neighbor data set using only those units sold PRIOR to current unit
#Jeremy R. Groves
#January 31, 2024


rm(list=ls())

library(tidyverse)
library(dbscan)

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

core <- main %>%
  select(ParcelNumber, lat, lon, CloseDate, year) %>%
  arrange(CloseDate) %>%
  mutate(ID = 1:n())

temp<-core %>%
  filter(year==2016)

seed <- nrow(temp)+1 #starts data with first sale of 2017
  rm(temp)

dist <- lapply(1:seed-1, function(i) as.integer(i+1))  
ID <- lapply(1:seed-1, function(i) as.integer(i+1))

for(i in seq(1,nrow(core)-seed)){ #seq(1,nrow(core)-seed)){
  #Limit Data to only those at or before current
  test <- core %>%
    filter(ID <= seed)
  
  #apply the nearest neighbor function
  temp <- kNN(test[,2:3], k=50)
  
  #Create Distance Matrix  
  temp2 <- list((temp$dist[seed,]))
    names(temp2) <- seed
  dist <- append(dist, temp2)
  
  #Create ID matrix
  temp2 <- list((temp$id[seed,]))
    names(temp2) <- seed
  ID <- append(ID, temp2)

  seed<-seed+1
  print(i)
}

save(dist, ID, file="./Build/Output/neighbors.RData")

#Create for the Single Family Housing Units only

load("./Build/Output/CoreData.RData")

  rm(main.s, main.s2)

core <- main %>%
  filter(PropertyType == "Single Family") %>%
  select(ParcelNumber, lat, lon, CloseDate, year) %>%
  arrange(CloseDate) %>%
  mutate(ID = 1:n())


  temp<-core %>%
    filter(year==2016)
  
seed <- nrow(temp)+1 #starts data with first sale of 2017
  rm(temp)

dist <- lapply(1:seed-1, function(i) as.integer(i+1))  
ID <- lapply(1:seed-1, function(i) as.integer(i+1))

for(i in seq(1,nrow(core)-seed)){
  #Limit Data to only those at or before current
  test <- core %>%
    filter(ID <= seed)
  
  #apply the nearest neighbor function
  temp <- kNN(test[,2:3], k=50)
  
  #Create Distance Matrix  
  temp2 <- list((temp$dist[seed,]))
  names(temp2) <- seed
  dist <- append(dist, temp2)
  
  #Create ID matrix
  temp2 <- list((temp$id[seed,]))
  names(temp2) <- seed
  ID <- append(ID, temp2)
  
  
  seed<-seed+1
  print(i)
}

save(dist, ID, file="./Build/Output/neighbors_sf.RData")
