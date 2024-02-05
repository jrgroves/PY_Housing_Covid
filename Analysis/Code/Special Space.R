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

seed <- 6882 #starts data with first sale of 2017
dist <- data.frame()
ID <- data.frame()

for(i in seq(1,43513)){
  #Limit Data to only those at or before current
    test <- core %>%
      filter(ID <= seed)
  
  #apply the nearest neighbor function
    temp <- kNN(test[,2:3], k=50)
    
  #Create Distance Matrix  
    temp2 <- as.data.frame(t(temp$dist[seed,]))
      temp2 <- cbind(temp2, seed)
    dist <- rbind(dist, temp2)
    
  #Create ID matrix
    temp2 <- as.data.frame(t(temp$id[seed,]))
      temp2 <- cbind(temp2, seed)
    ID <- rbind(ID, temp2)
    seed<-seed+1
    print(i)
}

save(dist, ID, file="./Build/Output/neighbors.RData")