#Creates a list of neighbor IDs and Distances using the dbscan library and then creates the
#spatial weight matrices (S) as outlined in Pace, et. al. (98). User should input the seed, 
#or number of starting observations to skip, and the number of nearest neighbors. File saves
#ID and dist lists of neighbors and an image of all Si's created. First part should only be run
#once.

#Jeremy R. Groves
#February 23, 2024

rm(list=ls())

library(tidyverse)
library(dbscan)

#Load main data and apply any limitations

  load("./Build/Output/CoreData.RData")
    rm(main.s, main.s2)
    
  core <- main %>%
    filter(PropertyType == "Single Family") %>%
    arrange(CloseDate) %>%
    mutate(ID = 1:n()) 
  
  #Subset out the coordinates
    coords <- cbind(core$lon, core$lat)
  #Set the seed which are the observations that will be removed because they have no previous sales and determine the number of neighbors
    seed <- ceiling(nrow(core) * .01)
    nn <- 50
  #Pre-allocate the dist and ID lists  
    dist <- lapply(1:seed-1, function(i) as.integer(i+1))  
    ID <- lapply(1:seed-1, function(i) as.integer(i+1))
    ID0 <- lapply(1:seed-1, function(i) as.integer(i+1))    
    ID2 <- lapply(1:seed-1, function(i) as.integer(i+1))    
    

  #Loop to pull the IDs and Distances for only previous observations
    
    for(i in seq(1,nrow(core)-seed)){
      #Limit Data to only those at or before current
      test <- core %>%
        filter(ID <= seed)
      
      #apply the nearest neighbor function
      temp <- kNN(test[,c("lon", "lat")], k=nn)
      
      #Create Distance Matrix  
      temp.dist <- list((temp$dist[seed,]))
      names(temp.dist) <- seed
      dist <- append(dist, temp.dist)
      
      #Create ID matrix
      temp.id <- list((temp$id[seed,]))
      names(temp.id) <- seed
      ID <- append(ID, temp.id)
      
      #Pull Out only dist = 0 neighbors
      temp.0 <- sapply(temp.dist, function(x) x==0)
      tt <- list(temp.id[[1]][temp.0])
      ID0 <- append(ID0, tt)
      
      tt2 <- list(temp.id[[1]][temp.dist[[1]] != 0])
      ID2 <- append(ID2, tt2)
      
      seed<-seed+1
      print(i)
    }
    
    save(ID, ID0, ID2, dist, file="./Build/Output/SingFam_NN.RData")
  
#Create Sparse Spatial Matrix based on however nearest neighbors were chosen previously
  
 #load("./Build/Pacer.RData") #load data if needed to be run seperately
    for(j in seq(1, nn)){
      m <- Matrix(nrow = 22965, ncol = 22965, data = 0, sparse = TRUE)
            for(i in seq(seed+1,length(ID))){
              m[i, as.numeric(unlist(ID[[i]][j]))] <- 1
            }
      assign(paste0("s", j), m)
    }
    
    rm(list = grep("^s", ls(), value = TRUE, invert = TRUE))
    
    save.image("./Build/Output/Space.RData")
