#Program to create the temporal weight matrix for data based on Pace, et. al. (98)

#Jeremy R. Groves
#February 23, 2024

rm(list=ls())

library(tidyverse)
library(Matrix)
library(zoo)

#Create for the Single Family Housing Units only

load("./Build/Output/CoreData.RData")
#load("./Build/Pacer.RData")

rm(main.s, main.s2)

core <- main %>%
  filter(PropertyType == "Single Family") %>%
  arrange(CloseDate) %>%
  mutate(ID = 1:n(),
         Month = as.yearmon(CloseDate, format="%Y=%m-%d"),
         Quarter = as.yearqtr(CloseDate, format = "%Y-%m-%d")) 

time <- core$CloseDate
d <- as.data.frame(table(factor(time))) #Determines the number of sales each day

#sets the sales window for the creates of the T matrix, should be one day larger than the desired time frame.

sale_win_d <- 121 #days window

#Create Sparse Temporal Matrix 
   
  #Creation of the current sales matrix
    TT <- list()

    for(i in seq(1:nrow(d))){
      temp<-Matrix(nrow = d[i,2], ncol = d[i,2], data = 1, sparse = FALSE)
      TT <- append(TT, temp)
    }
    D0 <- bdiag(TT) 
    
  #Creation of the remaining sales matrices for days
    
    for(j in seq(1, sale_win_d)){
      TT <- list()    #Generate a clear list
      Dc <- Matrix(nrow = nrow(core), ncol = nrow(core), data = 0, sparse = TRUE) #Create a sparse nxn matrix
    
      #This creates block matrix with rows equal to observations and columns equal to previous sales
      temp <- sum(d[1:j,2])
      
      a<-1
      b<-7
      
      for(i in seq((j+1),nrow(d))){
        ifelse(i==(j+1),
               x <- a + temp,
               x <- x + d[(i-1),2])
        
        ifelse(i==(j+1),
               y<-sum(d[1:i,2]),
               y<-y+d[i,2])
        
         Dc[x:y, a:b]  <- 1
         
         a <- a + d[(i-j),2]
         b <- b + d[(i-j+1),2]
      }
      assign(paste0("D",(j)), Dc)
    }
    

rm(core, d, TT, time, temp, i, j, x, y, main)
    
save.image("./Build/Output/Time_SF.RData")


#Same for Condo

rm(list=ls())

library(tidyverse)
library(Matrix)

#Create for the Single Family Housing Units only

load("./Build/Output/CoreData.RData")
#load("./Build/Pacer.RData")

rm(main.s, main.s2)

core <- main %>%
  filter(PropertyType == "Condo_Townhouse") %>%
  arrange(CloseDate) %>%
  mutate(ID = 1:n()) 

time <- core$CloseDate
d <- as.data.frame(table(factor(time))) #Determines the number of sales each day

#sets the sales window for the creates of the T matrix, should be one day larger than the desired time frame.

sale_win_d <- 121 #days window

#Create Sparse Temporal Matrix 

#Creation of the current sales matrix
TT <- list()

for(i in seq(1:nrow(d))){
  temp<-Matrix(nrow = d[i,2], ncol = d[i,2], data = 1, sparse = FALSE)
  TT <- append(TT, temp)
}
D0 <- bdiag(TT) 

#Creation of the remaining sales matrices for days

for(j in seq(1, sale_win_d)){
  TT <- list()    #Generate a clear list
  Dc <- Matrix(nrow = nrow(core), ncol = nrow(core), data = 0, sparse = TRUE) #Create a sparse nxn matrix
  
  #This creates block matrix with rows equal to observations and columns equal to previous sales
  temp <- sum(d[1:j,2])
  
  a<-1
  b<-7
  
  for(i in seq((j+1),nrow(d))){
    ifelse(i==(j+1),
           x <- a + temp,
           x <- x + d[(i-1),2])
    
    ifelse(i==(j+1),
           y<-sum(d[1:i,2]),
           y<-y+d[i,2])
    
    Dc[x:y, a:b]  <- 1
    
    a <- a + d[(i-j),2]
    b <- b + d[(i-j+1),2]
  }
  assign(paste0("D",(j)), Dc)
}


rm(core, d, TT, time, temp, i, j, x, y, main)

save.image("./Build/Output/Time_CD.RData")
