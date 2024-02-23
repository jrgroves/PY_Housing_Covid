#Analizes the model based on Pace, et. al. (98) for Single Family Housing
  #Jeremy R. Groves
  #January 31, 2024
  #Revised February 23, 2024


rm(list=ls())

library(tidyverse)
library(Matrix)

#Read in Cleaned Data, weight matrix, and filter for single family only

  load("./Build/Output/CoreData.RData")
  load("./Build/Output/Space.RData")
  load("./Build/Output/Time.RData")
  
  rm(main.s, main.s2)
  
  core <- main %>%
    filter(PropertyType == "Single Family") %>%
    arrange(CloseDate) %>%
    mutate(ID = 1:n()) 
  rm(main)
  
#Create Main Spatial Weight Matrix
  
  neighbors <- 10
  w <- 1/neighbors
  
  S <- s1+s2+s3+s4+s5+s6+s7+s8+s9+s10
  S <- w * S  
  rm(list = grep("^s", ls(), value = TRUE, invert = FALSE))
  
  
#Create Main Temporal Weight Matrix
 
  temp <- list()
  for(i in seq(1,59)){
    temp<-append(temp,  eval(parse(text=paste0("D", i))))
  }
 
  TT <- Reduce('+', temp)
  rm(temp)
  rm(list = grep("^D", ls(), value = TRUE, invert = FALSE))

  #
  
#Create Variables with spatial-temporal lags
  
  eXes <- core %>%
    select(BedsTotal, BathsTotal, Covid)
  
  X <- data.matrix(eXes, rownames.force = TRUE)
  Y <- data.matrix(core$lnClose, rownames.force = FALSE)
    colnames(Y) <- "lnClose"
  
  TX <- TT %*% X
  SX <- S %*% X
  STX <- S %*% TT %*% X
  TSX <- TT %*% S %*% X
  
  TY <- TT %*% Y
  SY <- S %*% Y
  STY <- S %*% TT %*% Y
  TSY <- TT %*% S %*% Y

  sX <- as.data.frame(as.matrix(SX))
    names(sX) <- paste("s", names(sX), sep="-")
  tX <- as.data.frame(as.matrix(TX))
    names(tX) <- paste("t", names(tX), sep="-")
  stX <- as.data.frame(as.matrix(STX))
    names(stX) <- paste("st", names(stX), sep="-")
  tsX <- as.data.frame(as.matrix(TSX))
    names(tsX) <- paste("ts", names(tsX), sep="-")
    
  sY <- as.data.frame(as.matrix(SY))
    names(sY) <- paste("s", names(sY), sep="-")
  tY <- as.data.frame(as.matrix(TY))
    names(tY) <- paste("t", names(tY), sep="-")
  stY <- as.data.frame(as.matrix(STY))
    names(stY) <- paste("st", names(stY), sep="-")
  tsY <- as.data.frame(as.matrix(TSY))
    names(tsY) <- paste("ts", names(tsY), sep="-")
    
  reg <- cbind(Y, eXes, sX, tX, stX, tsX, sY, tY, stY, tsY, core$Stories, core$year)
  
  reg <- reg %>%
    rename("year" = "core$year",
           "stories" = "core$Stories") %>%
    filter(!row_number() %in% seq(1,seed))
  
  mod1 <- lm(lnClose ~ . + factor(stories) -stories + factor(year) - year, data = reg)
  