#Analysis of the model based on Pace, et. al. (98) for Single Family Housing
  #Jeremy R. Groves
  #January 31, 2024
  #Revised February 23, 2024


rm(list=ls())

library(tidyverse)
library(Matrix)
library(matrixcalc)
library(stargazer)

#Read in Cleaned Data, weight matrix, and filter for single family only

load("./Build/Output/Space0_Condo.RData")

#Create Main Spatial Weight Matrix

  temp <- list()
  for(i in seq(1,50)){
    temp<-append(temp,  eval(parse(text=paste0("s0", i))))
  }
  S0 <- Reduce('+', temp) 
  
  rm(list = grep("^s0", ls(), value = TRUE, invert = FALSE))
  rs<-rowSums(S0)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  
  S0 <- S0 * rs #this row standardizes the matrix

  load("./Build/Output/Space_Condo.RData")
  S <- s1+s2+s3+s4+s5+s6+s7+s8+s9+s10

  rm(list = grep("^s", ls(), value = TRUE, invert = FALSE))
  rs<-rowSums(S)
  
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  
  S <- S * rs #this row standardizes the matrix  
  
#Create Main Temporal Weight Matrix
  load("./Build/Output/Time_CT.RData")
  
  temp <- list()
  for(i in seq(1,59)){
    temp<-append(temp,  eval(parse(text=paste0("D", i))))
  }
 
  TT <- Reduce('+', temp)
  rm(temp)
  rm(list = grep("^D", ls(), value = TRUE, invert = FALSE))
  
  rs<-rowSums(TT)
  
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  
  TTs <- TT * rs #this row standardizes the matrix  
  
#Create Variables with spatial-temporal lags
  load("./Build/Output/CoreData.RData")
  rm(main.s, main.s2)
  
  core <- main %>%
    filter(PropertyType ==  "Condo_Townhouse") %>%
    arrange(CloseDate) %>%
    mutate(ID = 1:n()) 
  rm(main)
  
  #Set the seed which are the observations that will be removed because they have no previous sales and determine the number of neighbors
  seed <- ceiling(nrow(core) * .01)
  nn <- 50
  
  eXes <- core %>%
    mutate(Excel_Cond = case_when(cond == "Excellent" ~ 1,
                                  TRUE ~ 0),
           AbAvg_Cond = case_when(cond == "Above Average" ~ 1,
                                  TRUE ~ 0),
           Fair_Cond = case_when(cond == "Fair" ~ 1,
                                 TRUE ~ 0),
           Avg_Cond = case_when(cond == "Average" ~ 1,
                                TRUE ~ 0)) %>%
    select(BedsTotal, BathsTotal, Covid, Age, Age2, livSQFT, par_area, beach, park, hospital,
           airport, elem_sch, mid_sch, high_sch, per_white, per_black, per_asian, per_hawaian,
           per_occupied, per_owner, Split, LowRise, HighRise, PUD, Condotel, SingleFam, Townhouse, 
           WalkUP, Excel_Cond, AbAvg_Cond, Fair_Cond)
  
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
  
  mod1.st <- lm(lnClose ~ . + factor(stories) - stories + factor(year) - year, data = reg)
  
  ST <- S * TT
  
  ST_X <- ST %*% X
 
  st_X <- as.data.frame(as.matrix(ST_X))
    names(st_X) <- paste("stx", names(st_X), sep="-")
    
  reg <- cbind(Y, eXes, st_X, core$Stories, core$year)
  
  reg <- reg %>%
    rename("year" = "core$year",
           "stories" = "core$Stories") %>%
    filter(!row_number() %in% seq(1,seed))
  
  mod2.st <- lm(lnClose ~ . + factor(stories) - stories + factor(year) - year, data = reg)
 
  #Now for the in building neighbors only 
  
  S <- S0
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
  
  mod1.s0t <- lm(lnClose ~ . + factor(stories) - stories + factor(year) - year, data = reg)
 
  ST <- S * TT
  ST_X <- ST %*% X
  st_X <- as.data.frame(as.matrix(ST_X))
  names(st_X) <- paste("stx", names(st_X), sep="-")
 
  reg <- cbind(Y, eXes, st_X, core$Stories, core$year)
  
  reg <- reg %>%
    rename("year" = "core$year",
           "stories" = "core$Stories") %>%
    filter(!row_number() %in% seq(1,seed))
  
  mod2.s0t <- lm(lnClose ~ . + factor(stories) - stories + factor(year) - year, data = reg)
  
  
  stargazer(mod1.st, mod2.st, mod1.s0t, mod2.s0t, type="html", out = "./Analysis/R1.html")
  