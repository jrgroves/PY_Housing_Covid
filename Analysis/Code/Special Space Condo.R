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

#Create Main Spatial Weight Matrix

  #Using only same building units

      load("./Build/Output/Space0_Condo.RData")
      
      temp <- list()
      for(i in seq(1,50)){
        temp<-append(temp,  eval(parse(text=paste0("s0", i))))
      }
      S0 <- Reduce('+', temp) 
      
      rm(list = grep("^s0", ls(), value = TRUE, invert = FALSE))
 
  #Using only NOT same building units
      
  load("./Build/Output/Space_Condo.RData")
    
    S <- s1+s2+s3+s4+s5+s6+s7+s8+s9+s10

  rm(list = grep("^s", ls(), value = TRUE, invert = FALSE))
 
  
#Create Main Temporal Weight Matrix
  load("./Build/Output/Time_CD.RData")
  
  temp <- list()
  for(i in seq(1,120)){
    temp<-append(temp,  eval(parse(text=paste0("D", i))))
  }
 
  TT <- Reduce('+', temp)
  rm(temp)
  rm(list = grep("^D", ls(), value = TRUE, invert = FALSE))
  
#Row Standardization
  
  ST <- S * TT
  rs<-rowSums(ST)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  ST <- ST * rs #this row standardizes the matrix
  
  S0t <- S0 * TT
  rs<-rowSums(S0t)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  S0t <- S0t * rs #this row standardizes the matrix
  
  STT <- S%*%TT
  rs<-rowSums(STT)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  STT <- STT * rs #this row standardizes the matrix
  
  TTS <- TT%*%S
  rs<-rowSums(TTS)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  TTS <- TTS * rs #this row standardizes the matrix
  
  rs<-rowSums(S)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  S <- S * rs #this row standardizes the matrix
  
  S0T <- S0 * TT
  rs<-rowSums(S0T)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  S0T <- S0T * rs #this row standardizes the matrix
  
  S0TT <- S0%*%TT
  rs<-rowSums(S0TT)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  S0TT <- S0TT * rs #this row standardizes the matrix
  
  TTS0 <- TT%*%S0
  rs<-rowSums(TTS0)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  TTS0 <- TTS0 * rs #this row standardizes the matrix
  
  rs<-rowSums(S0)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  S0 <- S0 * rs #this row standardizes the matrix
  
  rs<-rowSums(TT)
  rs[which(rs>0)] <- 1/rs[which(rs>0)]
  TT <- TT * rs #this row standardizes the matrix
  
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
  STX <- STT %*% X
  TSX <- TTS %*% X
  
  TY <- TT %*% Y
  SY <- S %*% Y
  STY <- STT %*% Y
  TSY <- TTS %*% Y

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
  
  mod1.st <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = reg)
  mod1a.st <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = subset(reg, stY>0))
  
  ST_X <- ST %*% X
 
  st_X <- as.data.frame(as.matrix(ST_X))
    names(st_X) <- paste("stx", names(st_X), sep="-")
  
  ST_Y <- ST %*% Y
    
  st_Y <- as.data.frame(as.matrix(ST_Y))
    names(st_Y) <- paste("stx", names(st_Y), sep="-")
    
  reg <- cbind(Y, eXes, st_X, st_Y, core$Stories, core$year)
  
  reg <- reg %>%
    rename("year" = "core$year",
           "stories" = "core$Stories") %>%
    filter(!row_number() %in% seq(1,seed))
  
  mod2.st <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = reg)
  mod2a.st <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = subset(reg, st_Y>0))
 
  #Now for the in building neighbors only 
  
  SX <- S0 %*% X
  STX <- S0TT %*% X
  TSX <- TTS0 %*% X
  
  TY <- TT %*% Y
  SY <- S0 %*% Y
  STY <- S0TT %*% Y
  TSY <- TTS0 %*% Y
  
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
  
  mod1.s0t <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = reg)
  mod1a.s0t <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = subset(reg, stY>0))


  ST_X <- S0t %*% X
  st_X <- as.data.frame(as.matrix(ST_X))
  names(st_X) <- paste("stx", names(st_X), sep="-")
 
  ST_Y <- S0t %*% Y
  
  st_Y <- as.data.frame(as.matrix(ST_Y))
  names(st_Y) <- paste("stx", names(st_Y), sep="-")
  
  
  reg <- cbind(Y, eXes, st_X, st_Y, core$Stories, core$year)
  
  reg <- reg %>%
    rename("year" = "core$year",
           "stories" = "core$Stories") %>%
    filter(!row_number() %in% seq(1,seed))
  
  mod2.s0t <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = reg)
  mod2a.s0t <- lm(lnClose ~ . + factor(stories) + factor(year) - stories - year, data = subset(reg, st_Y>0))
  
  
  stargazer(mod1.st, mod2.st, mod1.s0t, mod2.s0t, type="html", out = "./Analysis/R1.html")
  stargazer(mod1a.st, mod2a.st, mod1a.s0t, mod2a.s0t, type="html", out = "./Analysis/R1a.html")
  