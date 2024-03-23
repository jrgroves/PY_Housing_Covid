#Analizes the model based on Pace, et. al. (98) for Single Family Housing
  #Jeremy R. Groves
  #January 31, 2024
  #Revised February 23, 2024


rm(list=ls())

library(tidyverse)
library(Matrix)
library(matrixcalc)
library(stargazer)

#Create Main Spatial Weight Matrix
load("./Build/Output/Space.RData")

S <- s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15

rm(list = grep("^s", ls(), value = TRUE, invert = FALSE))


#Create Main Temporal Weight Matrix
load("./Build/Output/Time_SF.RData")

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
    
    rs<-rowSums(TT)
    rs[which(rs>0)] <- 1/rs[which(rs>0)]
    TT <- TT * rs #this row standardizes the matrix

#Load Core Data and filter
    load("./Build/Output/CoreData.RData")
    rm(main.s, main.s2)
    
    core <- main %>%
      filter(PropertyType == "Single Family") %>%
      arrange(CloseDate) %>%
      mutate(ID = 1:n()) 
    rm(main)

#Descriptive tables and data creation

    time <- core$CloseDate
    d <- as.data.frame(table(factor(time))) #Determines the number of sales each day

#Set the seed which are the observations that will be removed because they have no previous sales and determine the number of neighbors
  seed <- ceiling(nrow(core) * .01)


  #Subset out the independent variables 
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
           per_occupied, per_owner, Split, PUD, Townhouse, Duplex, MultiDwell,
           Excel_Cond, AbAvg_Cond, Fair_Cond)
  
  stargazer(eXes, type="html", out = "./Analysis/Output/SF_sum.html")
  
#Analysis of Sales Prices
   
  prices <- core %>%
    select(real.close.price, mon.yr, year) %>%
    mutate(st_price = as.vector(ST%*%real.close.price),
           s_price  = as.vector(S%*%real.close.price))

  
  sub.mon <- prices %>%
    group_by(mon.yr) %>%
    summarize(mean = mean(real.close.price),
              st_mean = mean(st_price[st_price>0]),
              s_mean = mean(s_price[s_price>0]),
              sales = n(),
              sales.m = mean(n()),
              max = max(real.close.price))
  
  sub.yr <- core %>%
    group_by(year) %>%
    summarize(mean = mean(real.close.price),
              sales = n(),
              sales.m = sales/12,
              max = max(real.close.price))
  
  sub.mon <- sub.mon %>%    
    mutate(year = as.numeric(substr(mon.yr, 0,4))) %>%
    left_join(., sub.yr, by="year")
  
  F1<-ggplot(sub.mon) +
    geom_line(aes(x = mon.yr, y=mean.x, group = 1)) +
    geom_vline(aes(xintercept = "2020-03")) +
    geom_segment(data=subset(sub.mon, year==2016), aes(x = "2016-01", y = mean.y, xend = "2016-12", yend = mean.y)) +
    geom_segment(data=subset(sub.mon, year==2017), aes(x = "2017-01", y = mean.y, xend = "2017-12", yend = mean.y)) +
    geom_segment(data=subset(sub.mon, year==2018), aes(x = "2018-01", y = mean.y, xend = "2018-12", yend = mean.y)) +
    geom_segment(data=subset(sub.mon, year==2019), aes(x = "2019-01", y = mean.y, xend = "2019-12", yend = mean.y)) +
    geom_segment(data=subset(sub.mon, year==2020), aes(x = "2020-01", y = mean.y, xend = "2020-12", yend = mean.y)) +
    geom_segment(data=subset(sub.mon, year==2021), aes(x = "2021-01", y = mean.y, xend = "2021-12", yend = mean.y)) +
    geom_segment(data=subset(sub.mon, year==2022), aes(x = "2022-01", y = mean.y, xend = "2022-12", yend = mean.y)) +
    scale_x_discrete(guide = guide_axis(angle = 90),
                     breaks = sub.mon$mon.yr[seq(1, length(sub.mon$mon.yr), by = 2)]) +
    scale_y_continuous(breaks = seq(800000, 1600000, 100000))+
    labs(x = "Month",
         y = "Average Real Closing Price",
         caption = "Veritcal line represents start of COVID-19 pandemic. Hoizontal segments are yearly averages",
         title = "Average Real Closing Price by Month for Single Family Units") +
    theme_bw()
  
  F2<-ggplot(sub.mon) +
    geom_line(aes(x = mon.yr, y=mean.x, group = 1, color = "Focus Unit"), linewidthF1 = 1) +
    geom_vline(aes(xintercept = "2020-03")) +
    geom_line(aes(x = mon.yr, y=st_mean, group = 2, color = "Spatial-Temporal Average"), linewidth = .75, alpha = .5)+
    geom_line(aes(x = mon.yr, y=s_mean, group = 3, color = "Spatial Average"), linewidth = .75, alpha = .5)+
    scale_x_discrete(guide = guide_axis(angle = 90),
                     breaks = sub.mon$mon.yr[seq(1, length(sub.mon$mon.yr), by = 2)]) +
    scale_y_continuous(breaks = seq(800000, 1600000, 100000))+
    labs(x = "Month",
         y = "Average Real Closing Price",
         caption = "Veritcal line represents start of COVID-19 pandemic. Hoizontal segments are yearly averages",
         title = "Average Real Closing Price by Month - Single Family Units (ignore zeros)") +
    guides(colour = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
  
  