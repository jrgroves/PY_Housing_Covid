#Analysis and Regressions for Hawaii Housing Data and Covid
#Jeremy R. Groves
#June 22, 2023
#NOTES:
    #Only use Elementary Schools because there is too much overlap with middle and High Schools causing NAs

rm(list=ls())

library(tidyverse)
library(estimatr)
library(stargazer)
library(spgwr)

#Read in Cleaned Data

  load("./Build/Output/CoreData.RData")
  
#Create New Variables for Analysis
  main <- core.2 %>%
    mutate(Covid2 = case_when(CloseDate <= "2020-03-01" ~ 0,
                              CloseDate >  "2022-06-30" ~ 0,
                              TRUE ~ 1),
           Cases2 = case_when(is.na(Cases) ~ 0,
                              TRUE ~ Cases)) %>%
  filter(!is.na(fld_zone))  #Removes two observations with no flood zone
  
  #Summary Statistics
  
  a<-as.data.frame(model.matrix(~PropertyType - 1, data=main))
  b<-as.data.frame(model.matrix(~Stories - 1, data=main))
  c<-as.data.frame(model.matrix(~year - 1, data=main))
  d<-as.data.frame(model.matrix(~LUC - 1, data=main))
  e<-as.data.frame(model.matrix(~cond - 1, data=main))
  f<-as.data.frame(model.matrix(~Parking - 1, data=main))
  g<-as.data.frame(model.matrix(~fld_zone - 1, data=main))
  
  sumfac<-as.data.frame(cbind(a,b,c,d,e,f,g))
  
  sum.data <- main %>%
    select(num_sale, Covid, Covid2, Cases, Cases2, BathsFull, BathsHalf, BedsTotal,DOM,livSQFT,SQFTGarageCarport,real.list.price,real.close.price,
           Basement, Split, PUD, LowRise, HighRise,Townhouse,Condotel,SingleFam, Duplex, MultiDwell, WalkUP,
           pool, Age, Age2, Age.r, par_area, beach, park, hospital, airport, per_white, per_black, per_asian,
           per_hawaian,per_occupied,per_vacant,per_owner,per_renter, HOA, Elevator, remod, lat, lon) 
  sum.data <- cbind(sum.data, sumfac)
  
  stargazer(sum.data, subset(sum.data, Covid==0), subset(sum.data, Covid==1),
           type="text",out="./Analysis/Output/hawaii_full.txt")
  
#Create Summary for Images
  
  sub.mon <- main %>%
    group_by(mon.yr) %>%
    summarize(mean = mean(real.close.price),
              sales = n(),
              sales.m = mean(n()),
              max = max(real.close.price))
  
  sub.yr <- main %>%
    group_by(year) %>%
    summarize(mean = mean(real.close.price),
              sales = n(),
              sales.m = sales/12,
              max = max(real.close.price))
  
  sub.mon <- sub.mon %>%
    mutate(year = as.numeric(substr(mon.yr, 0,4))) %>%
    left_join(., sub.yr, by="year")
  
  
  ggplot(sub.mon) +
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
         caption = "Veritcal line represents start of COVID-19 pandemic. Hoizontal segments are yearly averages") +
    theme_bw()
  

 ggplot(sub.mon) +
    geom_line(aes(x = mon.yr, y = sales.x, group = 1)) +
    geom_vline(aes(xintercept = "2020-03")) +
    geom_segment(data=subset(sub.mon, year==2016), aes(x = "2016-01", y = sales.m.y, xend = "2016-12", yend = sales.m.y)) +
    geom_segment(data=subset(sub.mon, year==2017), aes(x = "2017-01", y = sales.m.y, xend = "2017-12", yend = sales.m.y)) +
    geom_segment(data=subset(sub.mon, year==2018), aes(x = "2018-01", y = sales.m.y, xend = "2018-12", yend = sales.m.y)) +
    geom_segment(data=subset(sub.mon, year==2019), aes(x = "2019-01", y = sales.m.y, xend = "2019-12", yend = sales.m.y)) +
    geom_segment(data=subset(sub.mon, year==2020), aes(x = "2020-01", y = sales.m.y, xend = "2020-12", yend = sales.m.y)) +
    geom_segment(data=subset(sub.mon, year==2021), aes(x = "2021-01", y = sales.m.y, xend = "2021-12", yend = sales.m.y)) +
    geom_segment(data=subset(sub.mon, year==2022), aes(x = "2022-01", y = sales.m.y, xend = "2022-12", yend = sales.m.y)) +
    scale_x_discrete(guide = guide_axis(angle = 90),
                     breaks = sub.mon$mon.yr[seq(1, length(sub.mon$mon.yr), by = 2)]) +
    scale_y_continuous(breaks = seq(0, 1000, 125))+
    labs(x = "Month",
         y = "Average Monthly Sales",
         caption = "Veritcal line represents start of COVID-19 pandemic. Hoizontal segments are yearly averages") +
    theme_bw()
 
 rm(a, b, c, d, e, f, g, sumfac, sum.data)
  
#Regression Models
 
  mod1 <- lm(ln.r.close ~ Covid, data = main) 

  mod2 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond), data=main)
  mod3 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(year), data=main) 
  mod4 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year), data=main)
  mod5 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP, data=main)
  mod6 <- lm(ln.r.close ~ Covid2 + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
               beach + park + hospital + airport, data=main)
  mod7 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
               beach + park + hospital + airport, data=main)
  mod8 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
               beach + park + hospital + airport + per_black + per_asian + per_hawaian +
               per_owner + per_occupied, data=main)
  mod9 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
               beach + park + hospital + airport + per_black + per_asian + per_hawaian +
               per_owner + per_occupied + Parking + HOA + remod + Elevator, data=main)
  mod10 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
               Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
               Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
               beach + park + hospital + airport + per_black + per_asian + per_hawaian +
               per_owner + per_occupied + Parking + HOA + remod + Elevator + elem_sch + mid_sch + high_sch +
               lat + lon, data=main)
  mod11 <- lm(ln.r.close ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
                Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
                Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
                beach + park + hospital + airport + per_black + per_asian + per_hawaian +
                per_owner + per_occupied + Parking + HOA + remod + Elevator + elem_sch + mid_sch + high_sch +
                lat + lon + Cases2, data=main)

  