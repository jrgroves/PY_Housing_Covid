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
    select(num_sale, Covid, Covid2, Cases, BathsFull, BathsHalf, BedsTotal,DOM,livSQFT,SQFTGarageCarport,real.list.price,real.close.price,
           diff.price, Basement, Split, PUD, LowRise, HighRise,Townhouse,Condotel,SingleFam, Duplex, MultiDwell, WalkUP,
           pool, Age, Age2, Age.r, SqftTotal, par_area, beach, park, hospital, airport, elem_sch, mid_sch, high_sch,
           per_white, per_black, per_asian,
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
         caption = "Veritcal line represents start of COVID-19 pandemic. Hoizontal segments are yearly averages") +
    theme_bw()


 F2<-ggplot(sub.mon) +
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
 
#Using the smaller sample
 
 main <- main.s
 
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
   select(num_sale, Covid, Covid2, Cases, BathsFull, BathsHalf, BedsTotal,DOM,livSQFT,SQFTGarageCarport,real.list.price,real.close.price,
          diff.price, Basement, Split, PUD, LowRise, HighRise,Townhouse,Condotel,SingleFam, Duplex, MultiDwell, WalkUP,
          pool, Age, Age2, Age.r, SqftTotal, par_area, beach, park, hospital, airport, elem_sch, mid_sch, high_sch,
          per_white, per_black, per_asian,
          per_hawaian,per_occupied,per_vacant,per_owner,per_renter, HOA, Elevator, remod, lat, lon) 
 sum.data <- cbind(sum.data, sumfac)
 
 stargazer(sum.data, subset(sum.data, Covid==0), subset(sum.data, Covid==1),
           type="text",out="./Analysis/Output/hawaii_small.txt")
 
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
 
 
 F1.2<-ggplot(sub.mon) +
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
 
 
 F2.2<-ggplot(sub.mon) +
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
 
 #Using the smaller sample (first sell in set)
 
 main <- main.s2
 
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
   select(num_sale, Covid, Covid2, Cases, BathsFull, BathsHalf, BedsTotal,DOM,livSQFT,SQFTGarageCarport,real.list.price,real.close.price,
          diff.price, Basement, Split, PUD, LowRise, HighRise,Townhouse,Condotel,SingleFam, Duplex, MultiDwell, WalkUP,
          pool, Age, Age2, Age.r, SqftTotal, par_area, beach, park, hospital, airport, elem_sch, mid_sch, high_sch,
          per_white, per_black, per_asian,
          per_hawaian,per_occupied,per_vacant,per_owner,per_renter, HOA, Elevator, remod, lat, lon) 
 sum.data <- cbind(sum.data, sumfac)
 
 stargazer(sum.data, subset(sum.data, Covid==0), subset(sum.data, Covid==1),
           type="text",out="./Analysis/Output/hawaii_small2.txt")
 
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
 
 
 F1.3<-ggplot(sub.mon) +
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
 
 
 F2.3<-ggplot(sub.mon) +
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
 
 