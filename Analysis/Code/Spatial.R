rm(list=ls())

library(spatialreg)
library(tidyverse)
library(spdep)
library(spgwr)

#Read in Cleaned Data

#Read in Cleaned Data

load("./Build/Output/CoreData.RData")

GWRbandwidth <- gwr.sel(ln.r.close ~ Covid + DOM + factor(year), data=main.s, 
                        coords=cbind(main.s$lon, main.s$lat),adapt=T)

gwr.model = gwr(ln.r.close ~ Covid + DOM + factor(year), data=main.s,  coords=cbind(main.s$lon, main.s$lat), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 




coords <- cbind(work$lon, work$lat)
k1 <- knn2nb(knearneigh(coords, k = 7))

invd.weights <- nb2listw(k1,style = "W")

N<-fit.lag<-lagsarlm(lnClose ~ Covid + BedsTotal + BathsFull + BathsHalf + DOM + Stories + SqftTotal +
                     Age + Age2 + Basement + factor(cond) + factor(LUC) + factor(year) +
                     Split + PUD + LowRise + HighRise + Townhouse + Condotel + Duplex + WalkUP +
                     beach + park + hospital + airport + per_black + per_asian + per_hawaian +
                     per_owner + per_occupied + Parking + HOA + remod + Elevator + elem_sch + mid_sch + high_sch,
                     data = work, 
                     listw = invd.weights) 















critical.threshold <- max(unlist(nbdists(k1,coords)))
critical.threshold

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
nb<-nb.dist.band

distances <- nbdists(nb.dist.band,coords)

invd1 <- lapply(distances, function(x) ((1/x)*100))



#Conversion to only previous sales######

as.nb.sgbp <- function(x) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

nb<-k1


for(i in seq(1,length(nb),1)){
  c<-lapply(nb[[i]], function(x) x > i)
  nb[[i]][unlist(c)] <- NA
}

c<-lapply(nb, function(x) x[!is.na(x)])
############

c<-as.nb.sgbp(c)
invd.weights2 <- nb2listw(c, style = "B", zero.policy = TRUE)

N2<-fit.lag<-lagsarlm(lnClose ~ Covid+Age,
                     data = work, 
                     listw = invd.weights2,
                     zero.policy=TRUE) 
