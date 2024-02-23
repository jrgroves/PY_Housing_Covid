ggplot(map2) + 
  geom_sf()+
  geom_sf(data=df_coord, size=.5, color="blue") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())




set<-m.data %>%
  select(TMK, lon, lat) %>%
  st_drop_geometry()

df_coord <- st_as_sf(set, coords = c(2:3), crs=CRS)

                          