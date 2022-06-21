# obtaining the proportion of individuals 50+ at certain travel times from facilities


#### packages ####
library(tidyverse)
library(here)
library(sf)
library(raster)
library(rgdal)
library(tmap)


#### data ####

# motorized travel times
access.motor.surgperm <- raster(here("data","malawi","rasters","traveltimes", "motor travel time",
                                 "access_motor_surgery_perm.tif"))

access.motor.surg <-  raster(here("data","malawi","rasters","traveltimes","motor travel time",
                                        "access_motor_surgery.tif"))

access.motor.all <- raster(here("data","malawi","rasters","traveltimes","motor travel time",
                                    "access_motor_all.tif"))


# walking travel times
access.walk.all <- raster(here("data","malawi","rasters","traveltimes","walk travel time",
                                  "access_walk_all.tif"))

access.walk.surg <- raster(here("data","malawi","rasters","traveltimes","walk travel time",
                                   "access_walk_surg.tif"))

access.walk.surgperm <- raster(here("data","malawi","rasters","traveltimes","walk travel time",
                                       "access_walk_surgperm.tif"))


# population over 50 raster
popsum <- raster(here("data","malawi","rasters", "popsum_constrained.tif"))


# admin 1 boundaries
adm1 <- st_read(here("data","malawi","mwi_adm_nso_20181016_shp.nosync","mwi_admbnda_adm1_nso_20181016.shp"))


# location of eye facilities for plotting
eye_fac <- read.csv(here("data","malawi", "Malawi_facilities_public_gps.csv"))
eye_fac_sf <- eye_fac %>% 
  st_as_sf(coords = c("Long", "Lat"), 
           crs=4326)


#### matching resolution and extent of all rasters ####


# aggregating popsum
popsum.agg <- raster::aggregate(popsum, 
                                fact = 10, 
                                fun = sum, 
                                # dissolve = T,
                                expand = T)

compareRaster(popsum.agg, access.motor.all, resolution = T, extent = F, rowcol = F) # same resolution


# ensuring crs are the same
# compareCRS(popsum.agg, access.motor.all)
# compareCRS(popsum.agg, access.motor.surg)
# compareCRS(popsum.agg, access.motor.surgperm) 
# compareCRS(popsum.agg, access.walk.all)
# compareCRS(popsum.agg, access.walk.surg)
# compareCRS(popsum.agg, access.walk.surgperm)
# all TRUE


# now I have to match extent of popsum.agg and the travel time rasters
access.motor.all.resamp <- resample(access.motor.all, 
                              popsum.agg,
                              method = "ngb")

# comparing 
access.motor.all
access.motor.all.resamp
compareRaster(access.motor.all.resamp, popsum.agg) # TRUE


# repating for the other rasters
access.motor.surgperm <- resample(access.motor.surgperm,
                                  popsum.agg,
                                  method = "ngb")
compareRaster(access.motor.surgperm, popsum.agg)

access.motor.surg <- resample(access.motor.surg,
                               popsum.agg,
                               method = "ngb")
compareRaster(access.motor.surg, popsum.agg)

access.motor.all <- resample(access.motor.all,
                              popsum.agg,
                              method = "ngb")
compareRaster(access.motor.all, popsum.agg)

access.walk.all <- resample(access.walk.all,
                             popsum.agg,
                             method = "ngb")
compareRaster(access.walk.all, popsum.agg)

access.walk.surg <- resample(access.walk.surg,
                              popsum.agg,
                              method = "ngb")
compareRaster(access.walk.surg, popsum.agg)

access.walk.surgperm <- resample(access.walk.surgperm,
                                 popsum.agg,
                                 method = "ngb")
compareRaster(access.walk.surgperm, popsum.agg)


#### reclassifying the rasters ####

# creating a reclassification matrix
reclass <- c(0, 60, 1, # within an hour
             60, 120, 2, # within two hours
             120, Inf, 3) # greater than 2 hours

reclass.mat <- matrix(reclass,
                      ncol = 3,
                      byrow = T)
reclass.mat


## motor all
summary(access.motor.all)

# plot histogram of the data
hist(access.motor.all)

# histogram with travel time breaks of interest
hist(access.motor.all,
     breaks = c(0, 60, 120, 500))

# reclassifying raster
motor.all.cat <- reclassify(access.motor.all,
                             reclass.mat,
                             include.lowest = TRUE) # this includes a travel time of zero in the lowest group

# checking work
barplot(motor.all.cat)
# no 0s so no need to reclassify everything
summary(motor.all.cat)

# plotting 
motor.all.plot <- tm_shape(motor.all.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<1hr", "1-2hrs", ">2hrs"),
            title = "Travel time") + 
  tm_shape(eye_fac_sf) + 
  tm_dots(size = 0.05,
          col = "blue",
          title = "Eye care facilities",
          legend.show = TRUE) +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","top"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "A) Motorized travel time to nearest eye care facility",
            main.title.size = 1,
            main.title.position = "left")
motor.all.plot

tmap_save(motor.all.plot, here("results","malawi","maps","motor_all.png"))

## motor surgical services
# summary(access.motor.surg)
# hist(access.motor.surg)
# hist(access.motor.surg,
#      breaks = c(0, 60, 120, 500))
motor.surg.cat <- reclassify(access.motor.surg,
                                reclass.mat,
                                include.lowest = T)
# checking my work
# barplot(motor.surg.cat)
# plotting
motor.surg.plot <- tm_shape(motor.surg.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<1hr", "1-2hrs", ">2hrs"),
            title = "Travel time") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services != "None")) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","top"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "B) Motorized travel time to nearest eye care facility\nwith cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
motor.surg.plot 
tmap_save(motor.surg.plot, here("results","malawi","maps","motor_surg.png"))

## motor permanent surgical services
# summary(access.motor.surgperm)
# hist(access.motor.surgperm)
# hist(access.motor.surgperm,
#      breaks = c(0, 60, 120, 500))
motor.surgperm.cat <-  reclassify(access.motor.surgperm,
                                  reclass.mat,
                                  include.lowest = T)
# barplot(motor.surgperm.cat)
# plotting
motor.surgperm.plot <- tm_shape(motor.surgperm.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<1hr", "1-2hrs", ">2hrs"),
            title = "Travel time") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","top"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "C) Motorized travel time to nearest eye care facility\nwith permanent cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
motor.surgperm.plot 
tmap_save(motor.surgperm.plot, here("results","malawi","maps","motor_surgperm.png"))

## walking
summary(access.walk.all)

# plot histogram of the data
hist(access.walk.all)

# reclassifying raster
walk.all.cat <- reclassify(access.walk.all,
                            reclass.mat,
                            include.lowest = TRUE)

# checking work
barplot(walk.all.cat)
summary(walk.all.cat)

# plotting 
walk.all.plot <- tm_shape(walk.all.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<1hr", "1-2hrs", ">2hrs"),
            title = "Travel time") + 
  tm_shape(eye_fac_sf) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","top"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "D) Walking travel time to nearest eye care facility",
            main.title.size = 1,
            main.title.position = "left")
walk.all.plot 
tmap_save(walk.all.plot , here("results","malawi","maps","walk_all.png"))

## motor surgical services
summary(access.walk.surg)
hist(access.walk.surg)

walk.surg.cat <- reclassify(access.walk.surg,
                             reclass.mat,
                             include.lowest = T)
# checking my work
# barplot(walk.surg.cat)
# plotting the raster
walk.surg.plot <- tm_shape(walk.surg.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<1hr", "1-2hrs", ">2hrs"),
            title = "Travel time") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services != "None")) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","top"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "E) Walking travel time to nearest eye care facility\nwith cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
walk.surg.plot 
tmap_save(walk.surg.plot , here("results","malawi","maps","walk_surg.png"))


## walk permanent surgical services
summary(access.walk.surgperm)
hist(access.walk.surgperm)

walk.surgperm.cat <-  reclassify(access.walk.surgperm,
                                  reclass.mat,
                                  include.lowest = T)
# barplot(walk.surgperm.cat)
# plotting the raster
walk.surgperm.plot <- tm_shape(walk.surgperm.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<1hr", "1-2hrs", ">2hrs"),
            title = "Travel time") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","top"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "F) Walking travel time to nearest eye care facility\nwith permanent cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
walk.surgperm.plot 
tmap_save(walk.surgperm.plot  , here("results","malawi","maps","walk_surgperm.png"))


#### zonal statistics for each combination of walking/motorized and type of eye care facility ####

# motorized transport
compareRaster(motor.all.cat, popsum.agg)
table.motor.all <- zonal(popsum.agg,
                         motor.all.cat,
                         fun = 'sum') %>%
  as_tibble() %>%
  mutate(service="all",
         transport="motor")  
table.motor.all

compareRaster(motor.surg.cat, popsum.agg)
table.motor.surg <- zonal(popsum.agg,
                         motor.surg.cat,
                         fun = 'sum') %>%
  as_tibble() %>%
  mutate(service="surg",
         transport="motor")  
table.motor.surg


compareRaster(motor.surgperm.cat, popsum.agg)
table.motor.surgperm <- zonal(popsum.agg,
                          motor.surgperm.cat,
                          fun = 'sum') %>%
  as_tibble() %>%
  mutate(service="perm",
         transport="motor") 
table.motor.surgperm

# walking
compareRaster(access.walk.all, popsum.agg)
table.walk.all <- zonal(popsum.agg,
                        walk.all.cat,
                        fun = 'sum') %>%
  as_tibble() %>%
  mutate(service="all",
         transport="walk")
table.walk.all

compareRaster(access.walk.surg, popsum.agg)
table.walk.surg <- zonal(popsum.agg,
                        walk.surg.cat,
                        fun = 'sum') %>%
  as_tibble() %>%
  mutate(service="surg",
         transport="walk")
table.walk.surg

compareRaster(access.walk.surgperm, popsum.agg)
table.walk.surgperm <- zonal(popsum.agg,
                         walk.surgperm.cat,
                         fun = 'sum') %>%
  as_tibble() %>%
  mutate(service="perm",
         transport="walk")

table.walk.surgperm


#### creating a table of the results ####

par <- function(x) { paste0("(",x,"%)")}

table <- full_join(table.motor.all, table.motor.surg) %>%
  full_join(.,  table.motor.surgperm) %>%
  full_join(., table.walk.all) %>%
  full_join(., table.walk.surg) %>%
  full_join(., table.walk.surgperm) %>%
  pivot_wider(names_from = "zone",
              values_from = "sum") %>%
  rename(n_less1=`1`,
         n_1to2=`2`,
         n_2plus=`3`) %>%
  mutate(n_less1 = round(n_less1),
         n_1to2 = round(n_1to2),
         n_2plus = round(n_2plus),
         total = n_less1 + n_1to2 + n_2plus,
         p_less1 = round((n_less1 / total * 100), digits = 1),
         p_1to2 = round((n_1to2 / total * 100), digits = 1),
         p_2plus = round((n_2plus / total * 100), digits = 1)) %>%
  mutate(across(contains("p_"), par)) %>%
  unite(less1, n_less1, p_less1, sep = " ") %>%
  unite(oneto2, n_1to2, p_1to2, sep = " ") %>%
  unite(twoplus, n_2plus, p_2plus, sep = " ") %>%
  mutate(transport = if_else(transport == "motor", "Motorized", "Walking"),
         service = case_when(service == "all" ~ "All eye care facilities",
                             service == "surg" ~ "Facilities with surgical services",
                             service == "perm" ~ "Facilities with permanent surgical services")) %>%
  dplyr::select(-total) %>%
  rename(`<1 hour`=less1,
         `1-2 hours`=oneto2,
         `>2 hours`=twoplus,
         Services=service,
         Transportation=transport)
table

write_csv(table, here("results","malawi","tables","traveltime_motorwalk.csv"))

# #### getting results stratified by admin1 boundary
# 
# # getting spatial polygons corresponding to each admin level
# adm1.cent <- adm1 %>% filter(ADM1_EN == "Central")
# adm1.north <- adm1 %>% filter(ADM1_EN == "Northern")
# adm1.south <- adm1 %>% filter(ADM1_EN == "Southern")
# 
# #### clipping and calculating pop at travel time admin 1 areas ####
# 
# # motorized
# # all facilities
# # north
# popsum.agg.north <- mask(popsum.agg, adm1.north)
# motor.all.cat.nortb <- mask(motor.all.cat, adm1.north)
# table.motor.all.north <- zonal(popsum.agg.north,
#                                motor.all.cat.north,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "north",
#          transport = "motor",
#          service = "all")
# table.motor.all.north
# 
# # center
# popsum.agg.cent <- mask(popsum.agg, adm1.cent)
# motor.all.cat.cent <- mask(motor.all.cat, adm1.cent)
# table.motor.all.cent <- zonal(popsum.agg.cent,
#                                motor.all.cat.cent,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "cent",
#          transport = "motor",
#          service = "all")
# table.motor.all.cent
# 
# # south
# popsum.agg.south <- mask(popsum.agg, adm1.south)
# motor.all.cat.south <- mask(motor.all.cat, adm1.south)
# table.motor.all.south <- zonal(popsum.agg.south,
#                               motor.all.cat.south,
#                               fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "south",
#          transport = "motor",
#          service = "all")
# table.motor.all.south
# 
# # facilities with cataract surgical services
# # north
# motor.surg.cat.north <- mask(motor.surg.cat, adm1.north)
# table.motor.surg.north <- zonal(popsum.agg.north,
#                                motor.surg.cat.north,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "north",
#          transport = "motor",
#          service = "surg")
# table.motor.surg.north
# 
# # center
# motor.surg.cat.cent <- mask(motor.surg.cat, adm1.cent)
# table.motor.surg.cent <- zonal(popsum.agg.cent,
#                               motor.surg.cat.cent,
#                               fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "cent",
#          transport = "motor",
#          service = "surg")
# table.motor.surg.cent
# 
# # south
# motor.surg.cat.south <- mask(motor.surg.cat, adm1.south)
# table.motor.surg.south <- zonal(popsum.agg.south,
#                                motor.surg.cat.south,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "south",
#          transport = "motor",
#          service = "surg")
# table.motor.surg.south
# 
# # permanent cataract surgical services
# # north
# motor.perm.north <- mask(motor.surgperm.cat, adm1.north)
# table.motor.perm.north <- zonal(popsum.agg.north,
#                                 motor.perm.north,
#                                 fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "north",
#          transport = "motor",
#          service = "perm")
# table.motor.perm.north
# 
# # center
# motor.perm.cent <- mask(motor.surgperm.cat, adm1.cent)
# table.motor.perm.cent <- zonal(popsum.agg.cent,
#                                motor.perm.cent,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "cent",
#          transport = "motor",
#          service = "perm")
# table.motor.perm.cent
# 
# # south
# motor.perm.south <- mask(motor.surgperm.cat, adm1.south)
# table.motor.perm.south <- zonal(popsum.agg.south,
#                                 motor.perm.south,
#                                 fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "south",
#          transport = "motor",
#          service = "perm")
# table.motor.perm.south
# 
# 
# ## repeating for walking
# # all facilities
# # north
# walk.all.cat.north <- mask(walk.all.cat, adm1.north)
# table.walk.all.north <- zonal(popsum.agg.north,
#                                walk.all.cat.north,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "north",
#          transport = "walk",
#          service = "all")
# table.walk.all.north
# 
# # center
# walk.all.cat.cent <- mask(walk.all.cat, adm1.cent)
# table.walk.all.cent <- zonal(popsum.agg.cent,
#                               walk.all.cat.cent,
#                               fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "cent",
#          transport = "walk",
#          service = "all")
# table.walk.all.cent
# 
# # south
# walk.all.cat.south <- mask(walk.all.cat, adm1.south)
# table.walk.all.south <- zonal(popsum.agg.south,
#                                walk.all.cat.south,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "south",
#          transport = "walk",
#          service = "all")
# table.walk.all.south
# 
# # facilities with cataract surgical services
# # north
# walk.surg.cat.north <- mask(walk.surg.cat, adm1.north)
# table.walk.surg.north <- zonal(popsum.agg.north,
#                                 walk.surg.cat.north,
#                                 fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "north",
#          transport = "walk",
#          service = "surg")
# table.walk.surg.north
# 
# # center
# walk.surg.cat.cent <- mask(walk.surg.cat, adm1.cent)
# table.walk.surg.cent <- zonal(popsum.agg.cent,
#                                walk.surg.cat.cent,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "cent",
#          transport = "walk",
#          service = "surg")
# table.walk.surg.cent
# 
# # south
# walk.surg.cat.south <- mask(walk.surg.cat, adm1.south)
# table.walk.surg.south <- zonal(popsum.agg.south,
#                                 walk.surg.cat.south,
#                                 fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "south",
#          transport = "walk",
#          service = "surg")
# table.walk.surg.south
# 
# # permanent cataract surgical services
# # north
# walk.perm.north <- mask(walk.surgperm.cat, adm1.north)
# table.walk.perm.north <- zonal(popsum.agg.north,
#                                 walk.perm.north,
#                                 fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "north",
#          transport = "walk",
#          service = "perm")
# table.walk.perm.north
# 
# # center
# walk.perm.cent <- mask(walk.surgperm.cat, adm1.cent)
# table.walk.perm.cent <- zonal(popsum.agg.cent,
#                                walk.perm.cent,
#                                fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "cent",
#          transport = "walk",
#          service = "perm")
# table.walk.perm.cent
# 
# # south
# walk.perm.south <- mask(walk.surgperm.cat, adm1.south)
# table.walk.perm.south <- zonal(popsum.agg.south,
#                                 walk.perm.south,
#                                 fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(adm = "south",
#          transport = "walk",
#          service = "perm")
# table.walk.perm.south
# 
# 
# #### putting results together into table ####
# 
# table.adm <- full_join(table.motor.all.north, table.motor.all.cent) %>%
#   full_join(., table.motor.all.south) %>%
#   full_join(., table.motor.surg.north) %>%
#   full_join(., table.motor.surg.cent) %>%
#   full_join(., table.motor.surg.south) %>%
#   full_join(., table.motor.perm.north) %>%
#   full_join(., table.motor.perm.cent) %>%
#   full_join(., table.motor.perm.south) %>%
#   full_join(., table.walk.all.north) %>%
#   full_join(., table.walk.all.cent) %>%
#   full_join(., table.walk.all.south) %>%
#   full_join(., table.walk.surg.north) %>%
#   full_join(., table.walk.surg.cent) %>%
#   full_join(., table.walk.surg.south) %>%
#   full_join(., table.walk.perm.north) %>%
#   full_join(., table.walk.perm.cent) %>%
#   full_join(., table.walk.perm.south) %>%
#   pivot_wider(names_from = "zone",
#               values_from ="sum") %>%
#   rename(n_less1=`1`,
#          n_1to2=`2`,
#          n_2plus=`3`) %>%
#   mutate(across(contains("n_"), round),
#          total = n_less1 + n_1to2 + n_2plus,
#          p_less1 = round((n_less1 / total * 100), digits = 1),
#          p_1to2 = round((n_1to2 / total * 100), digits = 1),
#          p_2plus = round((n_2plus / total * 100), digits = 1)) %>%
#   mutate(across(contains("p_"), par)) %>%
#   unite(less1, n_less1, p_less1, sep = " ") %>%
#   unite(oneto2, n_1to2, p_1to2, sep = " ") %>%
#   unite(twoplus, n_2plus, p_2plus, sep = " ") %>%
#   mutate(transport = if_else(transport == "motor", "Motorized", "Walking"),
#          service = case_when(service == "all" ~ "All eye care facilities",
#                              service == "surg" ~ "Facilities with surgical services",
#                              service == "perm" ~ "Facilities with permanent surgical services"),
#          adm = case_when(adm == "north" ~ "North",
#                          adm == "cent" ~ "Center",
#                          adm == "south" ~ "South")) %>%
#   dplyr::select(-total) %>%
#   rename(`<1 hour`=less1,
#          `1-2 hours`=oneto2,
#          `>2 hours`=twoplus,
#          Services=service,
#          Transportation=transport,
#          `Administrative zone` = adm)
# table.adm  
# write_csv(table.adm, here("results","tables","admin_travel_time.csv"))








