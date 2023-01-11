# proportion of individuals 50+ at certain travel times from facilities in Rwanda

#### packages ####
library(tidyverse)
library(here)
library(sf)
library(raster)
library(rgdal)
library(tmap)

#### data ####

# motorized travel times
access.motor.surgperm <- raster(here("data", "rwanda", "rasters", "traveltimes", "motor travel time",
                                     "access_motor_surgperm.tif"))

access.motor.surg <-  raster(here("data", "rwanda", "rasters","traveltimes","motor travel time",
                                  "access_motor_surg.tif"))

access.motor.all <- raster(here("data", "rwanda", "rasters","traveltimes","motor travel time",
                                "access_motor_all.tif"))


# population over 50 raster
popsum <- raster(here("data", "rwanda", "rasters", "popsum_constrained.tif"))


# admin 1 boundaries
adm1 <- st_read(here("data", "rwanda", "rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync", "rwa_adm1_2006_NISR_WGS1984_20181002.shp"))


# location of eye facilities for plotting
eye_fac <- read.csv(here("data", "rwanda", "Rwanda_facilities_gps.csv"))
eye_fac_sf <- eye_fac %>% 
  st_as_sf(coords = c("Long", "Lat"), 
           crs=4326)

# waterways
water <- st_read(here("data","rwanda","rwalakes","RWA_Lakes_NISR.shp")) %>%
  st_transform(4326) %>%
  st_intersection(., adm1)
# tm_shape(water) + tm_polygons(col = "blue")


#### matching resolution and extent of all rasters ####

# aggregating popsum so it matches the friction surface resolution
popsum.agg <- raster::aggregate(popsum, 
                                fact = 10, 
                                fun = sum, 
                                dissolve = T,
                                expand = TRUE)

compareRaster(popsum.agg, access.motor.all, resolution = T, extent = F, rowcol = F) # same resolution

# now I have to match extent of popsum.agg and the travel time rasters

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

#### reclassification matrix ####

# creating reclassification matrix
reclass <- c(0, 60, 1, # within an hour
             60, 120, 2, # over an hour
             120, Inf, 3) # greater than 2 hours

reclass.mat <- matrix(reclass,
                      ncol = 3,
                      byrow = TRUE)
reclass.mat

## motor all
summary(access.motor.all)

# plot histogram of the data
hist(access.motor.all)

# histogram with travel time breaks of interest
hist(access.motor.all,
     breaks = c(0, 60, 120, 500))


#### reclassifying rasters and plotting travel time to facilities ####

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
            labels = c("<60", "60-120", ">120"),
            title = "Travel time (mins)") + 
  tm_shape(eye_fac_sf) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels= "Eye care facilities",
          legend.show = TRUE) +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "Motorized travel time to nearest eye care facility",
            main.title.size = 1,
            main.title.position = "left")
motor.all.plot


tmap_save(motor.all.plot, here("results", "rwanda", "maps","motor travel time","motor_all.png"))

## motor surgical services
# summary(access.motor.surg)
# hist(access.motor.surg)
# hist(access.motor.surg,
#      breaks = c(0, 60, 120, 500))
motor.surg.cat <- reclassify(access.motor.surg,
                             reclass.mat,
                             include.lowest = TRUE)
# checking my work
# barplot(motor.surg.cat)
# plotting
motor.surg.plot <- tm_shape(motor.surg.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<60", "60-120", ">120"),
            title = "Travel time (mins)") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services != "None")) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "Motorized travel time to nearest eye care facility\nwith cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
motor.surg.plot 
tmap_save(motor.surg.plot, here("results", "rwanda", "maps","motor travel time","motor_surg.png"))

## motor permanent surgical services
# summary(access.motor.surgperm)
# hist(access.motor.surgperm)
# hist(access.motor.surgperm,
#      breaks = c(0, 60, 120, 500))
motor.surgperm.cat <-  reclassify(access.motor.surgperm,
                                  reclass.mat,
                                  include.lowest = TRUE)
# barplot(motor.surgperm.cat)
# plotting
motor.surgperm.plot <- tm_shape(motor.surgperm.cat) +
  tm_raster(style = "cat",
            palette = c("#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("<60", "60-120", ">120"),
            title = "Travel time (mins)") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "C) Motorized travel time to nearest eye care facility\nwith permanent cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
motor.surgperm.plot 
tmap_save(motor.surgperm.plot, here("results", "rwanda", "maps","motor travel time","motor_surgperm.png"))


#### obtaining population density ####
# attempting to get population density per grid, per MAP FS paper the resolution should be 1km x 1km
# need to reclassify as <1 and >=1 person per km2

popsum.reclass <- c(0, 1, 1, # less than 1 person per km2
                    1, Inf, 10) # > 1 person per km2

popsum.reclass.mat <- matrix(popsum.reclass,
                             ncol = 3,
                             byrow = TRUE)

summary(popsum.agg)
hist(popsum.agg)
hist(popsum.agg,
     breaks = c(0,1,2000))

# reclassifying raster
popsum.agg.cat <- reclassify(popsum.agg,
                             popsum.reclass.mat,
                             include.lowest = TRUE)
popsum.agg.cat
plot(popsum.agg.cat, 
     col = c("green","red"))
plot(popsum.agg)
plot(popsum.agg.cat)


#### <1hr to facility with permanent cataract surgical services ####
# multiplying the two rasters
unique(popsum.agg.cat)
unique(motor.surgperm.cat)
popsumxmotorsurgperm <- motor.surgperm.cat * popsum.agg.cat
popsumxmotorsurgperm
unique(popsumxmotorsurgperm)

# reclassifying it into the travel time and population categories
# groups: within 1 hr, >1hr with <1person/km, and >1hr with >1person/km
motorsurgperm1hr <- reclassify(popsumxmotorsurgperm,
                                matrix(c(1, 1, # <1 person/km within 1 hr  -- group 1
                                         2, 2, # <1 person/km within 1-2hrs -- group 2
                                         3, 2, # <1 person/km 2+ hrs -- group 2
                                         10, 1, # >1 person/km within 1 hr -- group 1
                                         20, 3, # >1 person/km within 1-2hrs -- group 3
                                         30, 3), # >1 person/km 2+ hrs -- group 3
                                         ncol = 2,
                                         byrow = TRUE),
                                include.lowest = F)
unique(motorsurgperm1hr) # works correctly
plot(motorsurgperm1hr)

# replacing NA's with 0
motorsurgperm1hr[is.na(motorsurgperm1hr[])] <- 0
unique(motorsurgperm1hr)
plot(motorsurgperm1hr)

# clipping to match rwanda
motorsurgperm1hr <- crop(motorsurgperm1hr, adm1)
motorsurgperm1hr <- mask(motorsurgperm1hr, adm1)
plot(motorsurgperm1hr)

# mapping
motor.surgperm.1hr.plot <- tm_shape(motorsurgperm1hr) +
  tm_raster(style = "cat",
            palette = c("grey","#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("Unpopulated area",
                       "Within 1hrs travel time to nearest facility", 
                       "<1 person/km2 who would take >1hrs to travel to nearest facility", 
                       "≥1 person/km2 who would take >1hrs to travel to nearest facility"),
            title = "Travel time to nearest eye care facility with \npermanent cataract surgical coverage") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
  tm_dots(size = 0.25,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  # tm_shape(water) + tm_polygons(col = "blue") +
  tm_legend(legend.position = c("right","bottom"),
            legend.title.size = 1,
            legend.text.size = 0.6,
            # legend.width = 0.5,
            #legend.outside = T,
            legend.stack = "vertical",
            legend.height = 1.2,
            legend.width = 0.45) + 
  tm_layout(main.title = "Geographic access to eye care facilities with permanent cataract surgical \ncoverage",
            main.title.size = 1.2,
            main.title.position = "left")
motor.surgperm.1hr.plot
tmap_save(motor.surgperm.1hr.plot, here("results", "rwanda", "maps","pop at travel time","pop_1h_motor_surgperm.png"))

#### <2hr to facility with permanent cataract surgical services ####
# using raster multiplied above
# reclassifying it into the travel time and population categories
# groups: within 2 hr, >2hr with <1person/km, and >2hr with >1person/km
motorsurgperm2hr <- reclassify(popsumxmotorsurgperm,
                               matrix(c(1, 1, # <1 person/km within 1 hr  -- group 1
                                        2, 1, # <1 person/km within 1-2hrs -- group 1
                                        3, 2, # <1 person/km 2+ hrs -- group 2
                                        10, 1, # >1 person/km within 1 hr -- group 1
                                        20, 1, # >1 person/km within 1-2hrs -- group 1
                                        30, 3), # >1 person/km 2+ hrs -- group 3
                                      ncol = 2,
                                      byrow = TRUE),
                               include.lowest = F)
unique(motorsurgperm2hr) # works correctly
plot(motorsurgperm2hr)

# replacing NA's with 0
motorsurgperm2hr[is.na(motorsurgperm2hr[])] <- 0
unique(motorsurgperm2hr)
plot(motorsurgperm2hr)

# clipping to match rwanda
motorsurgperm2hr <- crop(motorsurgperm2hr, adm1)
motorsurgperm2hr <- mask(motorsurgperm2hr, adm1)
plot(motorsurgperm2hr)

# mapping
motor.surgperm.2hr.plot <- tm_shape(motorsurgperm2hr) +
  tm_raster(style = "cat",
            palette = c("grey","#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("Unpopulated area",
                       "Within 1hrs travel time to nearest facility", 
                       "<1 person/km2 who would take >2hrs to travel to nearest facility", 
                       "≥1 person/km2 who would take >2hrs to travel to nearest facility"),
            title = "Travel time to nearest eye care facility with \npermanent cataract surgical coverage") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
  tm_dots(size = 0.25,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.title.size = 1,
            legend.text.size = 0.6,
            # legend.width = 0.5,
            #legend.outside = T,
            legend.stack = "vertical",
            legend.height = 1.2,
            legend.width = 0.45) + 
  tm_layout(main.title = "Geographic access to eye care facilities with permanent cataract surgical \ncoverage",
            main.title.size = 1.2,
            main.title.position = "left")
motor.surgperm.2hr.plot
tmap_save(motor.surgperm.2hr.plot, here("results", "rwanda", "maps","pop at travel time","pop_2h_motor_surgperm.png"))


#### <1hr to a facility with temporary or permanent cataract surgical services ####
# multiplying the two rasters
unique(popsum.agg.cat)
unique(motor.surg.cat)
popsumxmotorsurg <- motor.surg.cat * popsum.agg.cat
popsumxmotorsurg
unique(popsumxmotorsurg)

# reclassifying it into the travel time and population categories
# groups: within 1 hr, >1hr with <1person/km, and >1hr with >1person/km
motorsurg1hr <- reclassify(popsumxmotorsurg,
                               matrix(c(1, 1, # <1 person/km within 1 hr  -- group 1
                                        2, 2, # <1 person/km within 1-2hrs -- group 2
                                        3, 2, # <1 person/km 2+ hrs -- group 2
                                        10, 1, # >1 person/km within 1 hr -- group 1
                                        20, 3, # >1 person/km within 1-2hrs -- group 3
                                        30, 3), # >1 person/km 2+ hrs -- group 3
                                      ncol = 2,
                                      byrow = TRUE),
                               include.lowest = F)
unique(motorsurg1hr) # works correctly
plot(motorsurg1hr)

# replacing NA's with 0
motorsurg1hr[is.na(motorsurg1hr[])] <- 0
unique(motorsurg1hr)
plot(motorsurg1hr)

# clipping to match rwanda
motorsurg1hr <- crop(motorsurg1hr, adm1)
motorsurg1hr <- mask(motorsurg1hr, adm1)
plot(motorsurg1hr)

# mapping
motor.surg.1hr.plot <- tm_shape(motorsurg1hr) +
  tm_raster(style = "cat",
            palette = c("grey","#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("Unpopulated area",
                       "Within 1hrs travel time to nearest facility", 
                       "<1 person/km2 who would take >1hrs to travel to nearest facility", 
                       "≥1 person/km2 who would take >1hrs to travel to nearest facility"),
            title = "Travel time to nearest eye care facility with \npermanent cataract surgical coverage") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent" | Cataract.surgical.services == "Occasional")) + 
  tm_dots(size = 0.25,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.title.size = 1,
            legend.text.size = 0.6,
            # legend.width = 0.5,
            #legend.outside = T,
            legend.stack = "vertical",
            legend.height = 1.2,
            legend.width = 0.45) + 
  tm_layout(main.title = "Geographic access to eye care facilities with cataract surgical coverage",
            main.title.size = 1.2,
            main.title.position = "left")
motor.surg.1hr.plot
tmap_save(motor.surg.1hr.plot, here("results", "rwanda", "maps","pop at travel time","pop_1h_motor_surg.png"))



#### <2hr to facility with any cataract surgical services ####
# using raster multiplied above
# reclassifying it into the travel time and population categories
# groups: within 2 hr, >2hr with <1person/km, and >2hr with >1person/km
motorsurg2hr <- reclassify(popsumxmotorsurg,
                               matrix(c(1, 1, # <1 person/km within 1 hr  -- group 1
                                        2, 1, # <1 person/km within 1-2hrs -- group 1
                                        3, 2, # <1 person/km 2+ hrs -- group 2
                                        10, 1, # >1 person/km within 1 hr -- group 1
                                        20, 1, # >1 person/km within 1-2hrs -- group 1
                                        30, 3), # >1 person/km 2+ hrs -- group 3
                                      ncol = 2,
                                      byrow = TRUE),
                               include.lowest = F)
unique(motorsurg2hr) # works correctly
plot(motorsurg2hr)

# replacing NA's with 0
motorsurg2hr[is.na(motorsurg2hr[])] <- 0
unique(motorsurg2hr)
plot(motorsurg2hr)

# clipping to match rwanda
motorsurg2hr <- crop(motorsurg2hr, adm1)
motorsurg2hr <- mask(motorsurg2hr, adm1)
plot(motorsurg2hr)

# mapping
motor.surg.2hr.plot <- tm_shape(motorsurg2hr) +
  tm_raster(style = "cat",
            palette = c("grey","#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("Unpopulated area",
                       "Within 1hrs travel time to nearest facility", 
                       "<1 person/km2 who would take >2hrs to travel to nearest facility", 
                       "≥1 person/km2 who would take >2hrs to travel to nearest facility"),
            title = "Travel time to nearest eye care facility with \npermanent cataract surgical coverage") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent" | Cataract.surgical.services == "Occasional")) + 
  tm_dots(size = 0.25,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.title.size = 1,
            legend.text.size = 0.6,
            # legend.width = 0.5,
            #legend.outside = T,
            legend.stack = "vertical",
            legend.height = 1.2,
            legend.width = 0.45) + 
  tm_layout(main.title = "Geographic access to eye care facilities with permanent cataract surgical \ncoverage",
            main.title.size = 1.2,
            main.title.position = "left")
motor.surg.2hr.plot
tmap_save(motor.surg.2hr.plot, here("results", "rwanda", "maps","pop at travel time","pop_2h_motor_surg.png"))

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


#### creating a table of the results ####

par <- function(x) { paste0("(",x,"%)")}

table <- full_join(table.motor.all, table.motor.surg) %>%
  full_join(.,  table.motor.surgperm) %>%
  # full_join(., table.walk.all) %>%
  # full_join(., table.walk.surg) %>%
  # full_join(., table.walk.surgperm) %>%
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

write_csv(table, here("results", "rwanda","tables","traveltime_motor.csv"))


#### zonal statistics for admin level 1 boundaries ####

# obtaining a spatial polygon corresponding to each adm1 boundary

# function
adm.split <- function(admin) {
  df <- adm1
  output <- df %>% filter(ADM1_EN == admin)
  output
}
# testing
adm.split("Eastern Province")

# iterating
# vector of admin1 levels
adm1.v <- unique(adm1$ADM1_EN)

adm1.list <- lapply(adm1.v, adm.split)

## travel time to any eye facility ##
# creating a function to apply over the admin 1 boundaries
table.travel.all <- function(adm) {
  pop <- popsum.agg
  adm <- adm
  travel <- motor.all.cat
  # masking
  pop.adm <- mask(pop, adm)
  travel.adm <- mask(travel, adm)
  # getting zonal stats
  stats <- zonal(pop.adm,
                 travel.adm,
                 fun = 'sum') %>%
    as_tibble(.) %>%
    mutate(adm = adm$ADM1_EN)
  # printing stats
  stats
}

# lapplying
table.motor.all <- lapply(adm1.list, table.travel.all) %>%
  # converting into a data frame
  plyr::ldply(., data.frame) %>% as_tibble(.) %>%
  mutate(transport = "motor",
         service = "all")
table.motor.all


## travel time to any facility with surgical capacity ##
table.travel.surg <- function(adm) {
  pop <- popsum.agg
  adm <- adm
  travel <- motor.surg.cat
  # masking
  pop.adm <- mask(pop, adm)
  travel.adm <- mask(travel, adm)
  # getting zonal stats
  stats <- zonal(pop.adm,
                 travel.adm,
                 fun = 'sum') %>%
    as_tibble(.) %>%
    mutate(adm = adm$ADM1_EN)
  # printing stats
  stats
}

# lapplying
table.motor.surg <- lapply(adm1.list, table.travel.surg) %>%
  # converting into a data frame
  plyr::ldply(., data.frame) %>% as_tibble(.) %>%
  mutate(transport = "motor",
         service = "surg")
table.motor.surg

## travel time to facilities with permanent cataract surgical capacity ##
table.travel.surgperm <- function(adm) {
  pop <- popsum.agg
  adm <- adm
  travel <- motor.surgperm.cat
  # masking
  pop.adm <- mask(pop, adm)
  travel.adm <- mask(travel, adm)
  # getting zonal stats
  stats <- zonal(pop.adm,
                 travel.adm,
                 fun = 'sum') %>%
    as_tibble(.) %>%
    mutate(adm = adm$ADM1_EN)
  # printing stats
  stats
}

# lapplying
table.motor.surgperm <- lapply(adm1.list, table.travel.surgperm) %>%
  # converting into a data frame
  plyr::ldply(., data.frame) %>% as_tibble(.) %>%
  mutate(transport = "motor",
         service = "surgperm")
table.motor.surgperm


# combining everything into a single table
table.admin1 <- full_join(table.motor.all, table.motor.surg) %>%
  full_join(., table.motor.surgperm) %>%
  pivot_wider(names_from = zone,
              values_from = sum) %>%
  rename(n_less1=`1`,
                  n_1to2=`2`,
                  n_2plus=`3`) %>%
           mutate(across(contains("n_"), round),
                  total = if_else(!is.na(n_1to2), n_less1 + n_1to2 + n_2plus, n_less1),
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
                                      service == "surgperm" ~ "Facilities with permanent surgical services")) %>%
           dplyr::select(-total) %>%
           rename(`<1 hour`=less1,
                  `1-2 hours`=oneto2,
                  `>2 hours`=twoplus,
                  Services=service,
                  Transportation=transport,
                  `Administrative zone` = adm)

table.admin1

# saving
write_csv(table.admin1, here("results", "rwanda", "tables","admin_travel_time.csv"))


#### OLD CODE -- IGNORE ####

# # walking
# compareRaster(access.walk.all, popsum.agg)
# table.walk.all <- zonal(popsum.agg,
#                         walk.all.cat,
#                         fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(service="all",
#          transport="walk")
# table.walk.all
# 
# compareRaster(access.walk.surg, popsum.agg)
# table.walk.surg <- zonal(popsum.agg,
#                          walk.surg.cat,
#                          fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(service="surg",
#          transport="walk")
# table.walk.surg
# 
# compareRaster(access.walk.surgperm, popsum.agg)
# table.walk.surgperm <- zonal(popsum.agg,
#                              walk.surgperm.cat,
#                              fun = 'sum') %>%
#   as_tibble() %>%
#   mutate(service="perm",
#          transport="walk")
# 
# table.walk.surgperm

# # adding a histogram with population by continuous travel time per kevin tang's suggestion
# 
# # taking the sum of population in each access.motor.all zone
# motor.all.hist <- zonal(popsum.agg, 
#                         access.motor.all,
#                         fun = "sum") %>%
#   as_tibble(.) %>%
#   rename(traveltime = zone,
#          pop = sum) %>%
#   # creating categorial travel time
#   mutate(traveltimecat = factor(case_when(traveltime < 60 ~ "<60",
#                                           traveltime >= 60 & traveltime < 120 ~ "60-120",
#                                           traveltime >= 120 ~ ">120"),
#                                 levels = c("<60","60-120",">120"))) %>%
#   ggplot(aes(x = traveltime)) + 
#   geom_histogram(aes( x = traveltime, weight = pop, fill = traveltimecat), # weight allows actual pop numbers to appear on y-axis
#                  breaks = c(seq(from = 0, to = 250, by = 10))) + # max(ts$traveltime)
#   scale_y_log10() + # scaling the y-axis to allow visualization of smaller proportion of the population >1hr away
#   scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33")) + 
#   theme_bw() + 
#   labs(fill = "Categorical travel \ntime (mins)",
#        x = "Motorized travel time to nearest \neye care facility (mins)",
#        y = "Log population 50+")
# motor.all.hist
# 
# # saving
# ggsave(here("results", "rwanda", "maps", "motor_all_hist.eps"), motor.all.hist)

# # bar plot but with zoom in on the smaller values
# motor.surg.hist <- zonal(popsum.agg, 
#                          access.motor.surg,
#                          fun = "sum") %>%
#   as_tibble(.) %>%
#   rename(traveltime = zone,
#          pop = sum) %>%
#   # creating categorial travel time
#   mutate(traveltimecat = factor(case_when(traveltime < 60 ~ "<60",
#                                           traveltime >= 60 & traveltime < 120 ~ "60-120",
#                                           traveltime >= 120 ~ ">120"),
#                                 levels = c("<60","60-120",">120"))) %>%
#   ggplot(aes(x = traveltime)) + 
#   geom_histogram(aes( x = traveltime, weight = pop, fill = traveltimecat), # weight allows actual pop numbers to appear on y-axis
#                  breaks = c(seq(from = 0, to = 490, by = 10))) + # max(ts$traveltime)
#   scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33")) + 
#   theme_bw() + 
#   labs(fill = "Categorical travel \ntime (mins)",
#        x = "Motorized travel time to nearest \neye care facility (mins)",
#        y = "Population 50+")
# motor.surg.hist
# 
# # saving
# ggsave(here("results", "rwanda", "maps", "motor_surg_hist_large.eps"), motor.surg.hist)
# 
# # now getting graph just with the smaller values
# motor.surg.hist.small <- zonal(popsum.agg, 
#                                access.motor.surg,
#                                fun = "sum") %>%
#   as_tibble(.) %>%
#   rename(traveltime = zone,
#          pop = sum) %>%
#   # creating categorial travel time
#   mutate(traveltimecat = factor(case_when(traveltime < 60 ~ "<60",
#                                           traveltime >= 60 & traveltime < 120 ~ "60-120",
#                                           traveltime >= 120 ~ ">120"),
#                                 levels = c("<60","60-120",">120"))) %>%
#   filter(traveltimecat != "<60") %>%
#   ggplot(aes(x = traveltime)) + 
#   geom_histogram(aes( x = traveltime, weight = pop, fill = traveltimecat), # weight allows actual pop numbers to appear on y-axis
#                  breaks = c(seq(from = 0, to = 490, by = 10))) + # max(ts$traveltime)
#   scale_fill_manual(values = c("#fdbb84","#e34a33")) + 
#   theme_bw() + 
#   labs(fill = "Categorical travel \ntime (mins)",
#        x = "Motorized travel time to nearest \neye care facility (mins)",
#        y = "Population 50+") 
# motor.surg.hist.small
# ggsave(here("results", "rwanda", "maps", "motor_surg_hist_small.eps"), motor.surg.hist.small)
## walking
# summary(access.walk.all)
# 
# # plot histogram of the data
# hist(access.walk.all)
# 
# # reclassifying raster
# walk.all.cat <- reclassify(access.walk.all,
#                            reclass.mat,
#                            include.lowest = TRUE)
# 
# # checking work
# barplot(walk.all.cat)
# summary(walk.all.cat)
# 
# # plotting 
# walk.all.plot <- tm_shape(walk.all.cat) +
#   tm_raster(style = "cat",
#             palette = c("#fee8c8","#fdbb84","#e34a33"),
#             # colorNA = "grey",
#             labels = c("<60", "60-120", ">120"),
#             title = "Travel time (mins)") + 
#   tm_shape(eye_fac_sf) + 
#   tm_dots(size = 0.05,
#           col = "blue",
#           labels = "Eye care facilities") +
#   tm_shape(adm1) + 
#   tm_borders(lwd = 1.5, col = "black") + 
#   tm_legend(legend.position = c("right","bottom"),
#             legend.stack = "vertical") + 
#   tm_layout(main.title = "Walking travel time to nearest eye care facility",
#             main.title.size = 1,
#             main.title.position = "left")
# walk.all.plot 
# tmap_save(walk.all.plot , here("results","rwanda" ,"maps","walk_all.png"))
# summary(access.walk.surg)
# hist(access.walk.surg)
# 
# walk.surg.cat <- reclassify(access.walk.surg,
#                             reclass.mat,
#                             include.lowest = T)
# # checking my work
# # barplot(walk.surg.cat)
# # plotting the raster
# walk.surg.plot <- tm_shape(walk.surg.cat) +
#   tm_raster(style = "cat",
#             palette = c("#fee8c8","#fdbb84","#e34a33"),
#             # colorNA = "grey",
#             labels = c("<60", "60-120", ">120"),
#             title = "Travel time (mins)") + 
#   tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services != "None")) + 
#   tm_dots(size = 0.05,
#           col = "blue",
#           labels = "Eye care facilities") +
#   tm_shape(adm1) + 
#   tm_borders(lwd = 1.5, col = "black") + 
#   tm_legend(legend.position = c("right","bottom"),
#             legend.stack = "vertical") + 
#   tm_layout(main.title = "Walking travel time to nearest eye care facility\nwith cataract surgical services",
#             main.title.size = 1,
#             main.title.position = "left")
# walk.surg.plot 
# tmap_save(walk.surg.plot , here("results","rwanda" ,"maps","walk_surg.png"))
# 
# 
# ## motor permanent surgical services
# summary(access.walk.surgperm)
# hist(access.walk.surgperm)
# 
# walk.surgperm.cat <-  reclassify(access.walk.surgperm,
#                                  reclass.mat,
#                                  include.lowest = T)
# # barplot(walk.surgperm.cat)
# # plotting the raster
# walk.surgperm.plot <- tm_shape(walk.surgperm.cat) +
#   tm_raster(style = "cat",
#             palette = c("#fee8c8","#fdbb84","#e34a33"),
#             # colorNA = "grey",
#             labels = c("<60", "60-120", ">120"),
#             title = "Travel time (mins)") + 
#   tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
#   tm_dots(size = 0.05,
#           col = "blue",
#           labels = "Eye care facilities") +
#   tm_shape(adm1) + 
#   tm_borders(lwd = 1.5, col = "black") + 
#   tm_legend(legend.position = c("right","bottom"),
#             legend.stack = "vertical") + 
#   tm_layout(main.title = "Walking travel time to nearest eye care facility\nwith permanent cataract surgical services",
#             main.title.size = 1,
#             main.title.position = "left")
# walk.surgperm.plot 
# tmap_save(walk.surgperm.plot  , here("results", "rwanda","maps","walk_surgperm.png"))


