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


# walking travel times
access.walk.all <- raster(here("data", "rwanda", "rasters","traveltimes","walk travel time",
                               "access_walk_all.tif"))

access.walk.surg <- raster(here("data", "rwanda", "rasters","traveltimes","walk travel time",
                                "access_walk_surg.tif"))

access.walk.surgperm <- raster(here("data", "rwanda", "rasters","traveltimes","walk travel time",
                                    "access_walk_surgperm.tif"))


# population over 50 raster
popsum <- raster(here("data", "rwanda", "rasters", "popsum_constrained.tif"))


# admin 1 boundaries
adm1 <- st_read(here("data", "rwanda", "rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync", "rwa_adm1_2006_NISR_WGS1984_20181002.shp"))


# location of eye facilities for plotting
eye_fac <- read.csv(here("data", "rwanda", "Rwanda_facilities_gps.csv"))
eye_fac_sf <- eye_fac %>% 
  st_as_sf(coords = c("Long", "Lat"), 
           crs=4326)


#### matching resolution and extent of all rasters ####

# aggregating popsum
popsum.agg <- raster::aggregate(popsum, 
                                fact = 10, 
                                fun = sum, 
                                dissolve = T,
                                expand = T)

compareRaster(popsum.agg, access.motor.all, resolution = T, extent = F, rowcol = F) # same resolution

# now I have to match extent of popsum.agg and the travel time rasters
access.motor.all.resamp <- resample(access.motor.all, 
                                    popsum.agg,
                                    method = "ngb")

# comparing
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

tmap_save(motor.all.plot, here("results", "rwanda", "maps","motor_all.png"))

# adding a histogram with population by continuous travel time per kevin tang's suggestion

# taking the sum of population in each access.motor.all zone
motor.all.hist <- zonal(popsum.agg, 
      access.motor.all,
      fun = "sum") %>%
  as_tibble(.) %>%
  rename(traveltime = zone,
         pop = sum) %>%
  # creating categorial travel time
  mutate(traveltimecat = factor(case_when(traveltime < 60 ~ "<60",
                                   traveltime >= 60 & traveltime < 120 ~ "60-120",
                                   traveltime >= 120 ~ ">120"),
                                levels = c("<60","60-120",">120"))) %>%
  ggplot(aes(x = traveltime)) + 
  geom_histogram(aes( x = traveltime, weight = pop, fill = traveltimecat), # weight allows actual pop numbers to appear on y-axis
                 breaks = c(seq(from = 0, to = 250, by = 10))) + # max(ts$traveltime)
  scale_y_log10() + # scaling the y-axis to allow visualization of smaller proportion of the population >1hr away
  scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33")) + 
  theme_bw() + 
  labs(fill = "Categorical travel \ntime (mins)",
       x = "Motorized travel time to nearest \neye care facility (mins)",
       y = "Log population 50+")
motor.all.hist

# saving
ggsave(here("results", "rwanda", "maps", "motor_all_hist.eps"), motor.all.hist)


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
tmap_save(motor.surg.plot, here("results", "rwanda", "maps","motor_surg.png"))

# bar plot but with zoom in on the smaller values
motor.surg.hist <- zonal(popsum.agg, 
                        access.motor.surg,
                        fun = "sum") %>%
  as_tibble(.) %>%
  rename(traveltime = zone,
         pop = sum) %>%
  # creating categorial travel time
  mutate(traveltimecat = factor(case_when(traveltime < 60 ~ "<60",
                                          traveltime >= 60 & traveltime < 120 ~ "60-120",
                                          traveltime >= 120 ~ ">120"),
                                levels = c("<60","60-120",">120"))) %>%
  ggplot(aes(x = traveltime)) + 
  geom_histogram(aes( x = traveltime, weight = pop, fill = traveltimecat), # weight allows actual pop numbers to appear on y-axis
                 breaks = c(seq(from = 0, to = 490, by = 10))) + # max(ts$traveltime)
  scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33")) + 
  theme_bw() + 
  labs(fill = "Categorical travel \ntime (mins)",
       x = "Motorized travel time to nearest \neye care facility (mins)",
       y = "Population 50+")
motor.surg.hist

# saving
ggsave(here("results", "rwanda", "maps", "motor_surg_hist_large.eps"), motor.surg.hist)

# now getting graph just with the smaller values
motor.surg.hist.small <- zonal(popsum.agg, 
                               access.motor.surg,
                               fun = "sum") %>%
  as_tibble(.) %>%
  rename(traveltime = zone,
         pop = sum) %>%
  # creating categorial travel time
  mutate(traveltimecat = factor(case_when(traveltime < 60 ~ "<60",
                                          traveltime >= 60 & traveltime < 120 ~ "60-120",
                                          traveltime >= 120 ~ ">120"),
                                levels = c("<60","60-120",">120"))) %>%
  filter(traveltimecat != "<60") %>%
  ggplot(aes(x = traveltime)) + 
  geom_histogram(aes( x = traveltime, weight = pop, fill = traveltimecat), # weight allows actual pop numbers to appear on y-axis
                 breaks = c(seq(from = 0, to = 490, by = 10))) + # max(ts$traveltime)
  scale_fill_manual(values = c("#fdbb84","#e34a33")) + 
  theme_bw() + 
  labs(fill = "Categorical travel \ntime (mins)",
       x = "Motorized travel time to nearest \neye care facility (mins)",
       y = "Population 50+") 
motor.surg.hist.small
ggsave(here("results", "rwanda", "maps", "motor_surg_hist_small.eps"), motor.surg.hist.small)


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
# tmap_save(motor.surgperm.plot, here("results", "rwanda", "maps","motor_surgperm.png"))

# attempting to get population density per grid, per the MAP the resolution should be 1km x 1km
# need to reclassify as <1 and >=1 person per km2

popsum.reclass <- c(0, 1, 1, # less than 1 person per km2
                    1, Inf, 2) # > 1 person per km2

popsum.reclass.mat <- matrix(popsum.reclass,
                             ncol = 3,
                             byrow = T)

summary(popsum.agg)
hist(popsum.agg)
hist(popsum.agg,
     breaks = c(0,1,2000))

# reclassifying raster
popsum.agg.cat <- reclassify(popsum.agg,
                             popsum.reclass.mat,
                             include.lowest = T)
popsum.agg.cat
plot(popsum.agg.cat, 
     col = c("green","red"))
plot(popsum.agg)

# multiplying the two rasters
unique(popsum.agg.cat)
unique(motor.surgperm.cat)
popsumxmotorsurgperm <- motor.surgperm.cat * popsum.agg.cat
popsumxmotorsurgperm
unique(popsumxmotorsurgperm)

# reclassifying it into the travel time and population categories
motorsurgperm3cat <- reclassify(popsumxmotorsurgperm,
                                matrix(c(1, 1, # <1/km2 <1hr -- group 1 = within 2 hrs
                                       2, 1, # <1/km2 1-2hrs or >=1/km2 <1hr  -- group 1 = within 2 hrs
                                       3, 2, # <1/km2 >2hrs -- group 2 <1person/km2 at >2hrs
                                       6, 3, # >=1/km2 at >2hrs -- group 3 >1person/km2 at >2hrs
                                       4, 1), # >=1/km2 at 1-2hrs  -- group 1 = within 2 hrs
                                       ncol = 2,
                                       byrow = T),
                                include.lowest = F)
unique(motorsurgperm3cat) # works correctly
plot(motorsurgperm3cat)

# replacing NA's with 0
motorsurgperm3cat[is.na(motorsurgperm3cat[])] <- 0
unique(motorsurgperm3cat)
plot(motorsurgperm3cat)

# clipping to match rwanda
motorsurgperm3cat <- crop(motorsurgperm3cat, adm1)
motorsurgperm3cat <- mask(motorsurgperm3cat, adm1)
plot(motorsurgperm3cat)

# mapping
motor.surgperm.plot <- tm_shape(motorsurgperm3cat) +
  tm_raster(style = "cat",
            palette = c("grey","#fee8c8","#fdbb84","#e34a33"),
            # colorNA = "grey",
            labels = c("Unpopulated area",
                       "Within 2hrs travel time to nearest facility", 
                       "<1 person/km2 who would take >2hrs to travel to nearest facility", 
                       "≥1 person/km2 who would take >2hrs to travel to nearest facility"),
            title = "Travel time to nearest eye care facility with \npermanent cataract surgical coverage") + 
  tm_shape(eye_fac_sf %>% filter(Cataract.surgical.services == "Permanent")) + 
  tm_dots(size = 0.05,
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
  tm_layout(main.title = "Geographic access to eye care facilities with permanent cataract surgical coverage",
            main.title.size = 1.2,
            main.title.position = "left")
motor.surgperm.plot 
tmap_save(motor.surgperm.plot, here("results", "rwanda", "maps","motor_surgperm.png"))


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
            labels = c("<60", "60-120", ">120"),
            title = "Travel time (mins)") + 
  tm_shape(eye_fac_sf) + 
  tm_dots(size = 0.05,
          col = "blue",
          labels = "Eye care facilities") +
  tm_shape(adm1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_legend(legend.position = c("right","bottom"),
            legend.stack = "vertical") + 
  tm_layout(main.title = "Walking travel time to nearest eye care facility",
            main.title.size = 1,
            main.title.position = "left")
walk.all.plot 
tmap_save(walk.all.plot , here("results","rwanda" ,"maps","walk_all.png"))

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
  tm_layout(main.title = "Walking travel time to nearest eye care facility\nwith cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
walk.surg.plot 
tmap_save(walk.surg.plot , here("results","rwanda" ,"maps","walk_surg.png"))


## motor permanent surgical services
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
  tm_layout(main.title = "Walking travel time to nearest eye care facility\nwith permanent cataract surgical services",
            main.title.size = 1,
            main.title.position = "left")
walk.surgperm.plot 
tmap_save(walk.surgperm.plot  , here("results", "rwanda","maps","walk_surgperm.png"))


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

write_csv(table, here("results", "rwanda","tables","traveltime_motorwalk.csv"))






