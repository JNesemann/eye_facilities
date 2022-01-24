# obtaining the proportion of individuals 50+ at certain travel times from facilities

#### packages ####
library(tidyverse)
library(here)
library(sf)
library(wpgpDownloadR)
library(raster)
library(sp)
library(rgdal)
library(viridis)
library(tmap)

#### data ####
# list of rasters for eye care facility access generated in eye_facilities.Rmd

# all hc facilities
access.raster.all <- raster(here("data","rasters","traveltimes","access_raster_all.tif"))

# eye care facilities with cataract surgical capacity
access.raster.surgery <- raster(here("data","rasters","traveltimes","access_raster_surgery.tif"))

# with permanent cataract surgical capacity
access.raster.surgery.perm <- raster(here("data","rasters","traveltimes","access_raster_surgery_perm.tif"))

# list of population over 50 raster (code taken from world_pop.Rmd)
popsum <- raster(here("data", "rasters", "popsum_constrained.tif"))

# malawi shape file
malawi <- st_read(here("data","mwi_adm_nso_20181016_shp.nosync", "mwi_admbnda_adm2_nso_20181016.shp"))

# cropping so the extents match
access.raster.all <- crop(access.raster.all, malawi)
access.raster.surgery <- crop(access.raster.surgery, malawi)
access.raster.surgery.perm <- crop(access.raster.surgery.perm, malawi)
popsum <- crop(popsum, malawi)

extent(access.raster.all)
extent(access.raster.surgery)
extent(access.raster.surgery.perm)
extent(popsum)

#### STEP 1 -- reclassify the raster  ####
# tutorial from: https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/

# trying it with access.raster.all
summary(access.raster.all)
# lots of NA's -- I wonder if these are values external to the borders of Malawi

# plot histogram of the data
hist(access.raster.all)

# histogram with travel time breaks of interest
hist(access.raster.all,
     breaks = c(0, 60, 120, 530))

# creating a reclassification matrix
reclass <- c(0, 60, 1, # within an hour
                 60, 120, 2, # within two hours
                 120, Inf, 3) # greater than 2 hours
reclass.mat <- matrix(reclass,
                      ncol = 3,
                      byrow = T)
reclass.mat

# reclassifying raster
access.all.cat <- reclassify(access.raster.all,
                         reclass.mat)

# checking work
barplot(access.all.cat)
# no 0s so no need to reclassify everything

summary(access.all.cat)

# plotting the raster
plot(access.all.cat,
     legend = F,
     col = c("green","blue","red"), 
     axes = F,
     box = F,
     main = "Grouped travel time to nearest eye care facility")

legend("topright",
       legend = c("<1hr","1-2hrs",">2hrs"),
       fill = c("green","blue","red"),
       border = F,
       bty = "n")

# looks good, mostly >1hr travel time

#### STEP 2 -- zonal statistics using the two rasters ####

# ensure both use the same CRS
compareRaster(access.all.cat, popsum)
# different extents, seems like popsum is slightly smaller

# cropping 
popsum.malawi <- crop(popsum, malawi)
compareRaster(popsum.malawi, access.all.cat) # still different extents

# converting to same extent using resample
access.all.resamp <- resample(access.all.cat, popsum, method = "bilinear")
compareRaster(popsum, access.all.resamp) #works

# getting pop sum for each category
zonal(popsum,
      access.all.resamp,
      fun = sum) # 0 is NA so ignore, about 3k people in this group.
# this is likely due to slightly different extent of each raster 
# maybe keving can help fix this since he created the friction surface

# repeating using vectors to compare
# converting the access raster to vector
# access.all.poly <- rasterToPolygons(access.all.resamp, 
#                                     dissolve = T) # combines neighboring pixel with same value
# 
# # comparing crs
# crs(access.all.poly)
# crs(popsum)
# extent(access.all.poly)
# extent(popsum.resamp)
# 
# # getting the sum for each 
# extract(popsum.resamp, access.all.poly, 
#         fun = sum, 
#         na.rm=T)
# 
# # comparing
# zonal(popsum.resamp,
#       access.all.cat,
#       fun = sum)
# exactly the same, the raster version was faster so will do that for the others.

#### repeating for facilities with surgical capacity ####
summary(access.raster.surgery)
hist(access.raster.surgery)

# reclassifying
access.surg.cat <- reclassify(access.raster.surgery,
                              reclass.mat) # using same reclassification matrix
barplot(access.surg.cat)
# plotting the raster
plot(access.surg.cat,
     legend = F,
     col = c("green","blue","red"), 
     axes = F,
     box = F,
     main = "Grouped travel time to nearest eye care facility")
legend("topright",
       legend = c("<1hr","1-2hrs",">2hrs"),
       fill = c("green","blue","red"),
       border = F,
       bty = "n")
# more folks with less access to facilities with surgical capacity

# converting to same extent using resample
access.surg.resamp <- resample(access.surg.cat, popsum, method = "bilinear")
compareRaster(popsum, access.surg.resamp) #works

# obtaining the zonal statistics
zonal(popsum,
      access.surg.resamp,
      fun = sum)

#### repeating for facilities with permanent surgical capacity ####
summary(access.raster.surgery.perm)
hist(access.raster.surgery.perm)

# reclassifying
access.surgperm.cat <- reclassify(access.raster.surgery.perm,
                              reclass.mat) # using same reclassification matrix
barplot(access.surgperm.cat)
# plotting the raster
plot(access.surgperm.cat,
     legend = F,
     col = c("green","blue","red"), 
     axes = F,
     box = F,
     main = "Grouped travel time to nearest eye care facility")
legend("topright",
       legend = c("<1hr","1-2hrs",">2hrs"),
       fill = c("green","blue","red"),
       border = F,
       bty = "n")
# large swath in the north now >2 hrs

# converting to same extent using resample
access.surgperm.resamp <- resample(access.surgperm.cat, popsum, method = "bilinear")
compareRaster(popsum, access.surgperm.resamp) #works

# obtaining the zonal statistics
zonal(popsum,
      access.surgperm.resamp,
      fun = sum)

#### creating a table of the results ####

# results of each level 
results.all <- zonal(popsum,
                     access.all.resamp,
                     fun = sum) %>% 
  as_tibble() %>%
  rename(all=value)

results.surg <- zonal(popsum,
                      access.surg.resamp,
                      fun = sum) %>% 
  as_tibble() %>%
  rename(surg=value)

results.perm <- zonal(popsum,
                      access.surgperm.resamp,
                      fun = sum) %>% 
  as_tibble() %>%
  rename(perm=value)

# cellStats(popsum, sum) # total pop of 1505583
round <- function(x) { base::round(x, digits = 1)}
par <- function(x) { paste0("(",x,"%)")}

table <- full_join(results.all, results.surg) %>%
  full_join(., results.perm) %>%
  pivot_longer(all:perm, 
               names_to = "services",
               values_to = "number") %>%
  pivot_wider(names_from = "zone",
              values_from = "number") %>%
  rename(n_missing=`0`,
         n_less1=`1`,
         n_onetotwo=`2`,
         n_over2=`3`) %>%
  mutate(total = n_missing + n_less1 + n_onetotwo + n_over2,
         p_less1 = n_less1 / total *100,
         p_missing = n_missing / total * 100,
         p_onetotwo = n_onetotwo / total * 100,
         p_over2 = n_over2 / total * 100) %>%
  mutate(across(n_missing:p_over2, round),
         across(contains("p_"), par)) %>%
  unite(less1, n_less1, p_less1, sep = " ") %>%
  unite(oneto2, n_onetotwo, p_onetotwo, sep = " ") %>%
  unite(over2, n_over2, p_over2, sep = " ") %>%
  unite(missing, n_missing, p_missing, sep = " ") %>%
  mutate(services=case_when(services == "all" ~ "All eye care facilities",
                            services == "surg" ~ "Facilities with surgical services",
                            services == "perm" ~ "Facilities with permanent surgical services")) %>%
  dplyr::select(-total) %>%
  rename(Missing=missing,
         `<1 hour`=less1,
         `1-2 hours`=oneto2,
         `>2 hours`=over2,
         Services=services)

table

write_csv(table, here("results","tables","traveltime.csv"))

