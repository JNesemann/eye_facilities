# generating motorized and walking friction surfaces for malawi

#### installing packages ####
library(raster)
library(here)
library(tidyverse)
library(sf)
library(malariaAtlas)

#### data ####

# friction surfaces
world.motor <- raster(here("data","frictionsurfaces","2020_motorized_friction_surface.geotiff"))
world.walk <- raster(here("data","frictionsurfaces","2020_walking_only_friction_surface.geotiff"))

# shape for clipping the raster
malawi_clip <- readRDS(here("data","malawi","popsum_sf.rds"))

# eye facilities data
eye_fac <- read.csv(here("data", "malawi", "Malawi_facilities_public_gps.csv"))
eye_fac_sf <- eye_fac %>% st_as_sf(coords = c("Long", "Lat"), crs=4326)
eye_fac_sf_t <- st_transform(eye_fac_sf, crs=2163)

#### raster operations ####

# clipping the FS so it is just Malawi

# motorized transport
malawi.motor <- crop(world.motor, malawi_clip)
malawi.motor <- mask(malawi.motor, malawi_clip)
malariaAtlas::autoplot_MAPraster(malawi.motor)

# walking
malawi.walk <- crop(world.walk, malawi_clip)
malawi.walk <- mask(malawi.walk, malawi_clip)
malariaAtlas::autoplot_MAPraster(malawi.walk)


#### convert friction surface to transition matrix ####

# motor
T <- gdistance::transition(malawi.motor, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T) # this corrects distance for curvature of earth

# walking 
T.walk <- gdistance::transition(malawi.walk, function(x) 1/mean(x), 8) 
T.GC.walk <- gdistance::geoCorrection(T.walk)


#### preparing point locations ####

# all eye care facilities
point.locations.all <- eye_fac %>% dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services)
names(point.locations.all) <- c("X_COORD", "Y_COORD", "name", "cataract_surgical_services")
coordinates(point.locations.all) <- ~ X_COORD + Y_COORD #Keep only point coordinates within the shapefile bounds
points.all <- as.matrix(point.locations.all@coords) #convert to matrix of lat-longs for accessibility algorithm

# facilities with cataract surgery capacity
point.locations.surgery <- eye_fac %>% dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services)
names(point.locations.surgery) <- c("X_COORD", "Y_COORD", "name", "cataract_surgical_services")
point.locations.surgery <- point.locations.surgery %>% filter(cataract_surgical_services!="None")
coordinates(point.locations.surgery) <- ~ X_COORD + Y_COORD #Keep only point coordinates within the shapefile bounds
points.surgery <- as.matrix(point.locations.surgery@coords) #convert to matrix of lat-longs for accessibility algorithm

# facilities with permanent surgical capacity
point.locations.surgery.perm <- eye_fac %>% dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services)
names(point.locations.surgery.perm) <- c("X_COORD", "Y_COORD", "name", "cataract_surgical_services")
point.locations.surgery.perm <- point.locations.surgery.perm %>% filter(cataract_surgical_services=="Permanent")
coordinates(point.locations.surgery.perm) <- ~ X_COORD + Y_COORD #Keep only point coordinates within the shapefile bounds
points.surgery.perm <- as.matrix(point.locations.surgery.perm@coords) #convert to matrix of lat-longs for accessibility algorithm


#### accumulated cost surface algorithm ####

## motorized
# all eye care facilities
access.motor.all <- gdistance::accCost(T.GC, points.all) 
p <- malariaAtlas::autoplot_MAPraster(access.motor.all, shp_df=malawi_clip, printed=F)

p[[1]] + 
  geom_point(data=data.frame(point.locations.all@coords), aes(x=X_COORD, y=Y_COORD)) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0, 
                                                   start=1.5, 
                                                   r=-1.0, 
                                                   hue=1.5, 
                                                   n=16)), 
                       name="Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Eye Care Facility") +
  theme(axis.text=element_blank(), panel.border=element_rect(fill=NA, color="white"))

# facilities with cataract surgical capacities
access.motor.surg <- gdistance::accCost(T.GC, points.surgery) 
p <- malariaAtlas::autoplot_MAPraster(access.motor.surg, shp_df=malawi_clip, printed=F)

p[[1]] + 
  geom_point(data=data.frame(point.locations.surgery@coords), aes(x=X_COORD, y=Y_COORD)) +
  # geom_sf(data = malawi, fill = NA) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0, 
                                                   start=1.5, 
                                                   r=-1.0, 
                                                   hue=1.5, 
                                                   n=16)), 
                       name="Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Eye Care Facility") +
  theme(axis.text=element_blank(), panel.border=element_rect(fill=NA, color="white"))

# facilities with permanent cataract surgical capacity
access.motor.surgperm <- gdistance::accCost(T.GC, points.surgery.perm) 
p <- malariaAtlas::autoplot_MAPraster(access.motor.surgperm, shp_df=malawi_clip, printed=F)

p[[1]] + 
  geom_point(data = data.frame(point.locations.surgery.perm@coords), aes(x = X_COORD, y = Y_COORD)) +
  # geom_sf(data = malawi, fill = NA) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma = 1.0, 
                                                   start = 1.5, 
                                                   r = -1.0, 
                                                   hue = 1.5, 
                                                   n = 16)), 
                       name = "Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Eye Care Facility") +
  theme(axis.text = element_blank(), panel.border = element_rect(fill = NA, color = "white"))


## walking
# all eye care facilities
access.walk.all <- gdistance::accCost(T.GC.walk, points.all) 
p <- malariaAtlas::autoplot_MAPraster(access.walk.all, shp_df=malawi_clip, printed=F)

p[[1]] + 
  geom_point(data=data.frame(point.locations.all@coords), aes(x=X_COORD, y=Y_COORD)) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0, 
                                                   start=1.5, 
                                                   r=-1.0, 
                                                   hue=1.5, 
                                                   n=16)), 
                       name="Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Eye Care Facility") +
  theme(axis.text=element_blank(), panel.border=element_rect(fill=NA, color="white"))

# facilities with cataract surgical capacities
access.walk.surg <- gdistance::accCost(T.GC.walk, points.surgery) 
p <- malariaAtlas::autoplot_MAPraster(access.walk.surg, shp_df=malawi_clip, printed=F)

p[[1]] + 
  geom_point(data=data.frame(point.locations.surgery@coords), aes(x=X_COORD, y=Y_COORD)) +
  # geom_sf(data = malawi, fill = NA) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0, 
                                                   start=1.5, 
                                                   r=-1.0, 
                                                   hue=1.5, 
                                                   n=16)), 
                       name="Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Eye Care Facility") +
  theme(axis.text=element_blank(), panel.border=element_rect(fill=NA, color="white"))

# facilities with permanent cataract surgical capacity
access.walk.surgperm <- gdistance::accCost(T.GC.walk, points.surgery.perm) 
p <- malariaAtlas::autoplot_MAPraster(access.walk.surgperm, shp_df=malawi_clip, printed=F)

p[[1]] + 
  geom_point(data = data.frame(point.locations.surgery.perm@coords), aes(x = X_COORD, y = Y_COORD)) +
  # geom_sf(data = malawi, fill = NA) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma = 1.0, 
                                                   start = 1.5, 
                                                   r = -1.0, 
                                                   hue = 1.5, 
                                                   n = 16)), 
                       name = "Minutes \n of Travel") + 
  ggtitle("Travel Time to Most Accessible Eye Care Facility") +
  theme(axis.text = element_blank(), panel.border = element_rect(fill = NA, color = "white"))


#### saving the rasters ####

# motor
writeRaster(access.motor.surgperm, here("data","malawi","rasters","traveltimes", "motor travel time",
                                             "access_motor_surgperm.tif"), overwrite=TRUE)

writeRaster(access.motor.surg, here("data","malawi","rasters","traveltimes","motor travel time",
                                        "access_motor_surg.tif"), overwrite=TRUE)

writeRaster(access.motor.all, here("data","malawi","rasters","traveltimes","motor travel time",
                                    "access_motor_all.tif"), overwrite=TRUE)

# walking
writeRaster(access.walk.all, here("data","malawi","rasters","traveltimes","walk travel time",
                                  "access_walk_all.tif"), overwrite=TRUE)

writeRaster(access.walk.surg, here("data","malawi","rasters","traveltimes","walk travel time",
                                   "access_walk_surg.tif"), overwrite=TRUE)

writeRaster(access.walk.surgperm, here("data","malawi","rasters","traveltimes","walk travel time",
                                       "access_walk_surgperm.tif"), overwrite=TRUE)
