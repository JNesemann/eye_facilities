# Rwanda travel time analysis

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
rwanda_clip <- st_read(here("data", "rwanda","rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync", 
                       "rwa_adm0_2006_NISR_WGS1984_20181002.shp")) 

# eye facilities data
eye_fac <- read.csv(here("data", "rwanda", "Rwanda_facilities_gps.csv"))
eye_fac_sf <- eye_fac %>% st_as_sf(coords = c("Long", "Lat"), crs=4326)
# creating a transition matrix
eye_fac_sf_t <- st_transform(eye_fac_sf, crs=2163)


#### raster operations ####

# clipping the FS so it is just rwanda

# motorized transport
# croping
rwanda.motor <- crop(world.motor, rwanda_clip)
# masking
rwanda.motor <- mask(rwanda.motor, rwanda_clip)
malariaAtlas::autoplot_MAPraster(rwanda.motor) # looks quite homogenous

#### convert friction surface to transition matrix ####

# motor
T <- gdistance::transition(rwanda.motor, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T) # this corrects distance for curvature of earth


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
plot(access.motor.all)

# facilities with cataract surgical capacities
access.motor.surg <- gdistance::accCost(T.GC, points.surgery) 
plot(access.motor.surg)

# facilities with permanent cataract surgical capacity
access.motor.surgperm <- gdistance::accCost(T.GC, points.surgery.perm) 
plot(access.motor.surgperm)

#### saving the rasters ####

# motor
writeRaster(access.motor.surgperm, here("data", "rwanda", "rasters","traveltimes", "motor travel time",
                                        "access_motor_surgperm.tif"), overwrite=TRUE)

writeRaster(access.motor.surg, here("data", "rwanda","rasters","traveltimes","motor travel time",
                                    "access_motor_surg.tif"), overwrite=TRUE)

writeRaster(access.motor.all, here("data", "rwanda", "rasters","traveltimes","motor travel time",
                                   "access_motor_all.tif"), overwrite=TRUE)
