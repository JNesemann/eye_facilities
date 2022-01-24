# creating new malawi shapefile to use for friction surface
# this is to ensure 100% overlap between the friction surface and the population raster

# libraries
library(raster)
library(here)
library(sp)
library(terra)
library(sf)

# will use the uncontrained raster as this has values for all pixels
popsum.uncon <- rast(here("data", "rasters", "popsum_unconstrained.tif"))
plot(popsum.uncon)
popsum.poly <- as.polygons(popsum.uncon > -Inf)
plot(popsum.poly)
class(popsum.poly)

# converting to SpatialPoints object
popsum.sp <- as(popsum.poly, "Spatial")
class(popsum.sp)
class(popsum.sp)

# saving
saveRDS(popsum.sp, here("data","popsum_sf.rds"))