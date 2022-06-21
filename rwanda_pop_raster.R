# Rwanda population raster

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

#### downloading pop data ####

# creating list of rasters to download
raster.list <- wpgpListCountryDatasets(ISO3 = "RWA") %>%
  # filtering to select the specific covariates we want (i.e., males and females 50+ yoa)
  filter(grepl("agesex_", Covariate)) %>%
  filter(grepl("_50", Covariate) | grepl("_55", Covariate) | grepl("_60", Covariate) |
           grepl("_65", Covariate) | grepl("_70", Covariate) | grepl("_75", Covariate) |
           grepl("_80", Covariate)) %>%
  filter(grepl("_2020_constrained$", Covariate)) %>%
  dplyr::select(Covariate)

raster.list

# turning the list into a vector
raster.list.v <- raster.list$Covariate

# building a function to download all of the rasters of interest
get.raster <- function(x, dir, ISO3) {
  
  # setting ISO
  ISO3 <- ISO3
  
  # setting destination directory
  destDir <- dir
  
  # getting a raster
  raster <- wpgpGetCountryDataset(ISO3 = ISO3,
                                  covariate = x,
                                  destDir = destDir)
  # returning the results
  raster
  
}

# # testing the function
# get.raster("agesex_f_55_2020_constrained", 
#            here("data","rwanda", "pop_constrained"), 
#            ISO3 = "RWA")   # works for a single covariate

# applying it over all the desired covariates
sapply(X = raster.list.v,  # the covariates we want rasters for
       dir = here("data","rwanda", "pop_constrained"),
       ISO3 = "RWA",
       FUN = get.raster,   # the function to apply
       simplify = T)

    ## NOTE - NO NEED TO RE-RUN THE ABOVE CODE EVERY TIME. 
    ## IF YOU HAVE ALREADY DOWNLOADED THE RASTERS YOU CAN SKIP TO THE SECTION BELOW

#### stacking the rasters ####
#### repeating the process for all of the downloaded rasters ####

# getting a list of all filenames
list <- list.files(here("data","rwanda", "pop_constrained"), 
                   full.names = TRUE)

# creating the stack
popstack <- stack(list)
popstack

# giving the stack more intelligible names
names(popstack) <- c("f_50","f_55","f_60","f_65","f_70", "f_75", "f_80",
                     "m_50","m_55","m_60","m_65","m_70", "m_75", "m_80")
popstack

# calculating a new raster which is the sum of the population values in all the stacks
popsum <- calc(popstack, fun=sum)

# renaming the newly created layer
names(popsum) <- "total_pop"

# saving the newly created raster
# writeRaster(popsum, here("data", "rwanda", "rasters", "popsum_constrained.tif"))


#### plot resulting raster with surgical care facilities ####

# Opening the tiff file created above (can do this instead of downloading and merging everything everytime you run the code)
popsum <- raster(here("data", "rwanda", "rasters", "popsum_constrained.tif"))

# loading rwanda shapefile
rwanda <- st_read(here("data", "rwanda", "rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync", 
                       "rwa_adm1_2006_NISR_WGS1984_20181002.shp"))
eye_fac <- read_csv(here("data", "rwanda", "Rwanda_facilities_gps.csv")) %>%
  rename(cat_serv=`Cataract surgical services`)
eye_fac_sf <- eye_fac %>% st_as_sf(coords = c("Long", "Lat"), crs=4326)

# plotting with tmap
constrained_map <- tm_shape(rwanda) + tm_polygons() +
  tm_shape(popsum) +
  tm_raster(style = "order",
            palette = "viridis",
            # colorNA = "grey",
            title = "Number of individuals 50+ per 100m2") + 
  tm_shape(eye_fac_sf %>% filter(cat_serv == "Permanent")) + 
  tm_dots(size = 0.05,
          col = "red") +
  tm_shape(eye_fac_sf %>% filter(cat_serv == "Occasional")) + 
  tm_dots(size = 0.05,
          col = "pink") +
  tm_add_legend(type = "symbol",
                col = c("red","pink"),
                labels = c("Permanent cataract surgical facilities",
                           "Occasional cataract surgical facilities")) + 
  tm_layout(legend.outside = T,
            legend.outside.position = "right",
            legend.stack = "vertical",
            legend.frame = T,
            legend.outside.size = 0.5)
constrained_map

# saving 
tmap_save(constrained_map, here("data", "rwanda", "maps", "constrainedpop_facilities.eps"))



