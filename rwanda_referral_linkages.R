# rwanda referral linkages

#### packages ####
library(tidyverse)
library(here)
library(sf)
library(raster)
library(rgdal)
library(tmap)
library(gdistance)
library(spdplyr) # this package allows application of splyr verbs (eg filter, select) to spatial points dataframes


#### notes from Ian ####
# Proportion of referral linkages between primary/secondary facilities and their closest secondary/tertiary facility for which the travel time is below a set threshold (1 or 2 hrs)
#
# Numerator: number of primary/secondary health facilities in a specific geographical area within a given travel time of the closest secondary/tertiary facility.
# Denominator: total number of primary/secondary facilities in the same geographical area.

# so I basically need two rasters: travel time from primary to secondary and travel time from secondary to tertiary
# will do for motorized for now

#### data ####
# admin 1 boundaries
adm1 <- st_read(here("data", "rwanda", "rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync", "rwa_adm1_2006_NISR_WGS1984_20181002.shp"))

# rwanda clip
rwanda_clip <- st_read(here("data", "rwanda","rwa_adm_2006_nisr_wgs1984_20181002_shp.nosync", 
                            "rwa_adm0_2006_NISR_WGS1984_20181002.shp")) 

# motorized friction surface
rwanda.motor <- raster(here("data","frictionsurfaces","2020_motorized_friction_surface.geotiff")) %>%
  crop(rwanda_clip)

# eye facilities
eye_fac <- read.csv(here("data", "rwanda", "Rwanda_facilities_gps.csv"))
eye_fac_sf <- eye_fac %>% st_as_sf(coords = c("Long", "Lat"), crs=4326)
# creating a transition matrix
# eye_fac_sf_t <- st_transform(eye_fac_sf, crs=2163)

#### converting FS to transition matrix ####
T.rwanda <- gdistance::transition(rwanda.motor, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T.rwanda)


#### descriptive stats for types of eye care facilities ####
eye_fac %>%
  group_by(Cataract.surgical.services) %>%
  summarise(n=n())

eye_fac %>%
  group_by(Cataract.surgical.services, Region) %>%
  summarise(n=n())


#### preparing point locations ####
prim_fac <- eye_fac %>% filter(Cataract.surgical.services == "None") %>%
  dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services, Region) %>%
  rename(X_COORD = Long, 
         Y_COORD = Lat , 
         name = Facility.Name, 
         cataract_surgical_services = Cataract.surgical.services)
# setting coordinates
coordinates(prim_fac) <- ~ X_COORD + Y_COORD 

occperm_fac <- eye_fac %>% filter(Cataract.surgical.services == "Occasional" | Cataract.surgical.services == "Permanent") %>%
  dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services, Region) %>%
  rename(X_COORD = Long, 
         Y_COORD = Lat , 
         name = Facility.Name, 
         cataract_surgical_services = Cataract.surgical.services)
# setting coordinates
coordinates(occperm_fac) <- ~ X_COORD + Y_COORD 

occ_fac <- eye_fac %>% filter(Cataract.surgical.services == "Occasional") %>%
  dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services, Region) %>%
  rename(X_COORD = Long, 
         Y_COORD = Lat , 
         name = Facility.Name, 
         cataract_surgical_services = Cataract.surgical.services)
# setting coordinates
coordinates(occ_fac) <- ~ X_COORD + Y_COORD 

perm_fac <- eye_fac %>% filter(Cataract.surgical.services == "Permanent") %>%
  dplyr::select(Long, Lat, Facility.Name, Cataract.surgical.services, Region) %>%
  rename(X_COORD = Long, 
         Y_COORD = Lat , 
         name = Facility.Name, 
         cataract_surgical_services = Cataract.surgical.services)
# setting coordinates
coordinates(perm_fac) <- ~ X_COORD + Y_COORD 


#### travel times to various facilities at the national level ####

# primary to secondary facilities
prim_sec <- costDistance(T.GC, # transition  matrix
             prim_fac, # from coords -- 8 facilities without cataract surgical services
             occperm_fac) %>%  # to coords -- 46 facilities with temp or perm cataract surg services
  # converting to tibble - rows are prim_fac and columns are second_fac
  as_tibble() %>%
  rownames_to_column() %>%
  rename_all(., function(x) paste0("temp_", x)) %>%
  rename(primary_fac=temp_rowname) %>%
  # finding minimum travel time from each primary facility to the secondary one
  rowwise() %>%
  # min is not working with select
  mutate(min_tt=min(c(temp_1,temp_2,temp_3,temp_4,temp_5,temp_6,temp_7,temp_8,temp_9,temp_10,temp_11,temp_12,temp_13,
                      temp_14,temp_15,temp_16,temp_17,temp_18,temp_19,temp_20,temp_21,temp_22,temp_23,temp_24,temp_25,
                      temp_26,temp_27,temp_28,temp_30,temp_31,temp_32,temp_33,temp_34,temp_35,temp_36,temp_37,temp_38,
                      temp_39,temp_40,temp_41,temp_42,temp_43,temp_44,temp_45,temp_46)),
         # / 60 to get hours traveled
         hr_tt=min_tt/60) %>%
  ungroup() %>%
  summarize(total=sum(!is.na(primary_fac)),
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  # assiging a column to ID these as national level estimates
  mutate(geo="national")
prim_sec

# primary to tertiary facilities
prim_tert <- costDistance(T.GC, # transition  matrix
                         prim_fac, # from coords -- 8 facilities without cataract surgical services
                         perm_fac) %>%  # to coords -- 46 facilities with temp or perm cataract surg services
  # converting to tibble - rows are prim_fac and columns are second_fac
  as_tibble() %>%
  rownames_to_column() %>%
  rename_all(., function(x) paste0("permanent_", x)) %>%
  rename(second_fac=permanent_rowname) %>%
  # finding minimum travel time from each primary facility to the secondary one
  rowwise() %>%
  mutate(min_tt=min(permanent_1,permanent_2,permanent_3,permanent_4,permanent_5,permanent_6,permanent_7,
                    permanent_8,permanent_9),
         # / 60 to get hours traveled
         hr_tt=min_tt/60) %>%
  ungroup() %>%
  summarize(total=sum(!is.na(second_fac)),
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  # assiging a column to ID these as national level estimates
  mutate(geo="national") %>%
  rename_all(., function(x) paste0("tert_",x))
prim_tert
  

#### travel times to various facilities at the sub-national level ####

# first step is to assing the points to their respective subnational boundary, this is already provided in the data provided by Ian (see below)
# therefore there is no need to use intersect to determine the points.
# unique(adm1)
# unique(eye_fac$Region) - North, West, South, East, Kigali City

#### north ####
# travel time from primary to secondary facilities in northern region
prim_sec_north <- costDistance(T.GC,
                              filter(prim_fac, Region =="North"),
                              filter(occperm_fac, Region == "North")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("temp_", x)) %>%
  rename(primary_fac=temp_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(temp_3, temp_4, temp_22, temp_33, temp_40, temp_41, temp_42),
         hr_tt=min_tt/60) %>%
  ungroup() %>%
  summarize(total=sum(!is.na(primary_fac)), # note to self -- can turn into function later
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  mutate(geo="North")
prim_sec_north

prim_tert_north <- costDistance(T.GC,
                              filter(prim_fac, Region =="North"),
                              filter(perm_fac, Region == "North")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("permanent_", x)) %>%
  rename(second_fac=permanent_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(permanent_7),
         hr_tt=min_tt/60,
         second_fac=as.numeric(second_fac)) %>%
  ungroup() %>%
  dplyr::summarize(total=sum(!is.na(second_fac)), # note to self -- can turn into function later
                   n_less1=sum(hr_tt<1),
                   p_less1=n_less1/total*100,
                   n_less2=sum(hr_tt<2 & hr_tt>1),
                   p_less2=n_less2/total*100,
                   n_2plus=sum(hr_tt>=2),
                   p_2plus=n_2plus/total*100) %>%
  mutate(geo="North") %>%
  rename_all(., function(x) paste0("tert_",x))
prim_tert_north

#### west ####
# travel time from primary to secondary facilities in western region
costDistance(T.GC,
             filter(prim_fac, Region =="West"),
             filter(occperm_fac, Region == "West")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("temp_", x)) %>%
  rename(primary_fac=temp_rowname) %>%
  rowwise()
# no primary facilities in the western region
 
prim_sec_west <- 
  tibble(total = 0, 
            n_less1 = 0, 
            p_less1 = 0, 
            n_less2 = 0,
            p_less2 = 0, 
            n_2plus = 0, 
            p_2plus = 0, 
            geo = "West")
prim_sec_west

costDistance(T.GC,
             filter(prim_fac, Region =="West"),
             filter(perm_fac, Region == "West")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("permanent_", x)) %>%
  rename(second_fac=permanent_rowname) %>%
  rowwise() 
# again no primary facilities in the western region

prim_tert_west <- tibble(total = 0, 
                         n_less1 = 0, 
                         p_less1 = 0, 
                         n_less2 = 0,
                         p_less2 = 0, 
                         n_2plus = 0, 
                         p_2plus = 0, 
                         geo = "West") %>%
  rename_all(., function(x) paste0("tert_",x))
prim_tert_west

#### South ####
# travel time from primary to secondary facilities in southern region
prim_sec_south <- costDistance(T.GC,
                              filter(prim_fac, Region =="South"),
                              filter(occperm_fac, Region == "South")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("temp_", x)) %>%
  rename(primary_fac=temp_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(temp_2,temp_6,temp_9,temp_11,temp_12,temp_13,temp_15,temp_20,temp_31,temp_37,temp_38,temp_39),
         hr_tt=min_tt/60) %>%
  ungroup() %>%
  summarize(total=sum(!is.na(primary_fac)), # note to self -- can turn into function later
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  # assigning a column to ID these as national level estimates
  mutate(geo="South")
prim_sec_south

prim_tert_south <- costDistance(T.GC,
                              filter(prim_fac, Region =="South"),
                              filter(perm_fac, Region == "South")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("permanent_", x)) %>%
  rename(second_fac=permanent_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(permanent_1,permanent_2),
         hr_tt=min_tt/60,
         second_fac=as.numeric(second_fac)) %>%
  ungroup() %>%
  dplyr::summarize(total=sum(!is.na(second_fac)), # note to self -- can turn into function later
                   n_less1=sum(hr_tt<1),
                   p_less1=n_less1/total*100,
                   n_less2=sum(hr_tt<2 & hr_tt>1),
                   p_less2=n_less2/total*100,
                   n_2plus=sum(hr_tt>=2),
                   p_2plus=n_2plus/total*100) %>%
  # assigning a column to ID these as national level estimates
  mutate(geo="South") %>%
  rename_all(., function(x) paste0("tert_",x))
prim_tert_south

#### east ####
# travel time from primary to secondary facilities in eastern region
prim_sec_east <- costDistance(T.GC,
             filter(prim_fac, Region =="East"),
             filter(occperm_fac, Region == "East")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("temp_", x)) %>%
  rename(primary_fac=temp_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(temp_5,temp_17, temp_23, temp_25, temp_34, temp_35, temp_36, temp_43, temp_45),
         hr_tt=min_tt/60) %>%
  ungroup() %>%
  summarize(total=sum(!is.na(primary_fac)), # note to self -- can turn into function later
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  # assigning a column to ID these as national level estimates
  mutate(geo="East")
prim_sec_east

prim_tert_east <- costDistance(T.GC,
                              filter(prim_fac, Region =="East"),
                              filter(perm_fac, Region == "East")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("permanent_", x)) %>%
  rename(second_fac=permanent_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(permanent_8),
         hr_tt=min_tt/60,
         second_fac=as.numeric(second_fac)) %>%
  ungroup() %>%
  dplyr::summarize(total=sum(!is.na(second_fac)), # note to self -- can turn into function later
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  # assigning a column to ID these as national level estimates
  mutate(geo="East") %>%
  rename_all(., function(x) paste0("tert_",x))
prim_tert_east

#### Kigali city ####
# travel time from primary to secondary facilities in kigali region
prim_sec_kig <- costDistance(T.GC,
                              filter(prim_fac, Region =="Kigali City"),
                              filter(occperm_fac, Region == "Kigali City")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("temp_", x)) %>%
  rename(primary_fac=temp_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(temp_14, temp_19, temp_21, temp_26, temp_29, temp_44),
         hr_tt=min_tt/60) %>%
  ungroup() %>%
  summarize(total=sum(!is.na(primary_fac)), # note to self -- can turn into function later
            n_less1=sum(hr_tt<1),
            p_less1=n_less1/total*100,
            n_less2=sum(hr_tt<2 & hr_tt>1),
            p_less2=n_less2/total*100,
            n_2plus=sum(hr_tt>=2),
            p_2plus=n_2plus/total*100) %>%
  # assigning a column to ID these as national level estimates
  mutate(geo="Kigali City")
prim_sec_kig

prim_tert_kig <- costDistance(T.GC,
                              filter(prim_fac, Region =="Kigali City"),
                              filter(perm_fac, Region == "Kigali City")) %>%
  as_tibble() %>% rownames_to_column() %>%
  rename_all(., function(x) paste0("permanent_", x)) %>%
  rename(second_fac=permanent_rowname) %>%
  rowwise() %>%
  mutate(min_tt=min(permanent_3, permanent_4, permanent_5, permanent_9),
         hr_tt=min_tt/60,
         second_fac=as.numeric(second_fac)) %>%
  ungroup() %>%
  dplyr::summarize(total=sum(!is.na(second_fac)), # note to self -- can turn into function later
                   n_less1=sum(hr_tt<1),
                   p_less1=n_less1/total*100,
                   n_less2=sum(hr_tt<2 & hr_tt>1),
                   p_less2=n_less2/total*100,
                   n_2plus=sum(hr_tt>=2),
                   p_2plus=n_2plus/total*100) %>%
  # assigning a column to ID these as national level estimates
  mutate(geo="Kigali City") %>%
  rename_all(., function(x) paste0("tert_",x))
prim_tert_kig

#### table ####
prim_sec_table <- rbind(prim_sec,
                        prim_sec_north,
                        prim_sec_west,
                        prim_sec_east,
                        prim_sec_south,
                        prim_sec_kig) %>%
  dplyr::select(geo, total, n_less1:p_less2)

prim_sec_table

prim_tert_table <- rbind(prim_tert,
                         prim_tert_north,
                         prim_tert_west,
                         prim_tert_east,
                         prim_tert_south,
                         prim_tert_kig) %>%
  dplyr::select(tert_geo, tert_total, tert_n_less1:tert_p_less2) %>%
  rename(geo=tert_geo, total=tert_total)
prim_tert_table


table <- full_join(prim_sec_table, prim_tert_table) %>%
  dplyr::select(-n_less1,-n_less2,-tert_n_less1,-tert_n_less2) %>%
  rename("Administrative area" = geo,
         "Total primary eye care facilities" = total,
         "% eye care facilities <1 hour travel to nearest cataract surgical centre" = p_less1,
         "% eye care facilities <2 hours travel to nearest cataract surgical centre" = p_less2,
         "% eye care facilities <1 hour travel to nearest permanent cataract surgical centre" = tert_p_less1,
         "% eye care facilities <2 hours travel to nearest permanent cataract surgical centre" = tert_p_less2)

write_csv(table, here("results", "rwanda", "tables", "referral_linkages.csv"))

