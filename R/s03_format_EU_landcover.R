library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(exactextractr)
library(readxl)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "europe_wc.tif")
legend_path <- "/Users/mikea/Documents/mikedata/cpm/202405/u2018_clc2018_v2020_20u1_raster100m/LEGEND/CLC2018_CLC2018_V2018_20_QGIS.txt"
legend <- read.csv(legend_path, header = F)
# set path to landcover tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202405/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"
Ukraine_lc_path <- "/Users/mikea/Documents/mikedata/cpm/202406/eea_r_3035_100_m_eni-clc-2018_p_2017-2019_v01_r00/TIFF/ENI2018_CLC2018_V2020_1_UA_PILOT.tif"

# map projections
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
mol <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
latlon <- 4326

# read in data for observations
cenv <- read_xlsx("data/Swood_new_arrangement.xlsx", 
                  sheet = "Data_all_New_shape")

# make spatial to view point distribution
pts <- cenv %>%
  filter(Scen %in% 2,
         Time %in% 1) %>%
  mutate(lat = Lat, lon = Long) %>%
  st_as_sf(., # first argument = data frame with coordinates
           coords = c("Long", "Lat"), # name of columns, in quotation marks
           crs = latlon)

# load Europe
eu <- rnaturalearth::ne_countries(continent = "Europe", type = "countries", returnclass = "sf", 
                                  scale = 10) %>%
  filter(!sovereignt %in% c("Russia")) %>%
  bind_rows(rnaturalearth::ne_countries(country = "Turkey", returnclass = "sf", 
                                        scale = 10)) %>%
  bind_rows(rnaturalearth::ne_countries(country = "Cyprus", returnclass = "sf", 
                                        scale = 10)) %>%
  st_transform(crs = latlon)

# Define the bounding box (xmin, ymin, xmax, ymax)
bbox <- st_bbox(c(xmin = -25, ymin = 31, xmax = 45, ymax = 70), crs = latlon)

# Convert the bbox to an sf object
bbox_sf <- st_as_sfc(bbox)

# Perform the clipping
eu2 <- st_intersection(eu, bbox_sf)

# test plot Europe region
plot(eu2[,"sovereignt"])

# Read the GeoTIFF files
wc <- rast(paste0(outpath, "europe_wc.tif"))
lc <- rast(landcov_path)
# lc_U <- rast(Ukraine_lc_path) # appears to be just a small part of Ukraine 
plot(lc)

# don't need to clip land cover
# # Clip the raster
# vect_sf <- vect(eu2)
# lcp <- project(lc, wc) %>%
#   crop(., vect_sf) %>%
#   mask(., vect_sf)
# plot(lcp)
# plot(eu2, add = T, color = "none")
# plot(wc[[1]])


# lcp2 <- clipped_raster
# plot(lcp2)
# plot(wc[[1]])

# make reclassification matrices for the Corine land cover (11 classes)
reclass_matrix <- legend %>%
  mutate(urban = case_when(V1 %in% 111:142 ~ 1,
                           TRUE ~ 0),
         ag = case_when(V1 %in% c(211:223, 241:244) ~ 1,
                        TRUE ~ 0),
         forest = case_when(V1 %in% 311:313 ~ 1,
                            TRUE ~ 0),
         grass = case_when(V1 %in% c(231,321,322) ~ 1,
                           TRUE ~ 0),
         shrub = case_when(V1 %in% c(323:324) ~ 1, # counting peat bogs as shrub
                           TRUE ~ 0),
         bare = case_when(V1 %in% 331:334 ~ 1,
                          TRUE ~ 0),
         snow = case_when(V1 %in% 335 ~ 1,
                          TRUE ~ 0),
         fwet = case_when(V1 %in% 411:412 ~ 1,
                          TRUE ~ 0),
         cwet = case_when(V1 %in% 421:423 ~ 1,
                                TRUE ~ 0),
         fwater = case_when(V1 %in% 511:512 ~ 1,
                              TRUE ~ 0),
         cwater = case_when(V1 %in% 521:523 ~ 1,
                                 TRUE ~ 0),
         LABEL3 = case_when(V6 %in% "Beaches dunes sands" ~ "Beaches, dunes, sands",
                            V6 %in% "Land principally occupied by agriculture with significant areas of natural vegetation" ~ "Land principally occupied by agriculture, with significant areas of natural vegetation",
                            TRUE ~ V6)) %>%
  left_join(levels(lc)[[1]]) %>%
  bind_rows(data.frame(urban = NA, ag = NA, shrub = NA, forest = NA, grass = NA,
                       bare = NA, snow = NA, fwet = NA, cwet = NA, fwater = NA,
                       cwater = NA, Value = 128))

urban_remat <- reclass_matrix %>%
  dplyr::select(Value, urban) %>%
  as.matrix

ag_remat <- reclass_matrix %>%
  dplyr::select(Value, ag) %>%
  as.matrix

forest_remat <- reclass_matrix %>%
  dplyr::select(Value, forest) %>%
  as.matrix

grass_remat <- reclass_matrix %>%
  dplyr::select(Value, grass) %>%
  as.matrix

shrub_remat <- reclass_matrix %>%
  dplyr::select(Value, shrub) %>%
  as.matrix

bare_remat <- reclass_matrix %>%
  dplyr::select(Value, bare) %>%
  as.matrix

snow_remat <- reclass_matrix %>%
  dplyr::select(Value, snow) %>%
  as.matrix

fwet_remat <- reclass_matrix %>%
  dplyr::select(Value, fwet) %>%
  as.matrix

cwet_remat <- reclass_matrix %>%
  dplyr::select(Value, cwet) %>%
  as.matrix

fwater_remat <- reclass_matrix %>%
  dplyr::select(Value, fwater) %>%
  as.matrix

cwater_remat <- reclass_matrix %>%
  dplyr::select(Value, cwater) %>%
  as.matrix

# create individual landcover category rasters in 1/0 format (these take ~3-5 min each)
# urban <- classify(lc, urban_remat)
# ag <- classify(lc, ag_remat)
# forest <- classify(lc, forest_remat)
# grass <- classify(lc, grass_remat)
# shrub <- classify(lc, shrub_remat)
# bare <- classify(lc, bare_remat)
# snow <- classify(lc, snow_remat)
# fwet <- classify(lc, fwet_remat)
# cwet <- classify(lc, cwet_remat)
# fwater <- classify(lc, fwater_remat)
# cwater <- classify(lc, cwater_remat)

# landcov <- c(urban, ag, forest, grass, shrub, bare, snow, fwet, cwet, fwater, cwater)
# names(landcov) <- c("urban", "ag", "forest", "grass", "shrub", "bare", "snow", "fwet", "cwet", "fwater", "cwater")
# names(landcov)
# plot(landcov[[3]])
# points(st_transform(pts, crs = st_crs(urban)), col = "red")

# Save the raster stack
# writeRaster(landcov, paste0(outpath, "europe_lc_binary.tif"), overwrite = TRUE)
landcov <- rast(paste0(outpath, "europe_lc_binary.tif"))
                
# get center points for all wc raster cells and make a 2500 m buffer around each one for area calcs

# Get the coordinates of the center of each non-NA cell for the worldclim raster
buffer_pts = terra::xyFromCell(wc[[1]], cells(wc[[1]])) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(wc)) %>%
  # project to sinusoidal projection in meters
  st_transform(crs = st_crs(landcov))

buffers <- buffer_pts %>%
  st_buffer(., dist = 2500)
library(tictoc)
# extract land cover proportions 
for(i in 7:11){ # dim(landcov)[3]){
nm <- names(landcov)[i]
message(paste0("Processing ", nm))
tic()
vals <- exactextractr::exact_extract(landcov[[i]], buffers, fun = 'mean')
toc()

landcov.p <- wc[[1]]
landcov.p[cells(landcov.p)] <- vals
writeRaster(landcov.p, paste0(outpath, "europe_lc_", nm, ".tif"), overwrite = TRUE)

}


plot(urban.p)
plot(landcov[[1]])

df <- pts %>%
  left_join(values) %>%
  st_as_sf(., coords = c("Lat", "Long"))





