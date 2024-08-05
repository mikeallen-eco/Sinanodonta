library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(exactextractr)
library(readxl)
library(tictoc)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "conus_wc.tif")
dws_path <- "/Users/mikea/Documents/mikedata/cpm/202406/drbbnd/drb_bnd_polygon.shp"
# set path to landcover tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202406/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img"



# map projections
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
mol <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
latlon <- 4326

# load CONUS
us <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))

# test plot CONUS region
plot(us[,"name"])

# Read the raster files
wc <- rast(wc_path)
lc <- rast(landcov_path)
plot(lc)

# legend <- freq(lc)
# https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
# write.csv(legend, "data/conus_lc_legend.csv", row.names = F)

legend <- levels(lc)[[1]] %>%
  rename(Value = value, value = 2) %>%
  filter(!value %in% "")

# make reclassification matrices for the Corine land cover (11 classes)
reclass_matrix <- legend %>%
  mutate(urban = case_when(grepl(value, pattern = "Developed") ~ 1,
                           TRUE ~ 0),
         ag.cult = case_when(value %in% c("Cultivated Crops") ~ 1,
                             TRUE ~ 0),
         forest = case_when(grepl(value, pattern = "Forest") ~ 1,
                            TRUE ~ 0),
         grass.ag = case_when(value %in% c("Hay/Pasture") ~ 1,
                              TRUE ~ 0),
         grass.nonag = case_when(value %in% c("Herbaceous") ~ 1,
                                 TRUE ~ 0),
         shrub = case_when(value %in% c("Shrub/Scrub") ~ 1, 
                           TRUE ~ 0),
         bare = case_when(value %in% c("Barren Land") ~ 1,
                          TRUE ~ 0),
         snow = case_when(value %in% c("Perennial Snow/Ice") ~ 1,
                          TRUE ~ 0),
         wet = case_when(grepl(value, pattern = "Wetlands") ~ 1,
                         TRUE ~ 0),
         water = case_when(value %in% c("Open Water") ~ 1,
                           TRUE ~ 0))

# write.csv(reclass_matrix, "output/conus_landcover_reclass.csv")

urban_remat <- reclass_matrix %>%
  dplyr::select(Value, urban) %>%
  as.matrix

ag.cult_remat <- reclass_matrix %>%
  dplyr::select(Value, ag.cult) %>%
  as.matrix

forest_remat <- reclass_matrix %>%
  dplyr::select(Value, forest) %>%
  as.matrix

grass.ag_remat <- reclass_matrix %>%
  dplyr::select(Value, grass.ag) %>%
  as.matrix

grass.nonag_remat <- reclass_matrix %>%
  dplyr::select(Value, grass.nonag) %>%
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

wet_remat <- reclass_matrix %>%
  dplyr::select(Value, wet) %>%
  as.matrix

water_remat <- reclass_matrix %>%
  dplyr::select(Value, water) %>%
  as.matrix

# create individual landcover category rasters in 1/0 format (these take ~5-6 min each)
urban <- classify(lc, urban_remat)
ag.cult <- classify(lc, ag.cult_remat)
tic()
forest <- classify(lc, forest_remat)
toc()
tic()
grass.ag <- classify(lc, grass.ag_remat)
toc()
grass.nonag <- classify(lc, grass.nonag_remat)
shrub <- classify(lc, shrub_remat)
bare <- classify(lc, bare_remat)
snow <- classify(lc, snow_remat)
wet <- classify(lc, wet_remat)
tic()
water <- classify(lc, water_remat)
toc()

# Save the binary rasters for easy loading later
writeRaster(urban, paste0(outpath, "conus_lc_urban_binary.tif"), overwrite = TRUE)
writeRaster(ag.cult, paste0(outpath, "conus_lc_ag.cult_binary.tif"), overwrite = TRUE)
writeRaster(forest, paste0(outpath, "conus_lc_forest_binary.tif"), overwrite = TRUE)
writeRaster(grass.ag, paste0(outpath, "conus_lc_grass.ag_binary.tif"), overwrite = TRUE)
writeRaster(grass.nonag, paste0(outpath, "conus_lc_grass.nonag_binary.tif"), overwrite = TRUE)
writeRaster(shrub, paste0(outpath, "conus_lc_shrub_binary.tif"), overwrite = TRUE)
writeRaster(bare, paste0(outpath, "conus_lc_bare_binary.tif"), overwrite = TRUE)
writeRaster(snow, paste0(outpath, "conus_lc_snow_binary.tif"), overwrite = TRUE)
writeRaster(wet, paste0(outpath, "conus_lc_wet_binary.tif"), overwrite = TRUE)
writeRaster(water, paste0(outpath, "conus_lc_water_binary.tif"), overwrite = TRUE)


urban <- rast(paste0(outpath, "conus_lc_urban_binary.tif"))
ag.cult <- rast(paste0(outpath, "conus_lc_ag.cult_binary.tif"))
forest <- rast(paste0(outpath, "conus_lc_forest_binary.tif"))
grass.ag <- rast(paste0(outpath, "conus_lc_grass.ag_binary.tif")); names(grass.ag) <- "grass.ag"
grass.nonag <- rast(paste0(outpath, "conus_lc_grass.nonag_binary.tif")); names(grass.nonag) <- "grass.nonag"
shrub <- rast(paste0(outpath, "conus_lc_shrub_binary.tif")); names(shrub) <- "shrub"
bare <- rast(paste0(outpath, "conus_lc_bare_binary.tif")); names(bare) <- "bare"
snow <- rast(paste0(outpath, "conus_lc_snow_binary.tif")); names(snow) <- "snow"
wet <- rast(paste0(outpath, "conus_lc_wet_binary.tif")); names(wet) <- "wet"
water <- rast(paste0(outpath, "conus_lc_water_binary.tif")); names(water) <- "water"
landcov <- c(grass.ag, grass.nonag, shrub, bare, snow, wet, water) # c(urban, ag.cult, forest) 
names(landcov) <- c("grass.ag", "grass.nonag", "shrub", "bare", "snow", "wet", "water") # c("urban", "ag.cult", "forest") 
names(landcov)
plot(landcov[[1]])         

# get center points for all wc raster cells and make a 1500 m buffer around each one for area calcs

# Get the coordinates of the center of each non-NA cell for the worldclim raster
buffer_pts = terra::xyFromCell(wc[[1]], cells(wc[[1]])) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(wc)) %>%
  # project to sinusoidal projection in meters
  st_transform(crs = st_crs(landcov))

buffers <- buffer_pts %>%
  st_buffer(., dist = 1500)

# extract land cover proportions 
for(i in 1:dim(landcov)[3]){ 
  nm <- names(landcov)[i]
  message(paste0("Processing ", i, " of ", dim(landcov)[3], ": ", nm))
  tic()
  vals <- exactextractr::exact_extract(landcov[[i]], buffers, fun = 'mean')
  toc()
  
  landcov.p <- wc[[1]]
  landcov.p[cells(landcov.p)] <- vals
  writeRaster(landcov.p, paste0(outpath, "conus_lc_", nm, "_p1500.tif"), overwrite = TRUE)
  
}

plot(urban.p)
plot(landcov[[1]])

df <- pts %>%
  left_join(values) %>%
  st_as_sf(., coords = c("Lat", "Long"))