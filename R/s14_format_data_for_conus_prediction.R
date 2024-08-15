library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "conus_wc.tif")
geo_path <- paste0(outpath, "conus_geology.tif")
elev_path <- paste0(outpath, "conus_elev.tif")
water_path <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/dist_w_mean_1500m.tif"

# load CONUS
us <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))

### read in CONUS covariate data and make into a single rasterstack
wc <- rast(wc_path)
plot(wc[[1]])

# format landcover rasters
lc_for <- rast(paste0(outpath, "conus_lc_forest_p1500.tif"))
names(lc_for) <- "forest_p1500m"
lc_grass <- rast(paste0(outpath, "conus_lc_grass.nonag_p1500.tif"))
names(lc_grass) <- "grass_p1500m"
lc_ag <- rast(paste0(outpath, "conus_lc_ag.cult_p1500.tif")) + 
  rast(paste0(outpath, "conus_lc_grass.ag_p1500.tif"))
plot(lc_ag)
names(lc_ag) <- "ag_p1500m"
lc_bare <- rast(paste0(outpath, "conus_lc_bare_p1500.tif"))
names(lc_bare) <- "bare_p1500m"
lc_wet <- rast(paste0(outpath, "conus_lc_wet_p1500.tif"))
names(lc_wet) <- "wet_p1500m"
lc_urban <- rast(paste0(outpath, "conus_lc_urban_p1500.tif"))
names(lc_urban) <- "urban_p1500m"

# read in geology and elevation rasters 
geo <- rast(geo_path)
elev <- rast(elev_path)
water <- rast(water_path)
water1 <- wc[[1]]
water1[!is.na(water1)] <- 246
water2 <- water - 246
water3 <- project(water2, water1)
plot(water3)
water_fin <- sum(water1, water3, na.rm = T)
plot(water_fin)

raster_stack <- c(wc, lc_for, lc_grass, lc_ag, lc_bare, lc_wet, lc_urban, geo, elev, water_fin)
names(raster_stack)
names(raster_stack)[c(1:19,30, 31)] <- c("bio1", "bio10", "bio11", "bio12", "bio13",
                                     "bio14", "bio15", "bio16", "bio17", "bio18",
                                     "bio19", "bio2", "bio3", "bio4", "bio5", 
                                     "bio6", "bio7", "bio8", "bio9", "elev", "dist_water")
names(raster_stack)
# writeRaster(raster_stack, paste0(outpath, "conus_final_raster_stack.tif"), overwrite=TRUE)

# test plots
plot(raster_stack[[20]])
plot(us[,"name"], add = T, color = "none", border="red")
