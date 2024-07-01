library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"

# load CONUS
us <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))

# test plot CONUS region
plot(us[,"name"])

# set path to worldclim tifs
wc_path <- "/Users/mikea/Documents/mikedata/cpm/202406/wc2"
wc_files <- list.files(wc_path, pattern = "*.tif", full.names = T)

# Read the GeoTIFF files into a raster stack
raster_stack <- rast(wc_files)

# Clip the raster stack
vect_sf <- vect(us)
cropped_raster <- crop(raster_stack, vect_sf)
clipped_raster <- mask(cropped_raster, vect_sf)

plot(clipped_raster[[3]])

# Save the raster stack
# writeRaster(clipped_raster, paste0(outpath, "conus_wc.tif"), overwrite = TRUE)