library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "europe_wc.tif")

# load europe
eu <- rnaturalearth::ne_countries(continent = "Europe", type = "countries", returnclass = "sf", 
                                  scale = 50) %>%
  filter(!sovereignt %in% c("Russia", "Iceland"))

# Define the bounding box (xmin, ymin, xmax, ymax)
bbox <- st_bbox(c(xmin = -25, ymin = 32, xmax = 40, ymax = 72), crs = st_crs(eu))

# Convert the bbox to an sf object
bbox_sf <- st_as_sfc(bbox)

# Perform the clipping
eu2 <- st_intersection(eu, bbox_sf)

# test plot Europe region
plot(eu2[,"sovereignt"])

# set path to worldclim tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202405/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"

# Read the GeoTIFF files
wc <- rast(paste0(outpath, "europe_wc.tif"))
lc <- rast(landcov_path)
lcp <- project(lc, wc)

plot(lcp)

# create individual landcover category rasters in 1/0 format


# Clip the raster stack
vect_sf <- vect(eu2)
cropped_raster <- crop(lcp, vect_sf)
clipped_raster <- mask(cropped_raster, vect_sf)

plot(clipped_raster)

# Save the raster stack
# writeRaster(clipped_raster, paste0(outpath, "europe_wc.tif"), overwrite = TRUE)

# Install and load the terra package
install.packages("terra")
library(terra)

# Read the categorical raster
categorical_raster <- rast("path/to/categorical_raster.tif")

# Get unique values from the raster
unique_values <- unique(values(categorical_raster))

# Loop through each unique value to create separate rasters
for (value in unique_values) {
  # Create a new raster with 1 where the value matches and NA otherwise
  separate_raster <- categorical_raster == value
  
  # Optionally, you could use the actual value instead of 1
  # separate_raster <- ifel(categorical_raster == value, value, NA)
  
  # Define the output file name
  output_filename <- paste0("path/to/output_folder/raster_value_", value, ".tif")
  
  # Save the separate raster
  writeRaster(separate_raster, output_filename, overwrite = TRUE)
}



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
           crs = st_crs(eu2))

values <- terra::extract(raster_stack, pts)

df <- pts %>%
  left_join(values) %>%
  st_as_sf(., coords = c("Lat", "Long"))