library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"

# load Europe
eu <- rnaturalearth::ne_countries(continent = "Europe", type = "countries", returnclass = "sf", 
                                  scale = 10) %>%
  filter(!sovereignt %in% c("Russia")) %>%
  bind_rows(rnaturalearth::ne_countries(country = "Turkey", returnclass = "sf", 
                                        scale = 10)) %>%
  bind_rows(rnaturalearth::ne_countries(country = "Cyprus", returnclass = "sf", 
                                        scale = 10))

# Define the bounding box (xmin, ymin, xmax, ymax)
bbox <- st_bbox(c(xmin = -25, ymin = 31, xmax = 45, ymax = 70), crs = st_crs(eu))

# Convert the bbox to an sf object
bbox_sf <- st_as_sfc(bbox)

# Perform the clipping
eu2 <- st_intersection(eu, bbox_sf)

# test plot Europe region
plot(eu2[,"sovereignt"])

# set path to worldclim tifs
wc_path <- "/Users/mikea/Documents/mikedata/cpm/202406/wc2"
wc_files <- list.files(wc_path, pattern = "*.tif", full.names = T)

# Read the GeoTIFF files into a raster stack
raster_stack <- rast(wc_files)

# Clip the raster stack
vect_sf <- vect(eu2)
cropped_raster <- crop(raster_stack, vect_sf)
clipped_raster <- mask(cropped_raster, vect_sf)

plot(clipped_raster[[3]])

# Save the raster stack
# writeRaster(clipped_raster, paste0(outpath, "europe_wc.tif"), overwrite = TRUE)

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

# test plots
plot(clipped_raster[[16]])
plot(eu2[,"sovereignt"], add = T, color = "none")
points(pts$lon, pts$lat, pch = 1, col="red")

# note: bio6 matches up exactly with the TMinCold variable provided with the data set (good)
ggplot() +
  geom_sf(data = eu2) +
  geom_sf(data = df, aes(color = wc2.1_2.5m_bio_6)) +
  scale_color_viridis_c()




