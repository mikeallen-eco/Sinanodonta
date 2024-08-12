library(dplyr)
library(sf)
library(ggplot2)
# library(rnaturalearth)
library(terra)
# library(exactextractr)
# library(fasterize)
# library(readxl)
library(tictoc)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

### NOTE:
# CREATED europe_dist_w_mean_1500m.tif ON AMAREL CLUSTER USING SCRIPTS FOUND IN s08_format_EU_waterdist_cluster FOLDER
###

# read in the final "mean closest distance to fresh water" (1500 m radius) raster
min_w = rast(paste0(outpath, "europe_dist_w_mean_1500m.tif"))

# crop and inspect

# Make box for inspecting
coords <- matrix(c(-9.5, 42, # upper left
                   -7, 42, # upper right
                   -7, 36.5, # lower right
                   -9.5, 36.5, # lower left
                   -9.5, 42), ncol = 2, byrow = TRUE) # upper left
# Create a polygon object
polygon <- st_polygon(list(coords))
# # Convert to an sf object
bbox <- st_sfc(polygon, crs = 4326)  # Specify the CRS (EPSG:4326 for WGS84)
min_w_crop <- min_w %>%
  crop(bbox)
min_w_crop %>%
  as.data.frame(xy = T, na.rm = T) %>%
ggplot() +
  geom_sf(data = rnaturalearth::ne_countries(country = "Portugal", scale = 10)) +
  geom_tile(aes(x = x, y = y, fill = dist_water)) +
  scale_fill_viridis_c(limits = c(0,2000)) +
  scale_x_continuous(limits = c(-10, -6)) +
  scale_y_continuous(limits = c(36.8, 42.1)) +
  coord_sf()

### read in CPM observations
cenv <- read_xlsx("data/Swood_new_arrangement.xlsx", 
                  sheet = "Data_all_New_shape")

# make spatial to view point distribution
pts <- cenv %>%
  filter(Scen %in% 2,
         Time %in% 1) %>%
  mutate(lat = Lat, lon = Long) %>%
  st_as_sf(., # first argument = data frame with coordinates
           coords = c("Lat", "Long"), # name of columns, in quotation marks
           crs = 4326)
extract(min_w, vect())
