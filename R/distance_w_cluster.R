library(dplyr)
library(sf)
library(terra)
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

# read in conus worldclim & landcover rasters and geology shapefile
lc <- rast("conus_lc_snow_binary.tif")
dist_temp <- lc %>%
  crop(vect(st_transform(dws_buffer, crs = st_crs(lc)))) %>%
  mask(vect(st_transform(dws_buffer, crs = st_crs(lc))))

# read in the Del Watershed boundary
dws <- read_sf("drb_bnd_polygon.shp") %>%
  st_transform(crs = sin)

dws_buffer <- dws %>%
  st_buffer(dist = 5000) %>%
  st_transform(crs = 4326)

# loop through water files and calculate distance raster for each
files <- list.files(pattern = ".gp")
for(i in 1:length(files)){
NHD_sf <- read_sf(files[i])
message("working on: ", files[i])

# Convert sf object to SpatVector for use with terra
vect <- NHD_sf %>%
  sf::st_transform(crs = st_crs(lc)) %>%
  vect()

# Compute the distance to the nearest point feature
distance_raster <- distance(dist_temp, vect)

writeRaster(distance_raster, paste0(outpath, "distance_water_", files[1], ".tif"), overwrite = TRUE)

}