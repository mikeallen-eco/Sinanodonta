i = 1
library(dplyr)
library(sf)
#library(ggplot2)
#library(rnaturalearth)
library(terra)
library(exactextractr)
library(tictoc)

message("Reading in raster files...")
landcov <- rast("europe_lc_binary2.tif")
wc <- rast("europe_wc.tif")
terraOptions(cores = 20)

# get center points for all wc raster cells and make a 2500 m buffer around each one for area calcs

# Get the coordinates of the center of each non-NA cell for the worldclim raster
message("Getting xy points from wc raster file...")
buffer_pts = terra::xyFromCell(wc[[1]], cells(wc[[1]])) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(wc)) %>%
  # project to landcov projection in meters
  st_transform(crs = st_crs(landcov))

message("Buffering xy points from wc raster file...")
buffers <- buffer_pts %>%
  st_buffer(., dist = 1500)

# extract land cover proportions 
#for(i in 1:dim(landcov)[3]){ 
nm <- names(landcov)[i]
message(paste0("Extracting values from ", nm, "..."))
tic()
vals <- exactextractr::exact_extract(landcov[[i]], buffers, fun = 'mean')
# 6832.904 sec elapsed

message("Populating raster with extracted values...")
landcov.p <- wc[[1]]
landcov.p[cells(landcov.p)] <- vals

message("Writing final raster...")
writeRaster(landcov.p, paste0("europe_lc_", nm, "_p1500.tif"), overwrite = TRUE)
toc()
#}