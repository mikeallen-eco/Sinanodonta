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
iw_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/inlandwater.gpkg"
s1_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_1.gpkg"
s2_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_2.gpkg"
s3_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_3.gpkg"
s4_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_4.gpkg"
s5_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_5.gpkg"
s6_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_6.gpkg"
s7_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_7.gpkg"
s8_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_8.gpkg"
s9_path <- "/Users/mikea/Documents/mikedata/cpm/202405/EUHYDRO/strahler_9.gpkg"
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
# set path to landcover tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/europe_lc_binary2.tif"
wc_path <- paste0(outpath, "europe_wc.tif")

# Make box for testing
coords <- matrix(c(-9, 37.2, # upper left
                   -8.6, 37.2, # upper right
                   -8.6, 37.06, # lower right
                   -9, 37.06, # lower left
                   -9, 37.2), ncol = 2, byrow = TRUE) # upper left
# Create a polygon object
polygon <- st_polygon(list(coords))
# # Convert to an sf object
bbox <- st_sfc(polygon, crs = 4326)  # Specify the CRS (EPSG:4326 for WGS84)

st_layers(iw_path)

# inland waters
iw <- read_sf(iw_path) 

iw1 <- iw %>%
  st_cast("MULTIPOLYGON") %>%
  st_zm(drop = TRUE, what = "ZM") %>% 
  st_geometry() %>%
  st_simplify(dTolerance = 5) %>% 
  st_transform(crs = 4326)

iw2 <- iw1 %>%
  st_intersection(., bbox)

plot(iw6)

# Strahler 1
s1 <- read_sf(s1_path) 

s2 <- s1 %>%
  st_cast("MULTILINESTRING") %>%
  st_zm(drop = TRUE, what = "ZM")

s3 <- s2 %>% 
  st_geometry() %>%
  st_simplify(dTolerance = 100) # confirmed 100 m is reasonable tolerance by mapping smaller area

s4 <- s3 %>% 
  st_transform(crs = 4326) %>%
  st_intersection(., bbox)

plot(s4, add = T, col = "red")

# Strahler 2
nj_area <- read_sf(nj_path, layer = "NHDArea") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Strahler 3
nj_waterbody <- read_sf(nj_path, layer = "NHDWaterbody") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# ggplot() +
#   geom_sf(data = nj_fline, color = "dodgerblue") +
#   geom_sf(data = nj_line, color = "green") +
#   geom_sf(data = nj_area, fill = "red") +
#   geom_sf(data = nj_waterbody, fill = "firebrick") +
#   theme_bw()

# write_sf(nj_fline, paste0(outpath, "nj_fline.gpkg"))
# write_sf(nj_waterbody, paste0(outpath, "nj_waterbody.gpkg"), append = F)
# write_sf(nj_line, paste0(outpath, "nj_line.gpkg"), append = F)
# write_sf(nj_area, paste0(outpath, "nj_area.gpkg"), append = F)

# New York
# Flowline
ny_fline <- read_sf(ny_path, layer = "NHDFlowline") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Line
ny_line <- read_sf(ny_path, layer = "NHDLine") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Area
ny_area <- read_sf(ny_path, layer = "NHDArea") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Waterbodies
ny_waterbody <- read_sf(ny_path, layer = "NHDWaterbody") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# write_sf(ny_fline, paste0(outpath, "ny_fline.gpkg"))
# write_sf(ny_waterbody, paste0(outpath, "ny_waterbody.gpkg"), append = F)
# write_sf(ny_line, paste0(outpath, "ny_line.gpkg"), append = F)
# write_sf(ny_area, paste0(outpath, "ny_area.gpkg"), append = F)

# ggplot() +
#   geom_sf(data = ny_fline, color = "dodgerblue") +
  # geom_sf(data = ny_line, color = "green") +
  # geom_sf(data = ny_area, fill = "red") +
  # geom_sf(data = ny_waterbody, fill = "firebrick") +
  # theme_bw()

# Pennsylvania
# Flowline
pa_fline <- read_sf(pa_path, layer = "NHDFlowline") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Line
pa_line <- read_sf(pa_path, layer = "NHDLine") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Area
pa_area <- read_sf(pa_path, layer = "NHDArea") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Waterbodies
pa_waterbody <- read_sf(pa_path, layer = "NHDWaterbody") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# write_sf(pa_fline, paste0(outpath, "pa_fline.gpkg"))
# write_sf(pa_waterbody, paste0(outpath, "pa_waterbody.gpkg"), append = F)
# write_sf(pa_line, paste0(outpath, "pa_line.gpkg"), append = F)
# write_sf(pa_area, paste0(outpath, "pa_area.gpkg"), append = F)

# ggplot() +
#   geom_sf(data = pa_fline, color = "dodgerblue") +
  # geom_sf(data = pa_line, color = "green") +
  # geom_sf(data = pa_area, fill = "red") +
  # geom_sf(data = pa_waterbody, fill = "firebrick") +
  # theme_bw()

# ggplot() +
#   geom_sf(data = ny_fline, color = "dodgerblue") +
  # geom_sf(data = ny_line, color = "green") +
  # geom_sf(data = ny_area, fill = "red") +
  # geom_sf(data = ny_waterbody, fill = "firebrick") +
  # theme_bw()

# Delaware
# Flowline
de_fline <- read_sf(de_path, layer = "NHDFlowline") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Line
de_line <- read_sf(de_path, layer = "NHDLine") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Area
de_area <- read_sf(de_path, layer = "NHDArea") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Waterbodies
de_waterbody <- read_sf(de_path, layer = "NHDWaterbody") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# write_sf(de_fline, paste0(outpath, "de_fline.gpkg"))
# write_sf(de_waterbody, paste0(outpath, "de_waterbody.gpkg"), append = F)
# write_sf(de_line, paste0(outpath, "de_line.gpkg"), append = F)
# write_sf(de_area, paste0(outpath, "de_area.gpkg"), append = F)

# ggplot() +
#   geom_sf(data = de_fline, color = "dodgerblue") +
  # geom_sf(data = de_line, color = "green") +
  # geom_sf(data = de_area, fill = "red") +
  # geom_sf(data = de_waterbody, fill = "firebrick") +
  # theme_bw()

# read in conus worldclim & landcover rasters and geology shapefile
lc <- rast(landcov_path)

dist_temp <- lc %>%
  crop(vect(st_transform(dws_buffer, crs = st_crs(lc)))) %>%
  mask(vect(st_transform(dws_buffer, crs = st_crs(lc)))) %>%
  aggregate(fact = 10)

# Get the coordinates of the center of each non-NA cell for the worldclim raster
lc_pts = terra::xyFromCell(dist_temp[[1]], cells(dist_temp[[1]])) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(lc))

# loop through all the files
files <- list.files(outpath, pattern = ".gpkg")
for(i in 2:length(files)){
tic()
message("working on: ", files[i])
NHD_sf <- read_sf(paste0(outpath, files[i])) %>%
  sf::st_transform(crs = st_crs(lc))

# Find the nearest feature in sf2 for each feature in sf1
nearest_indices <- st_nearest_feature(lc_pts, NHD_sf)

# Extract the nearest features from sf2
nearest_features <- NHD_sf[nearest_indices, ]

# Calculate the distances between each feature in sf1 and its nearest feature in sf2
distances <- st_distance(lc_pts, nearest_features, by_element = TRUE)

# Add the distances to the sf1 object
lc_pts$nearest_distance <- as.numeric(distances)

write_sf(lc_pts, paste0(outpath, "dist_water/dist_", files[i]))
toc()
}

# select minimum distance per point
dist_list <- list()
dist_files <- list.files(paste0(outpath, "dist_water/"), pattern = ".gpkg")
for(j in 1:length(dist_files)){
message("working on ", j, " of ", length(dist_files), ":", dist_files[j])
if(j %in% 1){
dist_w <- read_sf(paste0(outpath, "dist_water/", dist_files[j])) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(1:3)}else{
    dist_w <- read_sf(paste0(outpath, "dist_water/", dist_files[j])) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(nearest_distance)
    }
dist_list[[j]] <- dist_w
}

dist_w_df <- do.call(cbind, dist_list)
colnames(dist_w_df) <- c("lon", "lat", LETTERS[3:18])

tic()
dist_w_min <- dist_w_df %>%
  rowwise() %>%
  mutate(min_dist = min(c_across(3:18), na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(lon, lat, min_dist)
toc()

# populate 300 m raster with the minimum values
dist_w_raster <- dist_temp
dist_w_raster[cells(dist_w_raster)] <- dist_w_min$min_dist

# crop and mask template raster to Del. Watershed buffer area
wc <- rast(wc_path)[[1]]
wc_crop <- wc %>%
  crop(vect(st_transform(dws_buffer, crs = st_crs(wc)))) %>%
  mask(vect(st_transform(dws_buffer, crs = st_crs(wc)))) 

plot(wc_crop)

# Get the coordinates of the center of each non-NA cell for the worldclim raster
  # make 1500 m buffer around DWS template raster points

buffer_pts = terra::xyFromCell(wc_crop, cells(wc_crop)) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(wc)) %>%
  # project to sinusoidal projection in meters
  st_transform(crs = st_crs(lc))

buffers <- buffer_pts %>%
  st_buffer(., dist = 1500)

# extract average minimum distance to water for use in the final prediction surface
tic()
vals <- exactextractr::exact_extract(dist_w_raster, buffers, fun = 'mean')
toc()

dist_w_final <- wc_crop
dist_w_final[cells(dist_w_final)] <- vals
names(dist_w_final) <- "dist_water"
plot(dist_w_final)

writeRaster(dist_w_final, paste0(outpath, "dist_w_mean_1500mB.tif"), overwrite = TRUE)

dist_w_final <- rast(paste0(outpath, "dist_w_mean_1500m.tif"))
