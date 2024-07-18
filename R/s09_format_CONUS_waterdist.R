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
nj_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_New_Jersey_State_GPKG/NHD_H_New_Jersey_State_GPKG.gpkg"
de_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_Delaware_State_GPKG/NHD_H_Delaware_State_GPKG.gpkg"
ny_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_New_York_State_GPKG/NHD_H_New_York_State_GPKG.gpkg"
pa_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_Pennsylvania_State_GPKG/NHD_H_Pennsylvania_State_GPKG.gpkg"
dws_path <- "/Users/mikea/Documents/mikedata/cpm/202406/drbbnd/drb_bnd_polygon.shp"
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
# set path to landcover tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202406/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img"


# read in the Del Watershed boundary
dws <- read_sf(dws_path) %>%
  st_transform(crs = sin)

dws_buffer <- dws %>%
  st_buffer(dist = 5000) %>%
  st_transform(crs = 4326)

# Make Readington box for testing
# coords <- matrix(c(-74.856902, 40.635823,
#                    -74.692466, 40.635823,
#                    -74.692466, 40.517249,
#                    -74.856902, 40.517249,
#                    -74.856902, 40.635823), ncol = 2, byrow = TRUE)
# Create a polygon object
polygon <- st_polygon(list(coords))
# Convert to an sf object
bbox <- st_sfc(polygon, crs = 4326)  # Specify the CRS (EPSG:4326 for WGS84)

# New Jersey
# Flowline
nj_fline <- read_sf(nj_path, layer = "NHDFlowline") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Line
nj_line <- read_sf(nj_path, layer = "NHDLine") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Area
nj_area <- read_sf(nj_path, layer = "NHDArea") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(crs = 4326) %>%
  st_intersection(., dws_buffer) %>%
  st_geometry()

# Waterbodies
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

files <- list.files(outpath, pattern = ".gpgk")
NHD_sf <- read_sf(files[1])

# Convert sf object to SpatVector for use with terra
vect <- NHD_sf %>%
  sf::st_transform(crs = st_crs(lc)) %>%
  vect()

dist_temp <- lc %>%
  crop(vect(st_transform(dws_buffer, crs = st_crs(lc)))) %>%
  mask(vect(st_transform(dws_buffer, crs = st_crs(lc))))
plot(dist_temp)

# Compute the distance to the nearest point feature
distance_raster <- distance(dist_temp, nj_fline_vect)

# convert geology shapefile to raster (same extent/projection/resolution as landcover)
writeRaster(distance_raster_nj, paste0(outpath, "distance_water_", gsub("gpgk", "", files[1])), overwrite = TRUE)

