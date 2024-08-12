i = 8
library(dplyr)
library(sf)
library(terra)
library(tictoc)
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

# read in EU landcover raster
lc <- rast("europe_lc_binary2.tif")
dist_temp <- lc[[1]] %>%
  aggregate(fact = 3)

# Get the coordinates of the center of each non-NA cell for the worldclim raster
lc_pts = terra::xyFromCell(dist_temp, cells(dist_temp)) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(lc))

# loop through water files and make simplified version of each for distance calcs
# files <- list.files(pattern = ".gpkg")

#for(i in 1:length(files)){
#  message("working on: ", files[i])
#  tic()
#  if(grepl(files[i], pattern = "inlandwater")){
#  HYD_sf <- read_sf(files[i]) %>%
#      st_cast("MULTIPOLYGON") %>%
#      st_zm(drop = TRUE, what = "ZM") %>% 
#      st_geometry() %>%
#      st_simplify(dTolerance = 10) %>%
#      sf::st_transform(crs = st_crs(lc))
#  }else{
#  HYD_sf <- read_sf(files[i]) %>%
#    st_cast("MULTILINESTRING") %>%
#    st_zm(drop = TRUE, what = "ZM") %>% 
#    st_geometry() %>%
#    st_simplify(dTolerance = 100) %>%
#    sf::st_transform(crs = st_crs(lc))
#  }
#    write_sf(HYD_sf, paste0("simp/simp_", files[i])) 
#   toc()
#  }
  
  simpfiles <- list.files("simp")
  
#  for(i in 1:length(simpfiles)){
  tic()
  message("Reading in simplified sf file...", simpfiles[i])      
  HYD_sf <- read_sf(paste0("simp/", simpfiles[i]))      

  # Find the nearest feature in sf2 for each feature in sf1
  message("Finding nearest indices...", simpfiles[i])    
  nearest_indices <- st_nearest_feature(lc_pts, HYD_sf)
  
  # Extract the nearest features from sf2
  message("Extracting nearest features...", simpfiles[i]) 
  nearest_features <- HYD_sf[nearest_indices, ]
  
  # Calculate the distances between each feature in sf1 and its nearest feature in sf2
  message("Calculating distances...", simpfiles[i]) 
  distances <- st_distance(lc_pts, nearest_features, by_element = TRUE)
  
  # Add the distances to the sf1 object (optional)
  lc_pts$nearest_distance <- as.numeric(distances)
  
  message("Writing gpkg file...", simpfiles[i]) 
  write_sf(lc_pts, paste0("distw/dist_", simpfiles[i]))
  toc()
#}
