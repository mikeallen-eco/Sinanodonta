library(dplyr)
library(sf)
library(terra)
library(tictoc)
sin = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

# read in EU landcover raster
lc <- rast("europe_lc_binary2.tif")

# loop through water files and calculate distance raster for each
files <- list.files(pattern = ".gpkg")

for(i in 1:length(files)){
  message("working on: ", files[i])
  tic()
  if(grepl(files[i], pattern = "inlandwater")){
  HYD_sf <- read_sf(files[i]) %>%
      st_cast("MULTIPOLYGON") %>%
      st_zm(drop = TRUE, what = "ZM") %>% 
      st_geometry() %>%
      st_simplify(dTolerance = 10) %>%
      sf::st_transform(crs = st_crs(lc))
  }else{
  HYD_sf <- read_sf(files[i]) %>%
    st_cast("MULTILINESTRING") %>%
    st_zm(drop = TRUE, what = "ZM") %>% 
    st_geometry() %>%
    st_simplify(dTolerance = 100) %>%
    sf::st_transform(crs = st_crs(lc))
  }
 
  write_sf(HYD_sf, paste0("simp/simp_", files[i])) 
}
