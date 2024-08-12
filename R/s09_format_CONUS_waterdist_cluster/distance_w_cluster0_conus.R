### MAKE SIMPLIFIED VERSIONS OF ALL THE WATER FILES TO REDUCE MEMORY REQUIREMENTS
library(dplyr)
library(sf)
library(terra)
library(tictoc)

# bash code for cluster
# wget https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/National/GPKG/NHD_H_National_GPKG.zip
# unzip NHD_H_National_GPKG.zip

# create directory for simplified combined water files
if(!dir.exists("simp")){dir.create("simp")}

# read in EU landcover raster
lc <- rast("conus_lc_snow_binary.tif")

# get water file names
  # NOTE: download national gpkg files to folder from cluster first using wget command above
linefiles <- list.files(".", pattern = "line.gpkg")
areafiles <- list.files(".", pattern = "area.gpkg")
flinefiles <- list.files(".", pattern = "fline.gpkg")
waterbodyfiles <- list.files(".", pattern = "waterbody.gpkg")

# get conus for clipping
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska")) %>%
  st_union() %>%
  st_transform(crs = st_crs(read_sf("/Users/mikea/Documents/mikedata/cpm/202406/finaldata/del_watershed_water_files/de_area.gpkg"))) %>%
  st_buffer(dist = 500000); plot(conus)
  
combined_line <- lapply(linefiles, read_sf) %>%
  do.call(rbind, .) %>% 
  st_union() %>%
  st_intersection(., conus)
write_sf(combined_line, "combined_line.gpkg")

combined_area <- lapply(areafiles, read_sf) %>%
  do.call(rbind, .) %>% 
  st_union()
write_sf(combined_area, "combined_area.gpkg")

combined_fline <- lapply(flinefiles, read_sf) %>%
  do.call(rbind, .) %>% 
  st_union()
write_sf(combined_fline, "combined_fline.gpkg")

combined_waterbody <- lapply(waterbodyfiles, read_sf) %>%
  do.call(rbind, .) %>% 
  st_union()
write_sf(combined_waterbody, "combined_waterbody.gpkg")

files <- list.files(".", pattern = "combined*")
for(i in 1:length(files)){
  message("working on: ", files[i])
  tic()
  if(grepl(files[i], pattern = "waterbody|area")){
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
