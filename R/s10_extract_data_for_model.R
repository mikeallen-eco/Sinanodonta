library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(readxl)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "europe_wc.tif")

# load Europe
source("R/load_Europe.R")

### read in CPM observations
cenv <- read_xlsx("data/Swood_new_arrangement.xlsx", 
                  sheet = "Data_all_New_shape")

# make spatial to view point distribution
pts <- cenv %>%
  filter(Scen %in% 2,
         Time %in% 1) %>%
  mutate(lat = Lat, lon = Long) %>%
  st_as_sf(., # first argument = data frame with coordinates
           coords = c("Long", "Lat"), # name of columns, in quotation marks
           crs = 4326)

### read in EUROPE covariate data and make into a single rasterstack
wc <- rast(wc_path)


# format landcover rasters

lc_for <- rast(paste0(outpath, "europe_lc_forest_p1500.tif"))
names(lc_for) <- "forest_p1500m"
lc_grass <- rast(paste0(outpath, "europe_lc_grass.nonag_p2500.tif"))
names(lc_grass) <- "grass_p1500m"
lc_ag <- rast(paste0(outpath, "europe_lc_grass.ag_p1500.tif")) + 
    rast(paste0(outpath, "europe_lc_grass.ag_p1500.tif"))
names(lc_ag) <- "ag_p1500m"
lc_bare <- rast(paste0(outpath, "europe_lc_bare_p1500.tif"))
names(lc_bare) <- "bare_p1500m"
lc_wet <- rast(paste0(outpath, "europe_lc_cwet_p1500.tif")) + 
    rast(paste0(outpath, "europe_lc_fwet_p1500.tif"))
names(lc_wet) <- "wet_p1500m"
lc_urban <- rast(paste0(outpath, "europe_lc_urban_p1500.tif"))
names(lc_urban) <- "urban_p1500m"

raster_stack <- c(wc, lc_for, lc_grass, lc_ag, lc_bare, lc_wet, lc_urban) 
names(raster_stack)[1:19] <- c("bio1", "bio10", "bio11", "bio12", "bio13",
                               "bio14", "bio15", "bio16", "bio17", "bio18",
                               "bio19", "bio2", "bio3", "bio4", "bio5", 
                               "bio6", "bio7", "bio8", "bio9")
names(raster_stack)
# writeRaster(raster_stack, paste0(outpath, "europe_final_raster_stack.tif"))

### exctract environmental covariate values from EUROPE raster stack
values <- terra::extract(raster_stack, vect(pts))

# test plots
plot(raster_stack[[16]])
plot(eu2[,"sovereignt"], add = T, color = "none")
plot(pts[, "OProb"], add = T)

# make final dataframe for modeling
env <- pts %>%
  left_join(values, by = join_by(ID)) 

env_df <- env %>%
  dplyr::select(lat, lon, Elev, GeoNew, bio1:urban_p1500m) %>%
  st_drop_geometry() %>%
  as.data.frame()

# note: bio6 matches up exactly with the TMinCold variable provided with the data set (good)
ggplot() +
  geom_sf(data = eu2) +
  geom_sf(data = env, aes(color = bio6)) +
  scale_color_viridis_c()

names(env_df)
write.csv(env_df, "data/final_EU_cpm_data.csv", row.names = F)


