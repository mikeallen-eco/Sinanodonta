library(sf)
library(terra)
library(rnaturalearth)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
elev_path <- "/Users/mikea/Documents/mikedata/cpm/202406/worldclim_elev_30s/wc2.1_30s_elev.tif"
wc_path <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/conus_wc.tif"

# load CONUS
us <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))

# test plot CONUS region
plot(us[,"name"])

# load the elevation raster (30 s)
elev <- rast(elev_path); elev
plot(elev)

# load the worldclim template raster
wc <- rast(wc_path)[[1]]

# reproject elevation raster, averaging it into coarser resolution
el_final <- project(elev, wc, method = "average") %>%
  crop(wc) %>%
  mask(wc)

plot(wc)
plot(el_final)

writeRaster(el_final, paste0(outpath, "conus_elev.tif"))
