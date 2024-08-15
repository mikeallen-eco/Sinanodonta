library(dplyr)
library(sf)
# library(ggplot2)
# library(rnaturalearth)
library(terra)
# library(exactextractr)
# library(fasterize)
# library(readxl)
library(tictoc)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- "conus_wc.tif" # paste0(outpath, "conus_wc.tif")
# dws_path <- "/Users/mikea/Documents/mikedata/cpm/202406/drbbnd/drb_bnd_polygon.shp"
# set path to geology shapefile
geo_path <- "Geology_CONUS.shp" # /Users/mikea/Documents/mikedata/cpm/202406/Geology_CONUS/
# download straight to cluster using the following:
  # wget https://www.sciencebase.gov/catalog/file/get/623a013ed34e915b67cddcfa?f=__disk__e0%2Fda%2F25%2Fe0da25413b2934188e338d3df8caa2f58fb0b5af
  # then needed to rename it as a zip file and unzip!
# set path to landcover tifs
landcov_path <- "nlcd_2021_land_cover_l48_20230630.img" # /Users/mikea/Documents/mikedata/cpm/202406/
# landcover data straight to cluster:
    # wget https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip

# read in conus worldclim & landcover rasters and geology shapefile
message("Reading worldclim raster for use as final template...")
wc <- rast(wc_path)[[1]]

message("Reading landcover raster...")
lc <- rast(landcov_path)

message("Aggregating landcover raster for use as a template...")
lc.tmp <- lc %>%
  aggregate(fact = 3)

message("Reading and formatting the geology shapefile...")
geo <- read_sf(geo_path) %>%
  arrange(CMMI_Class) %>%
  st_transform(st_crs(lc)) %>%
  mutate(geotype = case_when(grepl(CMMI_Class, pattern = "Igneous_|Metamorphic_") ~ "CR",
                          grepl(CMMI_Class, pattern = "Other_Unconsolidated") ~ "SP",
                          grepl(CMMI_Class, pattern = "Other_Melange|Sedimentary_Siliciclastic") ~ "SF",
                          CMMI_Class %in% "Sedimentary_Chemical" ~ "SF",
                          grepl(CMMI_Class, pattern = "Sedimentary_Chemical_") ~ "KA",
                          grepl(CMMI_Class, pattern = "Other_Unknown") ~ NA,
                          CMMI_Class %in% "Other_Water" ~ "SP",
                          TRUE ~ NA)) %>% 
  dplyr::select(geotype) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 100)
object.size(geo)

# make CONUS final geology categorical raster
# Create a new numeric field in the polygon data for categorical values
us_fin_vect <- terra::vect(geo)
us_fin_vect$type_code <- as.numeric(factor(us_fin_vect$geotype, levels = c("CR", "SP", "SF", "KA")))

# Rasterize the polygon based on the numeric codes
message("Rasterizing the geology shapefile to match the aggregated land cover raster...")
geo.r <- rasterize(us_fin_vect, lc.tmp, field = "type_code", 
                   touches = F)

# Assign the categorical levels to the raster
levels(geo.r) <- data.frame(ID = 1:4, category = c("CR", "SP", "SF", "KA"))

# project the raster to wc template
message("Projecting the geology raster to the final raster template...")
geo.r.fin <- project(geo.r, wc, method = "mode")
# plot(geo.r.fin)

geo.r.fin2 <- geo.r.fin %>%
  crop(wc) %>%
  mask(wc)
levels(geo.r.fin2) <- data.frame(ID = 1:4, category = c("CR", "SP", "SF", "KA"))
plot(geo.r.fin2)

writeRaster(geo.r.fin2, "conus_geology_cat.tif")

###
geo.r.fin2 <- rast("conus_geology_cat.tif")
cat_raster <- geo.r.fin2
levels <- unique(values(cat_raster))
print(levels)

binary_rasters <- lapply(levels[2:5], function(level) {
  binary_raster <- cat_raster == level
  binary_raster <- binary_raster*1
  the_name <- levels(geo.r.fin2)[[1]][level,2]
  names(binary_raster) <- the_name  # Name the layer with the level
  return(binary_raster)
}); binary_rasters

finr <- rast(binary_rasters)
# plot(finr[[4]])

writeRaster(finr, "conus_geology.tif", overwrite = TRUE)

finr <- rast(paste0(outpath, "conus_geology.tif"))
finr
plot(finr[[1]])
plot(finr[[2]])
plot(finr[[3]])
plot(finr[[4]])

