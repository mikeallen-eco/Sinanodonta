library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(exactextractr)
library(fasterize)
library(readxl)
library(tictoc)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
nj_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_New_Jersey_State_GPKG/NHD_H_New_Jersey_State_GPKG.gpkg"
de_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_Delaware_State_GPKG/NHD_H_Delaware_State_GPKG.gpkg"
ny_path <- "/Users/mikea/Documents/mikedata/cpm/202405/NHD_H_New_Jersey_State_GPKG/NHD_H_New_York_State_GPKG.gpkg"
st_layers(nj_path) 
# want: NHDArea, NHDFlowline, NHDLine, NHDWaterbody
# layer_name                 geometry_type features fields crs_name
# 1                  NHDArea              3D Multi Polygon     2657     15    NAD83
# 2           NHDAreaEventFC                 Multi Polygon        0     17    NAD83
# 3              NHDFlowline 3D Measured Multi Line String   321253     21    NAD83
# 4                  NHDLine          3D Multi Line String     8977     13    NAD83
# 5           NHDLineEventFC             Multi Line String        0     19    NAD83
# 6                 NHDPoint                      3D Point      794     10    NAD83
# 7          NHDPointEventFC                         Point     5987     17    NAD83
# 8             NHDWaterbody              3D Multi Polygon    46790     16    NAD83
# 9                  WBDHU10                 Multi Polygon       85     17    NAD83
# 10                 WBDHU12                 Multi Polygon      425     20    NAD83
# 11                 WBDHU14                 Multi Polygon        0     20    NAD83
# 12                 WBDHU16                 Multi Polygon        0     20    NAD83
# 13                  WBDHU2                 Multi Polygon        1     14    NAD83
# 14                  WBDHU4                 Multi Polygon        3     14    NAD83
# 15                  WBDHU6                 Multi Polygon        5     14    NAD83
# 16                  WBDHU8                 Multi Polygon       14     14    NAD83
# 17                 WBDLine             Multi Line String     2119      8    NAD83

nj_line <- read_sf(nj_path, layer = "NHDFlowline") %>%
  st_zm(drop = TRUE, what = "ZM")
ny <- read_sf(ny_path)
pa <- read_sf(pa_path)

plot(nj[,"permanent_identifier"])

dws_path <- "/Users/mikea/Documents/mikedata/cpm/202406/drbbnd/drb_bnd_polygon.shp"
# set path to geology shapefile
geo_path <- "/Users/mikea/Documents/mikedata/cpm/202406/Geology_CONUS/Geology_CONUS.shp"
# set path to landcover tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202406/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img"

# read in conus worldclim & landcover rasters and geology shapefile
wc <- rast(wc_path)
lc <- rast(landcov_path)
geo <- read_sf(geo_path) %>%
  arrange(CMMI_Class) %>%
  mutate(CMMI_Class2 = as.numeric(as.factor(CMMI_Class))) %>%
  st_transform(st_crs(lc))

# convert geology shapefile to raster (same extent/projection/resolution as landcover)
tic()
geo_vect <- vect(geo)
geo.r <- terra::rasterize(geo_vect, lc, field = "CMMI_Class2")
writeRaster(geo.r, paste0(outpath, "conus_geology.tif"), overwrite = TRUE)
toc() # 1353.884 sec elapsed

# to examine components of each CMMI Class
unique(geo[geo$CMMI_Class=="Other_Melange",]$UNIT_NAME)
unique(geo[geo$CMMI_Class=="Other_Unknown",]$UNIT_NAME)
unique(geo[geo$CMMI_Class=="Other_Unconsolidated",]$UNIT_NAME)
unique(geo[geo$CMMI_Class=="Sedimentary_Chemical",]$UNIT_NAME)
unique(geo[geo$CMMI_Class=="Sedimentary_Chemical_Evaporite",]$UNIT_NAME)

# make reclassification table to collapse categories
reclass_df <- geo %>%
  as.data.frame() %>%
  select(class = CMMI_Class, value = CMMI_Class2) %>%
  distinct() %>%
  mutate(chrystaline = case_when(grepl(class, pattern = "Igneous_|Metamorphic_") ~ 1,
                                TRUE ~ 0),
         sed.porous = case_when(grepl(class, pattern = "Other_Unconsolidated") ~ 1,
                                 TRUE ~ 0),
         sed.fract = case_when(grepl(class, pattern = "Other_Melange|Sedimentary_Siliciclastic") ~ 1,
                               class %in% "Sedimentary_Chemical" ~ 1,
                                TRUE ~ 0),
         karst = case_when(grepl(class, pattern = "Sedimentary_Chemical_") ~ 1,
                               TRUE ~ 0),
         unknown = case_when(grepl(class, pattern = "Other_Unknown") ~ 1,
                           TRUE ~ 0))

# write.csv(reclass_df, "output/conus_geology_reclass.csv")

# make final reclassification matrices
chrystaline_remat <- reclass_df %>%
  dplyr::select(value, chrystaline) %>%
  as.matrix

sed.porous_remat <- reclass_df %>%
  dplyr::select(value, sed.porous) %>%
  as.matrix

sed.fract_remat <- reclass_df %>%
  dplyr::select(value, sed.fract) %>%
  as.matrix

karst_remat <- reclass_df %>%
  dplyr::select(value, karst) %>%
  as.matrix

unknown_remat <- reclass_df %>%
  dplyr::select(value, unknown) %>%
  as.matrix

# read in rasterized geology data
geo.r <- rast(paste0(outpath, "conus_geology.tif"))

# reclassify geology raster into binary rasters by collapsed category (~ 30 min total run time)
tic()
chrystaline <- classify(geo.r, chrystaline_remat)
toc() # 398.996 sec elapsed
sed.porous <- classify(geo.r, sed.porous_remat)
sed.fract <- classify(geo.r, sed.fract_remat)
karst <- classify(geo.r, karst_remat)
tic()
unknown <- classify(geo.r, unknown_remat)
toc() # 344.197 sec elapsed
tic()
writeRaster(unknown, paste0(outpath, "conus_geology_unknown_binary.tif"), overwrite = TRUE)
toc() # 243.425 sec elapsed
geo.r.binary <- c(chrystaline, sed.porous, sed.fract, karst)
names(geo.r.binary) <- c("chrystaline", "sed.porous", "sed.fract", "karst")
tic()
# writeRaster(geo.r.binary, paste0(outpath, "conus_geology_binary.tif"), overwrite = TRUE)
toc() # 1269.585 sec elapsed

# get center points for all wc raster cells and make a 2500 m buffer around each one for area calcs

# Get the coordinates of the center of each non-NA cell for the worldclim raster
buffer_pts = terra::xyFromCell(wc[[1]], cells(wc[[1]])) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(wc)) %>%
  # project to sinusoidal projection in meters
  st_transform(crs = st_crs(lc))

tic()
buffers <- buffer_pts %>%
  st_buffer(., dist = 2500)
toc() # 9.21 sec elapsed

# extract surface geology proportions 

# load binary geology raster stack
geo.r.binary <- rast(paste0(outpath, "conus_geology_binary.tif"))

for(i in 4:dim(geo.r.binary)[3]){ 
  nm <- names(geo.r.binary)[i]
  message(paste0("Processing ", i, " of ", dim(geo.r.binary)[3], ": ", nm))
  tic()
  vals <- exactextractr::exact_extract(geo.r.binary[[i]], buffers, fun = 'mean')
  
  geology.p <- wc[[1]]
  geology.p[cells(geology.p)] <- vals
  writeRaster(geology.p, paste0(outpath, "conus_geology_", nm, "_p2500.tif"), overwrite = TRUE)
  toc() # 16626.6 sec elapsed
}
