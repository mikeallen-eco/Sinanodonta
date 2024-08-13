library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(exactextractr)
library(fasterize)
library(readxl)
library(tictoc)
# load Europe
source("R/load_Europe.R")
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "europe_wc.tif")
# set path to landcover tifs
landcov_path <- "/Users/mikea/Documents/mikedata/cpm/202405/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"

# read in conus worldclim & landcover rasters and geology shapefile
wc <- rast(wc_path)
lc <- rast(landcov_path)

# Get 1:1.5M geology data for Europe (excluding Belarus, Moldova, Ukraine)
  # from OneGeology Europe data source published by BRGM / GISEurope
  # layer name: GISEurope Bedrock and Structural geology
  # WMS link:
    # http://mapsref.brgm.fr/wxs/1GG/GISEurope_Bedrock_and_Structural_Geology?request=GetCapabilities&service=WFS
    # link obtained in metadata tab of this site: https://portal.onegeology.org/OnegeologyGlobal/
  # note: broke into 3 chunks to facilitate processing (manageable amounts of RAM)
  # note: performed the GIS operations on the Amarel cluster
# Define the bounding box: xmin, ymin, xmax, ymax
library(sf)
library(dplyr)
message("Reading Europe clip file...")
eu <- read_sf("data/eu_clip.gpkg")

# bbox <- c(xmin = -25, ymin = 31, xmax = 45, ymax = 70) # full
bbox.L <- c(xmin = -25, ymin = 31, xmax = 5, ymax = 70) # left slice
bbox.M <- c(xmin = 5, ymin = 31, xmax = 20, ymax = 70) # middle slice
bbox.R <- c(xmin = 20, ymin = 31, xmax = 45, ymax = 70) # right slice

# Construct the WFS request URL with BBOX parameter
wfs_url.L <- paste0(
  "http://mapsref.brgm.fr/wxs/1GG/GISEurope_Bedrock_and_Structural_Geology?",
  "service=WFS&version=1.0.0&request=GetFeature&typeName=ms:Europe_GISEurope_1500K_BedrockAge&",
  "bbox=", paste(bbox.L, collapse = ",")
)

wfs_url.M <- paste0(
  "http://mapsref.brgm.fr/wxs/1GG/GISEurope_Bedrock_and_Structural_Geology?",
  "service=WFS&version=1.0.0&request=GetFeature&typeName=ms:Europe_GISEurope_1500K_BedrockAge&",
  "bbox=", paste(bbox.M, collapse = ",")
)

wfs_url.R <- paste0(
  "http://mapsref.brgm.fr/wxs/1GG/GISEurope_Bedrock_and_Structural_Geology?",
  "service=WFS&version=1.0.0&request=GetFeature&typeName=ms:Europe_GISEurope_1500K_BedrockAge&",
  "bbox=", paste(bbox.R, collapse = ",")
)

# Read the data within the bounding box
message("Subsetting left panel...")
geodata_subset.L <- st_read(wfs_url.L, crs = 4326)

message("Subsetting middle panel...")
geodata_subset.M <- st_read(wfs_url.M, crs = 4326)

message("Subsetting right panel...")
geodata_subset.R <- st_read(wfs_url.R, crs = 4326)

# message("Writing left panel...")
write_sf(geodata_subset.L, "Europe_GISEurope_1500K_BedrockAge.L.gpkg")
message("Writing middle panel...")
write_sf(geodata_subset.M, "Europe_GISEurope_1500K_BedrockAge.M.gpkg")
message("Writing right panel...")
write_sf(geodata_subset.R, "Europe_GISEurope_1500K_BedrockAge.R.gpkg")

message("Reading Europe geology (left panel)...")
geoL <- read_sf("/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/Europe_GISEurope_1500K_BedrockAge.L.gpkg") %>% # /Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/
  dplyr::select(type = desc_litho_en) %>%
  st_make_valid() %>%
  st_intersection(eu) %>%
  st_simplify(dTolerance = 100) %>%
  group_by(type) %>%
  summarize(geometry = st_union(geom), .groups = "drop") %>%
  mutate(id = paste0("L_", row_number()))
nrow(geoL); object.size(geoL)

geoL_df <- geoL %>%
  st_drop_geometry() %>%
  as.data.frame()
object.size(geoL_df)

message("Reading Europe geology (middle panel)...")
geoM <- read_sf("Europe_GISEurope_1500K_BedrockAge.M.gpkg") %>% # /Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/
  dplyr::select(type = desc_litho_en) %>%
  st_make_valid() %>%
  st_intersection(eu) %>%
  st_simplify(dTolerance = 100) %>%
  group_by(type) %>%
  summarize(geometry = st_union(geom), .groups = "drop") %>%
  mutate(id = paste0("M_", row_number()))
object.size(geoM)

geoM_df <- geoM %>%
  st_drop_geometry() %>%
  as.data.frame()
object.size(geoM_df)

message("Reading Europe geology (right panel)...")
geoR <- read_sf("Europe_GISEurope_1500K_BedrockAge.R.gpkg") %>% # /Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/
  dplyr::select(type = desc_litho_en) %>%
  st_make_valid() %>%
  st_intersection(eu) %>%
  st_simplify(dTolerance = 100) %>%
  group_by(type) %>%
  summarize(geometry = st_union(geom), .groups = "drop") %>%
  mutate(id = paste0("R_", row_number()))
object.size(geoR)

geoR_df <- geoR %>%
  st_drop_geometry() %>%
  as.data.frame()
object.size(geoR_df)

geo_df <- geoL_df %>%
  bind_rows(geoM_df, geoR_df)
write.csv(geo_df, "EU_rock_types.all.csv", row.names = F)

message("Merging 3 Europe geology files...")
geo <- st_union(geoL, geoM, geoL)

message("Writing merged Europe geology file...")
write_sf(geo, "geology_europe_hires.gpkg")


ggplot() +
  geom_sf(data = geo.c.dis, aes(fill = cat)) +
  geom_sf(data = eu, fill = "transparent", color = "red")

## World Geology Data - Europe
# Get 1:50M (large-scale) geologic data for parts of Europe without data (Ukraine, etc.)
# http://mapsref.brgm.fr/wxs/1GG/CGMW_Bedrock_and_Structural_Geology?request=GetCapabilities&service=WFS
# layers:
# ms:Eurasia_CGMW_12500K_GeologicalUnits
# ms:World_CGMW_50M_GeologicalUnitsOnshore
# ms:World_CGMW_50M_OverFeatures
# ms:Eurasia_CGMW_12500K_OverFeatures
bbox <- c(xmin = -25, ymin = 31, xmax = 45, ymax = 70) # full
wfs_url_world <- paste0(
  "http://mapsref.brgm.fr/wxs/1GG/CGMW_Bedrock_and_Structural_Geology?",
  "service=WFS&version=1.0.0&request=GetFeature&typeName=ms:World_CGMW_50M_GeologicalUnitsOnshore&",
  "bbox=", paste(bbox, collapse = ",")
)
geodata_world <- st_read(wfs_url_world, crs = 4326)
# write_sf(geodata_world, "World_CGMW_50M_GeologicalUnitsOnshore.gpkg")

wgeo <- read_sf("/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/World_CGMW_50M_GeologicalUnitsOnshore.gpkg") %>% # /Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/
  st_make_valid() %>%
  st_intersection(eu2) %>%
  st_simplify(dTolerance = 100) %>%
  filter(sovereignt %in% c("Ukraine", "Belarus", "Moldova", "Iceland")) %>%
  dplyr::select(type = LITHO_EN) %>%
  mutate(cat = case_when(grepl(type, pattern = "Undifferentiated") ~ "SP",
                         grepl(type, pattern = "undifferentiated") ~ "SP",
                         grepl(type, pattern = "Sedimentary rocks") ~ "SF",
                         grepl(type, pattern = "Extrusive") ~ "CR",
                         grepl(type, pattern = "Endogenous|Oceanic crust") ~ "CR",
                         is.na(type) ~ "SP"))
table(wgeo$cat)

# writing formatted world geology file
message("Writing world geology file...")
write_sf(wgeo, "/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/geology_europe_lowres.gpkg")

message("Recoding Europe geology file into 4 simple categories...")
eu_hi <- read_sf("/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/Europe_GISEurope_1500K_BedrockAge.L_formatted.gpkg")  %>%
  bind_rows(read_sf("/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/Europe_GISEurope_1500K_BedrockAge.M_formatted.gpkg")) %>%
  bind_rows(read_sf("/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/Europe_GISEurope_1500K_BedrockAge.R_formatted.gpkg")) %>%
  mutate(cat = case_when(grepl(type, pattern = "Gneis|Lava|Volcan|Basalt|Pluton|Granit|Schist|gneiss") ~ "CR",
                         grepl(type, pattern = "volcan|Amphibolite|Andesite|Anorthosite|Basanite") ~ "CR",
                         grepl(type, pattern = "schist|basalt|Diorite|Dolerite|Gabbro|Migmatite") ~ "CR",
                         grepl(type, pattern = "Granodiorite|Ignimbrite|Impactite|Igneous|granite") ~ "CR",
                         grepl(type, pattern = "Nepheline|Mugearite|quartzdiorite|Leptynite") ~ "CR",
                         grepl(type, pattern = "Metasediments|Quartzite|Rhyolite|Granulite") ~ "CR",
                         grepl(type, pattern = "Gneisse|Lava|Acid Volcanic|Meta-gabbro|Pyroclastic") ~ "CR",
                         grepl(type, pattern = "Metamorphic|Meta-granodiorite|Meta-sediment") ~ "CR",
                         grepl(type, pattern = "Ophiolite|Monzonite|Mylonite|Phonolite") ~ "CR",
                         grepl(type, pattern = "Quartzdiorite|Serpentinite|Microdiorite|Diabase") ~ "CR",
                         grepl(type, pattern = "Pelite|Phyllite|Spilite|Syenite|Tectonic") ~ "CR",
                         grepl(type, pattern = "Ultrabasic|Tonalite|Amphibole|Meta-andesite") ~ "CR",
                         grepl(type, pattern = "Dunite|Harzburgite|Pyroxenite|Wehrlite") ~ "CR",
                         grepl(type, pattern = "Clay, Sand, Poorly Consolidated Sandstone") ~ "SP",
                         grepl(type, pattern = "Sand, Sandstone, Clays|Sand, Sandstones") ~ "SP",
                         grepl(type, pattern = "Gravel, Sand, Sandstone, Argillaceou Sandstone, Coal") ~ "SP",
                         grepl(type, pattern = "ypsum|Basic Agglomerate|Carbonate|Chalk|Calcareou") ~ "KA",
                         grepl(type, pattern = "Carbonatite") ~ "KA",
                         grepl(type, pattern = "Limestone|Marl|Dolomite|Marble|Evaporite") ~ "KA",
                         grepl(type, pattern = "Tuff|tuff|Shale|Sandstone|Slate|Molasses") ~ "SF",
                         grepl(type, pattern = "Turbidite|Flysh") ~ "SF",
                         grepl(type, pattern = "Aeolian|Clay|Marine Deposits|Lake Sediment") ~ "SP",
                         grepl(type, pattern = "Mud, Silt|Peatland|Sand, Gravel, Pebbles") ~ "SP",
                         grepl(type, pattern = "Sand, Silts|Undifferentiated Sediment") ~ "SP",
                         grepl(type, pattern = "Alluvium|Glacial Deposits|Moraine|Peat") ~ "SP",
                         grepl(type, pattern = "Glacial Sediment|Loess|Sand, Gravel") ~ "SP",
                         grepl(type, pattern = "Conglomerate|Unconsolidated Marine|Radiolarian") ~ "SF",
                         grepl(type, pattern = "Sand|Sediment") ~ "SP",
                         is.na(type) ~ "other")) %>%
  dplyr::select(-id)
table(eu_hi$cat); sum(is.na(eu_hi$cat))
head(eu_hi)

eu_lo <- st_read("/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/geology_europe_lowres.gpkg")

eu_fin <- bind_rows(eu_hi, eu_lo) %>%
  group_by(cat) %>%
  summarize(geometry = st_union(geom), .groups = "drop")

eu_fin <- eu_fin %>%
  st_make_valid()

eu_fin <- eu_fin[!st_is_empty(eu_fin), ]

head(eu_fin)
table(eu_fin$cat); object.size(eu_fin)
write_sf(eu_fin, "/Users/mikea/Documents/mikedata/cpm/202406/Geology_EU/geology_europe_final.gpkg")

eu_fin_simp <- eu_fin %>%
  st_simplify(dTolerance = 1000)

# plot eu_fin
ggplot() +
  geom_sf(data = eu_fin_simp, aes(fill = cat)) +
  geom_sf(data = eu2, fill = "transparent", color = "red")
object.size(eu_fin_simp)

message("Extracting geology attributes for each CPM observation and writing file...")

### read in environmental covariate data
points <- read.csv("data/final_EU_cpm_data.csv") %>%
  st_as_sf(., coords = c("lon", "lat"),
           crs = 4326) 

points_with_attributes <- st_join(points, eu_fin, left = TRUE)
write_sf(points_with_attributes, "data/Swood_points_w_geology_attributes.gpkg")  

# make EU final geology categorical raster
# Create a new numeric field in the polygon data for categorical values
library(sp)
# eu_fin_sp <- as(dplyr::select(eu_fin, cat), "Spatial")
eu_fin_vect <- terra::vect(eu_fin)
eu_fin_vect$type_code <- as.numeric(factor(eu_fin_vect$cat, levels = c("CR", "SP", "SF", "KA")))

# Rasterize the polygon based on the numeric codes without the `touches` option
geo.r <- rasterize(eu_fin_vect, rast(wc_path)[[1]], field = "type_code", 
                   touches = F)
# Assign the categorical levels to the raster
levels(geo.r) <- data.frame(ID = 1:4, category = c("CR", "SP", "SF", "KA"))
plot(geo.r)

# writeRaster(geo.r, paste0(outpath, "europe_geology.tif"))

test <- extract(geo.r, vect(points_with_attributes), na.rm = T)

test2 <- cbind(points_with_attributes, test)

plot(wc[[1]])
plot(ue[,"LITHO_EN"], add = T)
class(wgeo)

head(wgeo)
table(wgeo$LITHO_EN)
table(wgeo$DESCR_EN)

                               
table(wgeo.c$cat); sum(is.na(wgeo.c$cat))
wgeo.c.dis <- wgeo.c %>%
  group_by(cat) %>%
  summarize(geometry = st_union(geom), .groups = "drop")



geo.all <- geo.c.dis %>%
  st_union(wgeo.c.dis)

ggplot() +
  geom_sf(data = geo.all, aes(fill = cat)) +
  geom_sf(data = eu, fill = "transparent", color = "red")

# Example points data
points_sf <- st_as_sf(data.frame(
  id = 1:3,
  lon = c(10, 12, 14),
  lat = c(45, 47, 49)
), coords = c("lon", "lat"), crs = 4326)

# Spatial join: attach polygon attributes to points
joined_data <- st_join(points_sf, geodata)

# Extract specific attribute (replace 'attribute_name' with actual attribute name)
extracted_values <- joined_data$attribute_name

# View results
print(joined_data)
print(extracted_values)




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
