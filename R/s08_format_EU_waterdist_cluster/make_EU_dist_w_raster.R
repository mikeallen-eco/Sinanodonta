library(dplyr)
library(sf)
library(terra)
library(tictoc)
library(parallel)
library(data.table)
wc_path <- "europe_wc.tif"


# read in EU landcover raster
message("Reading in EU landcover raster template...")
lc <- rast("europe_lc_binary2.tif")

#message("Aggregating EU landcover raster template to 300 m...")
#dist_temp <- lc[[1]] %>%
#  aggregate(fact = 3)

#message("Writing aggregated EU landcover raster template to file...")
#writeRaster(dist_temp, "europe_lc_template.300m.agg.tif", overwrite = TRUE)

message("Reading aggregated EU landcover raster template file...")
dist_temp <- rast("europe_lc_template.300m.agg.tif")

# Get the coordinates of the center of each non-NA cell for the worldclim raster
message("Getting coordinates for each non-NA cell in EU landcover raster template...")
lc_pts = terra::xyFromCell(dist_temp, cells(dist_temp)) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(lc))

# select minimum distance per point
#message("Getting minimum distance per point for all .gpkg files...")
#dist_list <- list()
#dist_files <- list.files("distw/", pattern = ".gpkg", full.names = T)
#for(j in 1:length(dist_files)){
#message("working on ", j, " of ", length(dist_files), ": reading & processing ", dist_files[j])
#tic()
#if(j %in% 1){
#dist_w <- read_sf(dist_files[j]) %>%
#  sf::st_drop_geometry() %>%
#  dplyr::select(1:3)}else{
#    dist_w <- read_sf(dist_files[j]) %>%
#      sf::st_drop_geometry() %>%
#      dplyr::select(nearest_distance)
#    }
#write.csv(dist_w, paste0("tmp_", j, ".csv"), row.names = F)
#rm(dist_w)
#gc()
#
#toc() # ~ 700 s for each loop iteration
#}

#message("Compiling all distw files into one dataframe...")

setDTthreads(20)
#tmp1 <- fread("tmp_1")
#tmp2 <- fread("tmp_2")
#tmp3 <- fread("tmp_3")
#tmp4 <- fread("tmp_4")
#tmp5 <- fread("tmp_5")
#tmp6 <- fread("tmp_6")
#tmp7 <- fread("tmp_7")
#tmp8 <- fread("tmp_8")
#tmp9 <- fread("tmp_9")
#tmp10 <- fread("tmp_10")
#dist_w_df <- cbind.data.frame(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10)
#rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10); gc()
#colnames(dist_w_df) <- c("coordX", "coordY", LETTERS[3:(length(tmpfiles)+2)])

# write.csv(dist_w_df, "dist_w_df.csv", row.names = F)

# read in the combined distance-to-water file
dist_w_df <- fread("dist_w_df.csv")

message("Calculating the minimum distance to water across all files...")
tic()
#dist_w_min <- dist_w_df %>%
#  rowwise() %>%
#  mutate(min_dist = min(c_across(3:(length(tmpfiles)+2)), na.rm = T)) %>%
#  ungroup() %>%
#  dplyr::select(coordX, coordX, min_dist)

#####
dist_w_df$min_dist <- apply(dist_w_df[, 3:12], 1, min, na.rm = TRUE)

dist_w_min <- dist_w_df %>%
  dplyr::select(coordX, coordY, min_dist)

####

# Number of cores to use
#num_cores <- detectCores() - 4

#dist_w_df$min_dist <- unlist(mclapply(1:nrow(dist_w_df), function(i) {
#  min(dist_w_df[i, 3:(length(tmpfiles) + 2)], na.rm = TRUE)
#}, mc.cores = num_cores))

#dist_w_min <- dist_w_df %>%
#  dplyr::select(coordX, coordY, min_dist)

write.csv(dist_w_min, "dist_w_min.csv", row.names = F)
toc()

# read in reduced "minimum distance to water" data file
dist_w_min <- fread("dist_w_min.csv")

# populate 300 m raster with the minimum values
message("Populating 300 m raster with the minimum values...")
dist_w_raster <- dist_temp; rm(dist_temp); gc()
dist_w_raster[cells(dist_w_raster)] <- dist_w_min$min_dist

# Get the coordinates of the center of each non-NA cell for the worldclim raster
  # make 1500 m buffer around raster points
wc <- rast(wc_path)[[1]]

message("Making 1500 m buffer around wc raster points...")
buffer_pts = terra::xyFromCell(wc, cells(wc)) %>%
  as.data.frame() %>%
  mutate(lon = x, lat = y) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(wc)) %>%
  # project to lc projection in meters
  st_transform(crs = st_crs(lc))

buffers <- buffer_pts %>%
  st_buffer(., dist = 1500)

message("Extracting average minimum distance to water for each buffer...")
# extract average minimum distance to water for use in the final prediction surface
tic()
vals <- exactextractr::exact_extract(dist_w_raster, buffers, fun = 'mean')
toc()

message("Adding average minimum distance to water values to raster...")
dist_w_final <- wc
dist_w_final[cells(dist_w_final)] <- vals
names(dist_w_final) <- "dist_water"

message("Writing final minimum distance to water raster...")
writeRaster(dist_w_final, "europe_dist_w_mean_1500m.tif", overwrite = TRUE)