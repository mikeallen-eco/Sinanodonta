library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(dismo)
library(rJava) 
# Note: need to install an x86_64 build of Java for rJava to work
# brew install --cask temurin
# R CMD javareconf
# install.packages("rJava", type = "source")
# Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home")
# export JAVA_HOME=$(/usr/libexec/java_home -v 17)
# sudo -E R CMD javareconf
# Note: I also had to download a version of R that matched the build of Java that was installed.
maxent()
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "conus_wc.tif")
rpath <- paste0(outpath, "conus_final_raster_stack.tif")
water_path <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/dist_w_mean_1500m.tif"

# load CONUS
us <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))

# read in CONUS environmental covariate data
env <- rast(rpath)

# load MaxEnt models
mod1 <- readRDS(paste0(outpath, "mod1_fewerBioclim.rds"))
mod2 <- readRDS(paste0(outpath, "mod2_fewerBioclim.rds"))
mod3 <- readRDS(paste0(outpath, "mod3_fewerBioclim.rds"))
mod4 <- readRDS(paste0(outpath, "mod4_fewerBioclim.rds"))
mod5 <- readRDS(paste0(outpath, "mod5_fewerBioclim.rds"))

# view the maxent models in a html brower
# show(mod1)
# show(mod2)
# show(mod3)
# show(mod4)
# show(mod5)

# make predictions for CONUS
ped1 <- predict(mod1,env)
ped2 <- predict(mod2,env)
ped3 <- predict(mod3,env)
ped4 <- predict(mod4,env)
ped5 <- predict(mod5,env)

# plotting points that are above the previously calculated
# thresholded value
plot(ped1)
plot(ped2)
plot(ped3)
plot(ped4)
plot(ped5)
plot(ped2 > 0.25)

# write rasters for individual model runs
ped_all <- c(ped1, ped2, ped3, ped4, ped5)
# writeRaster(ped_all, paste0(outpath, "conus_predictions_5mods.tif"), overwrite = T)

mean_pred <- mean(ped_all)
png("figures/USA_predictions_5mod_avg.600.png", width = 5*600, height = 5*600, res = 600)
plot(mean_pred)
dev.off()

plot(mean_pred)
plot((mean_pred > 0.25)*1)

# writeRaster(mean_pred, paste0(outpath, "conus_predictions_5mod_mean_fewerBioclim.tif"), overwrite = T)

mean_pred <- rast(paste0(outpath, "conus_predictions_5mod_mean_fewerBioclim.tif"))
nrow(xyFromCell(mean_pred, cells(mean_pred)))
water <- rast(water_path)
plot(water)

mean_pred_dws <- mean_pred %>%
  crop(water) %>%
  mask(water)

plot(mean_pred_dws)

png("figures/DWS_predictions_5mod_avg_fewerBioclim.png", width = 4*600, height = 6*600, res = 600)
plot(mean_pred_dws)
points(-74.916074, 40.547203, pch = 8, col = "red", cex = 1.5)
dev.off()
