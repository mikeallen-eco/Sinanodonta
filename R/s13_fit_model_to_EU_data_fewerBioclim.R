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
wc_path <- paste0(outpath, "europe_wc.tif")
rpath <- paste0(outpath, "europe_final_raster_stack.tif")
# load Europe
source("R/load_Europe.R")
eu_minus <- eu2 %>%
  filter(!sovereignt %in% c("Ukraine", "Moldova", "Belarus"))

### read in environmental covariate data
points <- read.csv("data/final_EU_cpm_data.csv") %>%
  st_as_sf(., coords = c("lon", "lat"),
           crs = 4326) 

env <- rast(rpath)

# this creates a 150 km buffer
# that equates to about a 4-decimal-degree buffer per https://www.johndcook.com/how_big_is_a_degree.html
# occurrence data
occ_buff <- points %>%
  # st_transform(crs = sin) %>%
  st_buffer(., dist = 150000) %>%
  st_union() %>%
  st_make_valid() %>%
  st_intersection(eu_minus)

# plot Europe with buffer "study area" and observation points
plot(rast(wc_path)[[16]]) #, xlim = c(-109, -50), ylim = c(15, 50))
plot(eu2[,"sovereignt"], add = T, outline = "gray", color = "none")
plot(occ_buff, add = T, border="red",)  # adds buffer polygon to the plot
plot(points, add = T, col = "gray", pch=4, size =1)  # adds occurrence data to the plot

# crop study area to a manageable extent (rectangle shaped)
studyArea <- terra::crop(env, ext(as(occ_buff, "Spatial")))  

# the 'study area' created by extracting the buffer area from the raster stack
studyArea <- mask(studyArea, vect(occ_buff))
# output will still be a raster stack, just of the study area
plot(studyArea[[20]])
plot(points, add = T, col = "red", pch=4, size =.5)  # adds occurrence data to the plot

# select background points from this buffered area
numbg <- 10000

set.seed(44); bg1 <- spatSample(x = studyArea,
                              size = numbg,
                              method = "random",
                              as.points = TRUE,
                              na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(id = as.numeric(paste0(9999, 1:numbg))) %>%
  dplyr::select(id)

set.seed(54); bg2 <- spatSample(x = studyArea,
                                size = numbg,
                                method = "random",
                                as.points = TRUE,
                                na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(id = as.numeric(paste0(9999, 1:numbg))) %>%
  dplyr::select(id)

set.seed(64); bg3 <- spatSample(x = studyArea,
                                size = numbg,
                                method = "random",
                                as.points = TRUE,
                                na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(id = as.numeric(paste0(9999, 1:numbg))) %>%
  dplyr::select(id)

set.seed(74); bg4 <- spatSample(x = studyArea,
                                size = numbg,
                                method = "random",
                                as.points = TRUE,
                                na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(id = as.numeric(paste0(9999, 1:numbg))) %>%
  dplyr::select(id)

set.seed(84); bg5 <- spatSample(x = studyArea,
                                size = numbg,
                                method = "random",
                                as.points = TRUE,
                                na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(id = as.numeric(paste0(9999, 1:numbg))) %>%
  dplyr::select(id)

# add the background & occurrence points to the plotted raster
plot(studyArea[[20]])
plot(bg1,add=T) 
plot(points, add=T, col="red")

# get environmental values from background points for modeling
bg1b <- cbind(bg1, terra::extract(env, bg1)) %>% dplyr::select(-ID)
bg2b <- cbind(bg1, terra::extract(env, bg2)) %>% dplyr::select(-ID)
bg3b <- cbind(bg1, terra::extract(env, bg3)) %>% dplyr::select(-ID)
bg4b <- cbind(bg1, terra::extract(env, bg4)) %>% dplyr::select(-ID)
bg5b <- cbind(bg1, terra::extract(env, bg5)) %>% dplyr::select(-ID)

# make a final point data set with observations (will be subsampled later)
train.points_sp <- points %>%
  mutate(id = row_number()) %>%
  dplyr::select(id, elev:urban_p1500m) %>%
  as(., "Spatial") 

# make a final point data set with background points
bg.train.points1 <- points %>%
  mutate(id = row_number()) %>%
  bind_rows(bg1b) %>%
  dplyr::select(id, elev:urban_p1500m) %>%
  filter(id %in% paste0("9999",1:numbg)) %>%
  as.data.frame()

bg.train.points2 <- points %>%
  mutate(id = row_number()) %>%
  bind_rows(bg2b) %>%
  dplyr::select(id, elev:urban_p1500m) %>%
  filter(id %in% paste0("9999",1:numbg)) %>%
  as.data.frame()

bg.train.points3 <- points %>%
  mutate(id = row_number()) %>%
  bind_rows(bg3b) %>%
  dplyr::select(id, elev:urban_p1500m) %>%
  filter(id %in% paste0("9999",1:numbg)) %>%
  as.data.frame()

bg.train.points4 <- points %>%
  mutate(id = row_number()) %>%
  bind_rows(bg4b) %>%
  dplyr::select(id, elev:urban_p1500m) %>%
  filter(id %in% paste0("9999",1:numbg)) %>%
  as.data.frame()

bg.train.points5 <- points %>%
  mutate(id = row_number()) %>%
  bind_rows(bg5b) %>%
  dplyr::select(id, elev:urban_p1500m) %>%
  filter(id %in% paste0("9999",1:numbg)) %>%
  as.data.frame()

# spatially thin occurrence data (keep one occurrence point per cell)
cells <- terra::cellFromXY(raster(env[[16]]), train.points_sp)
dups <- duplicated(cells)
occ_final <- train.points_sp[!dups, ] %>%
  st_as_sf(., crs = st_crs(env))
cat(nrow(train.points_sp) - nrow(occ_final), "records are removed")
st_crs(occ_final) <- st_crs(env)

### Build maxent model using dismo
# https://rdrr.io/cran/dismo/man/maxent.html
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md

# get the same random sample for training and testing

# randomly select 70% for training (5 sets)
set.seed(2); selected1 <- sample(1:nrow(occ_final), nrow(occ_final) * 0.7, replace = F)
set.seed(20); selected2 <- sample(1:nrow(occ_final), nrow(occ_final) * 0.7, replace = F)
set.seed(200); selected3 <- sample(1:nrow(occ_final), nrow(occ_final) * 0.7, replace = F)
set.seed(2000); selected4 <- sample(1:nrow(occ_final), nrow(occ_final) * 0.7, replace = F)
set.seed(2100); selected5 <- sample(1:nrow(occ_final), nrow(occ_final) * 0.7, replace = F)

occ_train1 <- occ_final[selected1, ]  # this is the selection to be used for model training
occ_test1 <- occ_final[-selected1, ]  # this is the opposite of the selection which will be used for model testing
occ_train2 <- occ_final[selected2, ]  # this is the selection to be used for model training
occ_test2 <- occ_final[-selected2, ]  # this is the opposite of the selection which will be used for model testing
occ_train3 <- occ_final[selected3, ]  # this is the selection to be used for model training
occ_test3 <- occ_final[-selected3, ]  # this is the opposite of the selection which will be used for model testing
occ_train4 <- occ_final[selected4, ]  # this is the selection to be used for model training
occ_test4 <- occ_final[-selected4, ]  # this is the opposite of the selection which will be used for model testing
occ_train5 <- occ_final[selected5, ]  # this is the selection to be used for model training
occ_test5 <- occ_final[-selected5, ]  # this is the opposite of the selection which will be used for model testing

# env conditions for training occurrences from the raster
p.env1 <- occ_train1 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points1, pa = 0))) %>%
  dplyr::select(-id, -geometry)

p.env2 <- occ_train2 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points2, pa = 0))) %>%
  dplyr::select(-id, -geometry)

p.env3 <- occ_train3 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points3, pa = 0))) %>%
  dplyr::select(-id, -geometry)

p.env4 <- occ_train4 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points4, pa = 0))) %>%
  dplyr::select(-id, -geometry)

p.env5 <- occ_train5 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points5, pa = 0))) %>%
  dplyr::select(-id, -geometry)

# just the presence observations
p.env1.pres <- p.env1 %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

p.env2.pres <- p.env2 %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

p.env3.pres <- p.env3 %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

p.env4.pres <- p.env4 %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

p.env5.pres <- p.env5 %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

# env conditions for testing occurrence (presence) points
p.env1_test.pres <- occ_test1 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id)

p.env2_test.pres <- occ_test2 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id)

p.env3_test.pres <- occ_test3 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id)

p.env4_test.pres <- occ_test4 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id)

p.env5_test.pres <- occ_test5 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id)


# train Maxent with tabular data
mod1 <- maxent(x=dplyr::select(p.env1, c(elev:SP, bio10:bio11, 
                        bio15, bio17:bio18, bio4:bio6,
                        forest_p1500m:urban_p1500m)), ## env conditions
              p=p.env1$pa,   ## 1:presence or 0:absence
              path="/Users/mikea/Documents/research/Sinanodonta/output/maxent_fewerBioclim_out1", 
              args=c("responsecurves") ## parameter specification
)

mod2 <- maxent(x=dplyr::select(p.env2, c(elev:SP, bio10:bio11, 
                                         bio15, bio17:bio18, bio4:bio6,
                                         forest_p1500m:urban_p1500m)), ## env conditions
               p=p.env2$pa,   ## 1:presence or 0:absence
               path="/Users/mikea/Documents/research/Sinanodonta/output/maxent_fewerBioclim_out2", 
               args=c("responsecurves") ## parameter specification
)

mod3 <- maxent(x=dplyr::select(p.env3, c(elev:SP, bio10:bio11, 
                                         bio15, bio17:bio18, bio4:bio6,
                                         forest_p1500m:urban_p1500m)), ## env conditions
               p=p.env3$pa,   ## 1:presence or 0:absence
               path="/Users/mikea/Documents/research/Sinanodonta/output/maxent_fewerBioclim_out3", 
               args=c("responsecurves") ## parameter specification
)

mod4 <- maxent(x=dplyr::select(p.env4, c(elev:SP, bio10:bio11, 
                                         bio15, bio17:bio18, bio4:bio6,
                                         forest_p1500m:urban_p1500m)), ## env conditions
               p=p.env4$pa,   ## 1:presence or 0:absence
               path="/Users/mikea/Documents/research/Sinanodonta/output/maxent_fewerBioclim_out4", 
               args=c("responsecurves") ## parameter specification
)

mod5 <- maxent(x=dplyr::select(p.env5, c(elev:SP, bio10:bio11, 
                                         bio15, bio17:bio18, bio4:bio6,
                                         forest_p1500m:urban_p1500m)), ## env conditions
               p=p.env5$pa,   ## 1:presence or 0:absence
               path="/Users/mikea/Documents/research/Sinanodonta/output/maxent_fewerBioclim_out5", 
               args=c("responsecurves") ## parameter specification
)

# save model output
saveRDS(mod1, paste0(outpath, "mod1_fewerBioclim.rds"))
saveRDS(mod2, paste0(outpath, "mod2_fewerBioclim.rds"))
saveRDS(mod3, paste0(outpath, "mod3_fewerBioclim.rds"))
saveRDS(mod4, paste0(outpath, "mod4_fewerBioclim.rds"))
saveRDS(mod5, paste0(outpath, "mod5_fewerBioclim.rds"))

# view the maxent models in a html brower
# show(mod1)
# show(mod2)
# show(mod3)
# show(mod4)
# show(mod5)

# make predictions for Europe 
ped1 <- predict(mod1,env)
ped2 <- predict(mod2,env)
ped3 <- predict(mod3,env)
ped4 <- predict(mod4,env)
ped5 <- predict(mod5,env)

# make predictions for training occurrences
tped1 <- predict(mod1, p.env1.pres); (tpq1 <- quantile(tped1, c(0.1))) # .2712
tped2 <- predict(mod2, p.env2.pres); (tpq2 <- quantile(tped2, c(0.1))) # 
tped3 <- predict(mod3, p.env3.pres); (tpq3 <- quantile(tped3, c(0.1))) # 
tped4 <- predict(mod4, p.env4.pres); (tpq4 <- quantile(tped4, c(0.1))) # 
tped5 <- predict(mod5, p.env5.pres); (tpq5 <- quantile(tped5, c(0.1))) # 

# using 'training data' to evaluate model performance
(mod1_eval_train <- dismo::evaluate(p = p.env1.pres, a = bg.train.points1, model = mod1))
(mod2_eval_train <- dismo::evaluate(p = p.env2.pres, a = bg.train.points2, model = mod2))
(mod3_eval_train <- dismo::evaluate(p = p.env3.pres, a = bg.train.points3, model = mod3))
(mod4_eval_train <- dismo::evaluate(p = p.env4.pres, a = bg.train.points4, model = mod4))
(mod5_eval_train <- dismo::evaluate(p = p.env5.pres, a = bg.train.points5, model = mod5))

# get test evaluation stats
(mod1_eval_test <- dismo::evaluate(p = p.env1_test.pres, a = bg.train.points1, model = mod1))
(mod2_eval_test <- dismo::evaluate(p = p.env2_test.pres, a = bg.train.points2, model = mod2))
(mod3_eval_test <- dismo::evaluate(p = p.env3_test.pres, a = bg.train.points3, model = mod3))
(mod4_eval_test <- dismo::evaluate(p = p.env4_test.pres, a = bg.train.points4, model = mod4))
(mod5_eval_test <- dismo::evaluate(p = p.env5_test.pres, a = bg.train.points5, model = mod5))

# calculate thresholds of models
# (thd1 <- threshold(mod_eval_train, "no_omission"))  # 0% omission rate 
# (thd2 <- threshold(mod_eval_train, "spec_sens"))  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
# plot(ped2 > 0.25)
# plot(occ_final, add = T)

# write rasters for individual model runs
ped_all <- c(ped1, ped2, ped3, ped4, ped5)
# writeRaster(ped_all, paste0(outpath, "europe_predictions_5mods.tif"), overwrite = T)

mean_pred <- mean(ped_all)
png("figures/Europe_predictions_5mod_avg_fewerBioclim.600.png", width = 5*600, height = 5*600, res = 600)
plot(mean_pred)
dev.off()
plot(mean_pred)

# writeRaster(mean_pred, paste0(outpath, "europe_predictions_5mod_mean_fewerBioclim.tif"), overwrite = T)

# collect model stats
mod_AUC <- data.frame(model = c("mod1", "mod2", "mod3", "mod4", "mod5", "mean"),
                      vars = "all",
                      trainAUC = c(mod1_eval_train@auc, mod2_eval_train@auc,
                                   mod3_eval_train@auc, mod4_eval_train@auc,
                                   mod5_eval_train@auc, NaN),
                      testAUC = c(mod1_eval_test@auc, mod2_eval_test@auc,
                                  mod3_eval_test@auc, mod4_eval_test@auc,
                                  mod5_eval_test@auc, NaN))
mod_AUC[6,3] <- mean(mod_AUC$trainAUC, na.rm = T)
mod_AUC[6,4] <- mean(mod_AUC$testAUC, na.rm = T)
mod_AUC

# write.csv(mod_AUC, "output/mod_AUC_fewerBioclim.csv")
