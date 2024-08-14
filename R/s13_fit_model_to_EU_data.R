library(dplyr)
library(ggplot2)
library(sf)
library(terra)
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

# plot EU raster stack
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

# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the spatSample function every time, if you want to get
# the same "random samples"
set.seed(44); bg <- spatSample(x = studyArea,
                              size = 10000,
                              method = "random",
                              as.points = TRUE,
                              na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(id = as.numeric(paste0(9999, 1:10000))) %>%
  dplyr::select(id)

plot(studyArea[[1]])

# add the background points to the plotted raster
plot(bg,add=T) 
# add the occurrence data to the plotted raster
plot(points, add=T, col="red")

# get environmental values from background points for modeling
values <- terra::extract(env, bg)
bg2 <- cbind(bg, values) %>%
  dplyr::select(-ID)

# make combined point dataset with observations and background points
points.w.bg <- points %>%
  mutate(id = row_number()) %>%
  bind_rows(bg2)

df <- points.w.bg %>%
  dplyr::select(id, elev:urban_p1500m)

# Build maxent model using dismo
# https://rdrr.io/cran/dismo/man/maxent.html
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
# Note: to make maxent work, I had to download a version of R that matched the build of Java that was installed. 

library(dismo)
library(rJava) # note: need to install an x86_64 build of Java for this to work
# brew install --cask temurin
# R CMD javareconf
# install.packages("rJava", type = "source")
# Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home")

# export JAVA_HOME=$(/usr/libexec/java_home -v 17)
# sudo -E R CMD javareconf
maxent()

train.points <- df %>%
  filter(!id %in% paste0("9999",1:10000))
train.points_sp <- as(train.points, "Spatial")
bg.train.points <- df %>%
  filter(id %in% paste0("9999",1:10000)) %>%
  as.data.frame()

# thin occ data (keep one occurrence point per cell)
cells <- terra::cellFromXY(raster(env[[16]]), train.points_sp)
dups <- duplicated(cells)
occ_final <- train.points_sp[!dups, ] %>%
  st_as_sf(., crs = st_crs(env))
cat(nrow(train.points_sp) - nrow(occ_final), "records are removed")
st_crs(occ_final) <- st_crs(env)

# plot the first environmental layer (or replace [[1]] with any
# nth number of the layer of interest from the raster stack).
plot(env[[18]], xlim = c(-109, -50), ylim = c(15, 50))

# plot the final occurrence data on the environmental layer
plot(occ_final, add = T, col = "red")  # the 'add=T' tells R to put the incoming data on the existing layer

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

# env conditions for training occ from the raster
p.env1 <- occ_train1 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points, pa = 0))) %>%
  dplyr::select(-id, -geometry)

p.env1.pres <- p.env1 %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

# env conditions for testing occ
p.env1_test <-  occ_test1 %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(-id) %>%
  mutate(pa = 1) %>%
  bind_rows(st_drop_geometry(mutate(bg.train.points, pa = 0))) %>%
  dplyr::select(-id)

p.env1_test.pres <- p.env1_test %>%
  filter(pa %in% 1) %>%
  dplyr::select(-pa)

# train Maxent with tabular data
mod1 <- maxent(x=p.env1[,c(1:30)], ## env conditions
              p=p.env1$pa,   ## 1:presence or 0:absence
              path="/Users/mikea/Documents/research/Sinanodonta/output/maxent_out1", 
              args=c("responsecurves") ## parameter specification
)

# view the maxent model in a html brower
show(mod1)

# view detailed results
mod@results

# example 1, project to study area [raster]
ped1 <- predict(mod1, studyArea)  # studyArea is the clipped rasters 
plot(ped1)  # plot the continuous prediction

# example 2, project to the broader region 
ped2 <- predict(mod1,env)
plot(ped2)

# example 3, project with training occurrences [dataframes]
ped3 <- predict(mod1, p.env1.pres)
head(ped3)
hist(ped3)  # creates a histogram of the prediction
quantile(ped3, c(0.1)) # .2712
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train <- dismo::evaluate(p = p.env1.pres, a = bg.train.points, model = mod1)
print(mod_eval_train)

mod_eval_test <- dismo::evaluate(p = p.env1_test.pres, a = bg.train.points, model = mod1)
print(mod_eval_test)  # training AUC may be higher than testing AUC

# calculate thresholds of models
(thd1 <- threshold(mod_eval_train, "no_omission"))  # 0% omission rate 
(thd2 <- threshold(mod_eval_train, "spec_sens"))  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(ped1 >= thd1)
plot(ped2 >= thd2)
plot(ped2 > 0.2712)
plot(occ_final, add = T)

writeRaster(ped2, paste0(outpath, "europe_predictions_mod1.tif"))
