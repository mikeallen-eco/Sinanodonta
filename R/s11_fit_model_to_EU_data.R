library(dplyr)
library(ggplot2)
library(sf)
library(terra)
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
wc_path <- paste0(outpath, "europe_wc.tif")
rpath <- paste0(outpath, "europe_final_raster_stack.tif")
# load Europe
source("R/load_Europe.R")

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
  st_union()

# plot EU raster stack
plot(rast(wc_path)[[16]]) #, xlim = c(-109, -50), ylim = c(15, 50))
plot(eu2[,"sovereignt"], add = T, outline = "gray", color = "none")
plot(occ_buff, add = T,border="red")  # adds buffer polygon to the plot
plot(points, add = T, col = "red", pch=4, size =1)  # adds occurrence data to the plot

# crop study area to a manageable extent (rectangle shaped)
studyArea <- terra::crop(env, ext(as(occ_buff, "Spatial")))  

# the 'study area' created by extracting the buffer area from the raster stack
studyArea <- mask(studyArea, vect(occ_buff))
# output will still be a raster stack, just of the study area
plot(studyArea[[1]])

# save the new study area rasters as ascii
writeRaster(studyArea,
            # a series of names for output files
            filename=paste0(
              "/Users/mikea/Documents/mikedata/grasshopper/202407/env_final/",
              names(studyArea),".wclc.asc"), 
            # format="ascii", ## the output format
            # bylayer=TRUE, ## this will save a series of layers
            overwrite=T)

# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the spatSample function every time, if you want to get
# the same "random samples"
set.seed(2); bg <- spatSample(x = studyArea,
                              size = 10000,
                              method = "random",
                              as.points = TRUE,
                              na.rm = TRUE) %>%
  as(., "Spatial") %>%
  st_as_sf(crs = 4326) %>%
  mutate(gbifID = as.numeric(paste0(9999, 1:10000))) %>%
  dplyr::select(gbifID)

plot(studyArea[[1]])

# add the background points to the plotted raster
plot(bg,add=T) 
# add the occurrence data to the plotted raster
plot(occ_final, add=T, col="red")

# make combined point dataset with observations and background points
points.w.bg <- points %>%
  bind_rows(bg) %>%
  # project to lc projection in meters
  st_transform(crs = 4326)
# 
# buffers.w.bg <- points.w.bg %>%
#   st_buffer(., dist = 1500)

# get 1500 m land cover pct around each observation point
# vals <- exactextractr::exact_extract(landcov, buffers.w.bg, fun = 'mean')

# get environmental values from observation points for modeling
values <- terra::extract(env, st_transform(points.w.bg, crs = 4326))

df <- cbind.data.frame(as.data.frame(points.w.bg), values[,2:20])# %>%
dplyr::select(-mean.snow) %>% # no snow
  # rename(ag.cult = mean.ag.cult, bare = mean.bare, forest = mean.forest, 
  #        grass.ag = mean.grass.ag, grass.nonag = mean.grass.nonag,
  #        shrub = mean.shrub, urban = mean.urban, water = mean.water,
  #        wet = mean.wet)
  df

write.csv(df, "output/train_dataset_no_lc.csv", row.names = F)

# Build maxent model using dismo
build a maxent model in dismo
https://rdrr.io/cran/dismo/man/maxent.html
https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
Note: to make maxent work, I had to download a version of R that matched the build of Java that was installed. 

library(dismo)
library(rJava) # note: need to install an x86_64 build of Java for this to work
# brew install --cask temurin
# R CMD javareconf
# install.packages("rJava", type = "source")
# Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home")

# export JAVA_HOME=$(/usr/libexec/java_home -v 17)
# sudo -E R CMD javareconf
maxent()

train.points <- points.w.bg %>%
  filter(!gbifID %in% paste0("9999",1:10000)) %>%
  st_transform(crs = 4326)
train.points_sp <- as(train.points, "Spatial")
bg.train.points <- points.w.bg %>%
  filter(gbifID %in% paste0("9999",1:10000)) %>%
  st_transform(crs = 4326) %>%
  as.data.frame() %>%
  left_join(select(df, -geometry), by = "gbifID") %>%
  dplyr::select(-gbifID, -geometry)

# thin occ data (keep one occurrence point per cell)
cells <- terra::cellFromXY(raster(env[[1]]), train.points_sp)
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

# randomly select 50% for training
set.seed(2); selected <- sample(1:nrow(occ_final), nrow(occ_final) * 0.5)

occ_train <- occ_final[selected, ]  # this is the selection to be used for model training
occ_test <- occ_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

# env conditions for training occ from the raster
p.env.pres <- occ_train %>%
  left_join(df, by = join_by(gbifID)) %>%
  as.data.frame() %>%
  dplyr::select(-gbifID,-geometry.x, -geometry.y)

p.env <- p.env.pres %>%
  bind_rows(bg.train.points) %>%
  dplyr::select(1:19)

# env conditions for testing occ
p.env_test <-  occ_test %>%
  left_join(df, by = join_by(gbifID)) %>%
  as.data.frame() %>%
  dplyr::select(-gbifID)

# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa <- c(rep(1, nrow(occ_train)), rep(0, nrow(bg.train.points)))

# (rep(1,nrow(p)) creating the number of rows as the p data
# set to have the number '1' as the indicator for presence;
# rep(0,nrow(a)) creating the number of rows as the a data
# set to have the number '0' as the indicator for absence;
# the c combines these ones and zeros into a new vector that
# can be added to the Maxent table data frame with the
# environmental attributes of the presence and absence
# locations

# train Maxent with spatial data
# mod <- maxent(x=clim,p=occ_train)

# train Maxent with tabular data
mod <- maxent(x=p.env, ## env conditions minus snow
              p=pa,   ## 1:presence or 0:absence
              
              path=paste0("./output/maxent_outputs_no_lc"), 
              ## folder for maxent output; 
              # if we do not specify a folder R will put the results in a temp file, 
              # and it gets messy to read those. . .
              args=c("responsecurves") ## parameter specification
)
# the maxent functions runs a model in the default settings. To change these parameters,
# you have to tell it what you want...i.e. response curves or the type of features

# view the maxent model in a html brower
show(mod)

# view detailed results
mod@results

# example 1, project to study area [raster]
ped1 <- predict(mod, studyArea)  # studyArea is the clipped rasters 
plot(ped1)  # plot the continuous prediction

# example 2, project to the world 
# ped2 <- predict(mod,wc)
# plot(ped2)

# example 3, project with training occurrences [dataframes]
ped3 <- predict(mod, p.env)
head(ped3)

hist(ped3)  # creates a histogram of the prediction

# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train <- dismo::evaluate(p = p.env.pres, a = bg.train.points, model = mod)
print(mod_eval_train)

mod_eval_test <- dismo::evaluate(p = p_test, a = bg.train.points, model = mod)
print(mod_eval_test)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1 <- threshold(mod_eval_train, "no_omission")  # 0% omission rate 
thd2 <- threshold(mod_eval_train, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(ped1 >= thd1)
# plot(ped1 >= thd2)
plot(occ_final, add = T)
