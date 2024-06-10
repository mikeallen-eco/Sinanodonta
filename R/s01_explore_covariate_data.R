library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

# read in covariate data for observations
cenv <- read_xlsx("data/Swood_new_arrangement.xlsx", 
                  sheet = "Data_all_New_shape")

# make spatial to view point distribution
pts <- st_as_sf(cenv, # first argument = data frame with coordinates
                   coords = c("Lat", "Long"), # name of columns, in quotation marks
                   crs = 4326)


