### Load map of Europe
library(rnaturalearth)
library(dplyr)
library(sf)

# load Europe
eu <- rnaturalearth::ne_countries(continent = "Europe", type = "countries", returnclass = "sf", 
                                  scale = 10) %>%
  filter(!sovereignt %in% c("Russia")) %>%
  bind_rows(rnaturalearth::ne_countries(country = "Turkey", returnclass = "sf", 
                                        scale = 10)) %>%
  bind_rows(rnaturalearth::ne_countries(country = "Cyprus", returnclass = "sf", 
                                        scale = 10)) %>%
  st_transform(crs = 4326)

# Define the bounding box (xmin, ymin, xmax, ymax)
bbox <- st_bbox(c(xmin = -25, ymin = 31, xmax = 45, ymax = 70), crs = 4326)

# Convert the bbox to an sf object
bbox_sf <- st_as_sfc(bbox)

# Perform the clipping
eu2 <- st_intersection(eu, bbox_sf)

# test plot Europe region
plot(eu2[,"sovereignt"])