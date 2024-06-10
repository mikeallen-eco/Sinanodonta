library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

# read in covariate data for observations
cenv <- read_xlsx("data/Swood_new_arrangement.xlsx", 
                  sheet = "Data_all_New_shape")

# make spatial to view point distribution
pts <- cenv %>%
  filter(Scen %in% 2,
         Time %in% 1) %>%
  mutate(lat = Lat, lon = Long) %>%
  st_as_sf(., # first argument = data frame with coordinates
                   coords = c("Lat", "Long"), # name of columns, in quotation marks
                   crs = 4326)

eu <- rnaturalearth::ne_countries(continent = "Europe", returnclass = "sf") %>%
  filter(!sovereignt %in% c("Russia", "Iceland"))

ggplot(pts) +
  geom_sf(data = eu) +
  geom_point(aes(x = lon, y = lat, color = TminCold), 
             alpha = .4, size = 1) +
  scale_color_viridis_c() +
  scale_x_continuous(limits = c(-10, 40)) +
  scale_y_continuous(limits = c(32,62)) +
  labs(x = "", y = "")

ggsave("figures/cpm_tmincold.png", height = 6, width = 6, dpi = 400)
