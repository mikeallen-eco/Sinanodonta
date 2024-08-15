library(dplyr)
library(sf)
library(terra)
library(vegan)
library(ggplot2)

# load Europe
source("R/load_Europe.R")
eu_minus <- eu2 %>%
  filter(!sovereignt %in% c("Ukraine", "Moldova", "Belarus"))
# subset to just the largest countries 
eu_minus$area <- st_area(eu_minus)
eu_minus2 <- eu_minus %>% 
  arrange(desc(area)) %>%
  slice_head(n = 30)

# load CONUS
us <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))
outpath <- "/Users/mikea/Documents/mikedata/cpm/202406/finaldata/"
rpath.eu <- paste0(outpath, "europe_final_raster_stack.tif")
rpath.us <- paste0(outpath, "conus_final_raster_stack.tif")

### read in environmental covariate data for Sinanodonta locations
points <- read.csv("data/final_EU_cpm_data.csv") %>%
  st_as_sf(., coords = c("lon", "lat"),
           crs = 4326) 

# load environmental data rasters
env.eu <- rast(rpath.eu)
env.us <- rast(rpath.us)

# Function to generate random points within a single polygon and retain attributes
generate_random_points <- function(polygon, n = 1000) {
  # Sample random points within the polygon
  points <- st_sample(polygon, size = n, type = "random")
  
  # Convert points to sf object
  points_sf <- st_sf(geometry = points)
  
  # Repeat the polygon's attributes for each point
  attributes_df <- st_drop_geometry(polygon)
  attributes_repeated <- attributes_df[rep(1, n), , drop = FALSE]
  
  # Bind the attributes to the points
  points_sf <- cbind(points_sf, attributes_repeated)
  
  return(points_sf)
}

# Apply the function to each polygon
random_points_list.eu <- lapply(1:nrow(eu_minus2), 
                             function(i) generate_random_points(eu_minus2[i, ], 
                                                                n = 10))

random_points_list.us <- lapply(1:nrow(us), 
                                function(i) generate_random_points(us[i, ], 
                                                                   n = 10))

# Combine all points into a single sf object
random_points.eu <- do.call(rbind, random_points_list.eu)
random_points.us <- do.call(rbind, random_points_list.us)

# plot(st_geometry(eu_minus2), add = F, border = "blue", pch = 16)
# plot(st_geometry(random_points.eu), add = TRUE, col = "red", pch = 16)
# plot(st_geometry(us), add = F, border = "blue", pch = 16)
# plot(st_geometry(random_points.us), add = TRUE, col = "red", pch = 16)

eu_env <- cbind(select(random_points.eu, name = sovereignt, name2 = abbrev), 
                extract(env.eu, random_points.eu)) %>%
  select(-ID, -dist_water) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  # group_by(name, name2) %>%
  # summarise_all(., median, na.rm = T, .groups = "drop") %>%
  ungroup()
  
us_env <- cbind(select(random_points.us, name, name2 = postal), 
                  extract(env.us, random_points.us)) %>%
  select(-ID, -dist_water) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  # group_by(name, name2) %>%
  # summarise_all(., median, na.rm = T, .groups = "drop") %>%
  ungroup()
  
cpm_env <- extract(env.eu, points) %>%
  select(-ID, -dist_water) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(name = "Sinanodonta", name2 = "CPM")

comb.env <- eu_env %>%
  bind_rows(us_env) %>%
  bind_rows(cpm_env) %>%
  filter(!is.na(forest_p1500m),
         !is.na(CR))

# package vegan NMDS
# to run the NMDS, un-comment the code; otherwise load the pre-run model below

set.seed(444)
NMDS.comb = metaMDS(as.matrix(comb.env[,c(3:32)]),
                     k=2, distance = 'euclidean', trymax = 100, na.rm = T)
# saveRDS(NMDS.comb, "output/NMDS.comb_10_rnd_pts_per_unit.rds")
# NMDS.comb <- readRDS("output/NMDS.comb_10_rnd_pts_per_unit.rds")
NMDS.comb # stres

# format covariate data - species level
plot.data <- data.frame(
  name = comb.env$name,
  abr = comb.env$name2) %>%
  cbind(scores(NMDS.comb))

plot.eu <- plot.data %>%
  slice_head(n = 298) %>%
  group_by(name, abr) %>%
  summarise_all(., median, na.rm = T, .groups = "drop") %>%
  ungroup() %>%
  bind_rows(filter(plot.data, abr %in% "CPM")) %>%
  mutate(region = "Europe")
  
plot.us <- plot.data %>%
  filter(!abr %in% plot.eu$abr) %>%
  mutate(region = "United States") %>%
  group_by(name, abr, region) %>%
  summarise_all(., median, na.rm = T, .groups = "drop") %>%
  ungroup()

plot.env <- plot.eu %>%
  bind_rows(plot.us) %>%
  mutate(type = case_when(abr %in% "CPM" ~ "CPM observations",
                          TRUE ~ "All locations"),
         abr = case_when(abr %in% "CPM" ~ "*",
                         TRUE ~ abr))

plot.env.cpm <- plot.env %>%
  filter(abr %in% "*")

plot.env.nocpm <- plot.env %>%
  filter(!abr %in% "*")

plot.env %>%
    ggplot() +
  stat_ellipse(data = plot.env %>% filter(type == "CPM observations"), 
               aes(x = NMDS1, y = NMDS2), 
               color = "black", size = 1, linetype = 2) +
    geom_text(aes(x = NMDS1, y = NMDS2, label = abr, color = region),
              size = 5, alpha = 0.75) +
    scale_color_manual(values = c("dodgerblue", "orangered")) +
  scale_x_continuous(limits = c(min(plot.env.cpm$NMDS1), max(plot.env.cpm$NMDS1))) +
  scale_y_continuous(limits = c(min(plot.env.cpm$NMDS2), max(plot.env.cpm$NMDS2))) +
    facet_wrap(~type) +
    guides(color = "none") +
  theme_bw() +
  theme(panel.background = element_rect(color = "white"))

# ggsave("figures/NMDS_all_variables.png", height = 4, width = 6, dpi = 400)


##################
# remake comb.env with larger sample size of random points for env variable plotting
##################

random_points_list.eu <- lapply(1:nrow(eu_minus2), 
                                function(i) generate_random_points(eu_minus2[i, ], 
                                                                   n = 1000))

random_points_list.us <- lapply(1:nrow(us), 
                                function(i) generate_random_points(us[i, ], 
                                                                   n = 1000))

# Combine all points into a single sf object
random_points.eu <- do.call(rbind, random_points_list.eu)
random_points.us <- do.call(rbind, random_points_list.us)

eu_env <- cbind(select(random_points.eu, name = sovereignt, name2 = abbrev), 
                extract(env.eu, random_points.eu)) %>%
  select(-ID, -dist_water) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  ungroup()

us_env <- cbind(select(random_points.us, name, name2 = postal), 
                extract(env.us, random_points.us)) %>%
  select(-ID, -dist_water) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  ungroup()

cpm_env <- extract(env.eu, points) %>%
  select(-ID, -dist_water) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(name = "Sinanodonta", name2 = "CPM")

comb.env.plot <- eu_env %>%
  mutate(region = "Europe") %>%
  bind_rows(mutate(us_env, region = "United States")) %>%
  dplyr::rename(abr = name2) %>%
  group_by(name, abr, region) %>%
  summarise_all(., median, na.rm = T, .groups = "drop") %>%
  ungroup() %>%
  bind_rows(mutate(cpm_env, region = "Sinanodonta")) %>%
  filter(!is.na(forest_p1500m),
         !is.na(CR)) %>%
  mutate(abr = case_when(is.na(abr) ~ "CPM",
                         TRUE ~ abr))

comb.env.plot %>%
  filter(!abr %in% "CPM") %>%
  mutate(surf = ifelse(SP %in% 1, "Mostly sand,\nsediments",
                       "Mostly rock")) %>%
  ggplot() +
  geom_point(aes(x = bio1, y = elev), color = "darkgray", 
             data = filter(comb.env.plot, abr %in% "CPM"),
             alpha = 0.5) +
  stat_ellipse(data = filter(comb.env.plot, abr %in% "CPM"), 
               aes(x = bio1, y = elev), 
               color = "black", size = 1, linetype = 2, alpha = 0.75) +
  geom_text(aes(x = bio1, y = elev, label = abr, color = surf),
            size = 3) +
  # scale_color_viridis_c(direction = -1) # if using "median" above
  scale_color_manual(values = c("black", "orangered")) +
  # guides(color = "none") +
  theme_bw() +
  labs(x = "Mean temperature (C)", y = "Median elevation (m)", color = "Surface geology") +
  theme(legend.position = c(.85, .85))

ggsave("figures/top_variables_overallmeantemp.png", width = 8, height = 4, dpi = 400)  

# same plot but closer up
comb.env.plot %>%
  filter(!abr %in% "CPM") %>%
  mutate(surf = ifelse(SP %in% 1, "Mostly sand,\nsediments",
                       "Mostly rock")) %>%
  ggplot() +
  geom_point(aes(x = bio1, y = elev), color = "darkgray", 
             data = filter(comb.env.plot, abr %in% "CPM"),
             alpha = 0.5) +
  stat_ellipse(data = filter(comb.env.plot, abr %in% "CPM"), 
               aes(x = bio1, y = elev), 
               color = "black", size = 1, linetype = 2, alpha = 0.75) +
  geom_text(aes(x = bio1, y = elev, label = abr, color = surf),
            size = 3) +
  # scale_color_viridis_c(direction = -1) # if using "median" above
  scale_color_manual(values = c("black", "orangered")) +
  scale_x_continuous(limits = c(4, 20)) +
  scale_y_continuous(limits = c(0, 400)) +
  # guides(color = "none") +
  theme_bw() +
  labs(x = "Mean temperature (C)", y = "Median elevation (m)", color = "Surface geology") +
  theme(legend.position = c(.85, .85))

ggsave("figures/top_variables_overallmeantemp_closeup.png", width = 8, height = 4, dpi = 400)  


####
# collect all covariate values for further analysis
#

# Europe
mean_pred.eu <- rast(paste0(outpath, "europe_predictions_5mod_mean.tif"))
eu_env <- rast(paste0(outpath, "europe_final_raster_stack.tif"))
eu_raw_data <- as.data.frame(c(mean_pred.eu, eu_env), xy = TRUE) %>%
  filter(!is.na(mean)) %>%
  rename(OProb = mean, Long = x, Lat = y)
write.csv(eu_raw_data, "output/eu_raw_data.csv", row.names = F)

# US
mean_pred.us <- rast(paste0(outpath, "conus_predictions_5mod_mean.tif"))
us_env <- rast(paste0(outpath, "conus_final_raster_stack.tif"))
us_raw_data <- as.data.frame(c(mean_pred.us, us_env), xy = TRUE) %>%
  filter(!is.na(mean)) %>%
  rename(OProb = mean, Long = x, Lat = y)
write.csv(us_raw_data, "output/us_raw_data.csv", row.names = F)

# S. woodiana observation locations
Swood_raw_data <- extract(c(mean_pred.eu, eu_env), points) %>%
  cbind(st_coordinates(points)) %>%
  select(Long = X, Lat = Y, Oprob = mean, bio1:dist_water)
write.csv(Swood_raw_data, "output/Swood_raw_data.csv", row.names = F)
