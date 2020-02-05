# TidyTuesday 2020: week 5: San Francisco Trees

library(tidyverse)
library(ggmap)
library(lubridate)
library(patchwork)



# data download
sf_trees <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# create variable of common species names
sf_trees <- sf_trees %>% mutate(species_common = sub(pattern = '.*\\:', '', species)) 

# trim whitespace
sf_trees$species_common <- str_trim(sf_trees$species_common)


sf_trees %>% select(species_common, species_common_cnt) %>% distinct() %>% arrange(desc(species_common_cnt)) %>% View()



# SF map
osm_sf <-get_map(location = c(left = -122.525, bottom = 37.7, right = -122.35, top = 37.810), source = "stamen", maptype = "toner", zoom = 13, color = "bw")

a <- ggmap(osm_sf) +
geom_point(data = sf_trees, aes( x = longitude, y = latitude),
             colour = "darkgreen",
             size = 0.2,
             alpha = 0.1) +
theme_linedraw() +
  theme(axis.text = element_text(size = 6)) +
  labs(title = "Tree location: all species")


# add species count variable
sf_trees <- sf_trees %>% group_by(species_common) %>% mutate(species_common_cnt = n()) %>% ungroup()

# faceted maps of commonly found species
b <- sf_trees %>% filter(species_common!= '') %>% filter(species_common_cnt > 5000) %>% filter(latitude > 37.7 & latitude < 38.0) %>% 
  ggplot(aes(longitude, latitude)) +
  geom_point(colour = "darkgreen", alpha = 0.1, size = 0.5) +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 6)) +
  labs(title = "Tree location: most commonly found species", caption = "species unknown excluded") +
  facet_wrap(~ species_common) +
  theme(strip.text = element_text(size = 7))


# # add site_info count variable
# sf_trees <- sf_trees %>% group_by(site_info) %>% mutate(site_info_cnt = n()) %>% ungroup()
# 
# c <- sf_trees %>% filter(latitude > 37.7 & latitude < 38.0) %>% filter(site_info != ":") %>% filter(site_info_cnt > 900) %>% 
#   ggplot(aes(longitude, latitude)) +
#   geom_point(colour = "darkorange", alpha = 0.1, size = 0.5) +
#   theme_linedraw() +
#   theme(legend.position = "none") +
#   labs(title = "Tree location: most commonly found site type", caption = "site unknown excluded") +
#   facet_wrap(~ site_info)
# 
# 
# # add legal_status count variable
# sf_trees <- sf_trees %>% group_by(legal_status) %>% mutate(legal_status_cnt = n()) %>% ungroup()
# 
# d <- sf_trees %>% filter(latitude > 37.7 & latitude < 38.0) %>% filter(!is.na(legal_status)) %>% 
#   ggplot(aes(longitude, latitude)) +
#   geom_point(colour = "dodgerblue3", alpha = 0.1, size = 0.5) +
#   theme_linedraw() +
#   theme(legend.position = "none") +
#   labs(title = "Tree location: legal status", caption = "legal status unknown excluded") +
#   facet_wrap(~ legal_status)


# density plot of tree planting year by species
e <- sf_trees %>% filter(species_common!= '') %>% filter(species_common_cnt > 5000) %>% 
  ggplot(aes(year(date), fill = species_common)) +
  geom_density(alpha = 0.3) +
  theme_linedraw() +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Tree planting year: most commonly found species")



# patchwork
sf_trees_patch <- (a | b) / e

sf_trees_patch + 
  plot_layout(heights = c(2.5, 1)) +
  plot_annotation(
  title = "The Trees of San Francisco",
  theme = theme(plot.title = element_text(size = 24))
) 

# export
ggsave("tt2020_w5.png", width = 30, height = 25, units = "cm", dpi = 300)



