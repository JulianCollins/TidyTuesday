# Food Consumption and CO2 Emissions

library(tidyverse)
library(countrycode)
library(maps)
library(patchwork)


# Download data
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Add continent names
food_consumption <- food_consumption %>% mutate(continent = factor(countrycode(sourcevar = food_consumption$country, origin = 'country.name', destination = 'continent')))

# Country as factor
food_consumption$country <- as_factor(food_consumption$country)

# heatmap
heatmap_all <- food_consumption %>% 
  ggplot(aes(country, fct_rev(food_category), fill = co2_emmission)) +
  geom_tile(colour = 'white', alpha = 0.8) +
  theme_minimal() +
  scale_fill_distiller(palette = "Spectral", direction = -1, name = "CO2 Emissions (Kg CO2/person/year") +
  #scale_fill_gradientn(colours = rev(wes_palette('Moonrise1')), name = "Co2 Emissions (Kg Co2/person/year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5.5, family = 'Geneva')) +
  facet_wrap(~ continent, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none') +
  #theme(legend.key.width = unit(3, 'cm')) +
  labs(x = "", y = "") 


# create data frame of maps::world data
worlddata <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

# modify country names to match food_consumption
worlddata <- worlddata %>% mutate(region = str_replace(region, "UK", "United Kingdom"))

food_beef <- food_consumption %>% filter(food_category == "Beef")

# world map
map_beef <- ggplot() +
  geom_map(data = worlddata, map = worlddata, aes(map_id = region), fill = 'grey90', colour = 'grey60', size = 0.5) +
  expand_limits(x = worlddata$long, y = worlddata$lat) +
  geom_map(data = food_beef, map = worlddata,
            aes(fill = co2_emmission, map_id = country), alpha = 0.8) +
  scale_fill_distiller(palette = "Spectral", direction = -1, name = "Beef consumption: CO2 Emissions (Kg CO2/person/year)") +
  theme(legend.position = 'top') +
  theme(panel.background = element_rect(fill = 'white')) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  theme(legend.key.width = unit(2, 'cm')) +
  theme(legend.title = element_text(family = 'Geneva', size = 12)) +
  theme(legend.text = element_text(family = 'Geneva'))

  
# layout with patchwork
map_beef /
heatmap_all +
  plot_layout(heights = c(3, 1.25)) +
  plot_annotation(
    title = "Where's the Beef?",
    subtitle = "consumption of beef (map below) leads to greater CO2 emissions than other foodstuffs (see heatmap)",
    caption = "data: https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018",
    theme = theme(plot.title = element_text(size = 22, family = "Geneva"), plot.subtitle = element_text(size = 16, family = "Geneva"), plot.caption = element_text(size = 10, family = "Geneva"))
  )  

ggsave("tt2020_w8_FoodCarbonFootprint.png", width = 30, height = 25, units = "cm", dpi = 300)



# version 2, new colours

# heatmap
heatmap_all2 <- food_consumption %>% 
  ggplot(aes(country, fct_rev(food_category), fill = co2_emmission)) +
  geom_tile(colour = 'white', alpha = 0.8) +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, name = "CO2 Emissions (Kg CO2/person/year") +
  #scale_fill_gradientn(colours = rev(wes_palette('Moonrise1')), name = "Co2 Emissions (Kg Co2/person/year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5.5, family = 'Geneva')) +
  facet_wrap(~ continent, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none') +
  #theme(legend.key.width = unit(3, 'cm')) +
  labs(x = "", y = "") 

# world map
map_beef2 <- ggplot() +
  geom_map(data = worlddata, map = worlddata, aes(map_id = region), fill = 'grey90', colour = 'grey60', size = 0.5) +
  expand_limits(x = worlddata$long, y = worlddata$lat) +
  geom_map(data = food_beef, map = worlddata,
           aes(fill = co2_emmission, map_id = country), alpha = 0.8) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, name = "Beef consumption: CO2 Emissions (Kg CO2/person/year)") +
  theme(legend.position = 'top') +
  theme(panel.background = element_rect(fill = 'white')) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  theme(legend.key.width = unit(2, 'cm')) +
  theme(legend.title = element_text(family = 'Geneva', size = 12)) +
  theme(legend.text = element_text(family = 'Geneva'))

# layout with patchwork
map_beef2 /
  heatmap_all2 +
  plot_layout(heights = c(3, 1.25)) +
  plot_annotation(
    title = "Where's the Beef?",
    subtitle = "consumption of beef (map below) leads to greater CO2 emissions than other foodstuffs (see heatmap)",
    caption = "data: https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018",
    theme = theme(plot.title = element_text(size = 22, family = "Geneva"), plot.subtitle = element_text(size = 16, family = "Geneva"), plot.caption = element_text(size = 10, family = "Geneva"))
  )  

ggsave("tt2020_w8_FoodCarbonFootprint2.png", width = 30, height = 25, units = "cm", dpi = 300)



