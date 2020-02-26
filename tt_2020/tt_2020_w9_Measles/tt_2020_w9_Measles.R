# Week 9 - Measles Immunization USA

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(ggtext)


# download data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')


# schools across multiple locations have repeated rows for vaccine info (rows differ by lat/lon)
## not mapping therefore can simplify
measles <- measles %>% select(state, name, type, city, county, enroll, mmr) %>% distinct()


# cut school enrollment size into groups
measles %>% filter(mmr != -1) %>% filter(enroll > 0) %>% mutate(sch_size = cut_number(enroll, 5)) %>% View()

measles <- measles %>% mutate(sch_size = case_when(enroll > 0 & enroll < 50 ~ "1-49",
                                                   enroll >= 50 & enroll < 75 ~ "50-74",
                                                   enroll >= 75 & enroll <= 99 ~ "75-99",
                                                   enroll > 100 & enroll <= 199 ~ "100-199",
                                                   enroll >= 200 ~ "200+"))

# convert sch_size to ordered factor
measles$sch_size <- factor(measles$sch_size, levels = c("1-49", "50-74", "75-99", "100-199", "200+"), ordered = T)


# create mean mmr rates by state and school size
measles <- measles %>% filter(mmr != -1) %>% group_by(state, sch_size) %>% mutate(mmr_grp_state = mean(mmr, na.rm = T))

# plot states
plot_states <- measles %>% filter(!is.na(sch_size)) %>% 
  ggplot(aes(state, sch_size, fill = mmr_grp_state)) +
  geom_tile(colour = 'white') +
  theme_minimal() +
  scale_fill_viridis_c(option = 'A') +
  coord_equal() +
  labs(title = "MMR vaccinations rates by US State and school size", x = "", y = "School size", fill = "MMR vaccination rate: %") +
  theme(axis.text.x = element_text(angle = -90, hjust = 1, face = 'bold')) +
  theme(legend.position = 'top')



# create mean mmr rates by city and school size to investigate below level
measles <- measles %>% filter(mmr != -1) %>% group_by(city, sch_size) %>% mutate(mmr_grp_city = mean(mmr, na.rm = T))

# plot Arkansas only
plot_ark <- measles %>% filter(state == "Arkansas") %>% filter(!is.na(sch_size)) %>% 
  ggplot(aes(fct_lump(city, 10), sch_size, fill = mmr_grp_city)) +
  geom_tile(colour = 'white') +
  theme_minimal() +
  scale_fill_viridis_c(option = 'A') +
  coord_equal() +
  labs(title = "Arkansas", x = "", y = "School enrollment size")

# which cities in Arkansas have low mmr % in larger schools?
measles_ark_low <- measles %>% filter(state == "Arkansas") %>% filter(enroll > 200) %>% filter(mmr < 65)

plot_ark_city <- measles %>% filter(state == "Arkansas") %>% filter(enroll > 200) %>% filter(mmr >= 65) %>% 
  ggplot(aes(enroll, mmr)) +
  geom_point(alpha = 0.5) +
  geom_point(data = measles_ark_low, aes(colour = city), alpha = 0.5, show.legend = T) +
  guides(colour = guide_legend(nrow = 2)) +
  geom_text_repel(data = measles_ark_low,
                  aes(label = name, colour = city),
                  show.legend = F) +
  scale_colour_viridis_d(option = 'A', name = "") +
  theme_minimal() +
  #theme(plot.background = element_rect(fill = 'grey90')) +
  labs(title = "Arkansas: school enrollment 200+", x = "School enrollment size", y = "MMR vaccination rate: %") +
  theme(legend.position = 'bottom')



# plot California only
plot_cali <- measles %>% filter(state == "California") %>% filter(!is.na(sch_size)) %>% 
  ggplot(aes(fct_lump(city, 10), sch_size, fill = mmr_grp_city)) +
  geom_tile(colour = 'white', size = 1) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'grey30')) +
  scale_fill_viridis_c(option = 'A') +
  coord_equal() +
  labs(title = "California", x = "", y = "")


# which cities in California have low mmr % in larger schools?
measles_cali_low <- measles %>% filter(state == "California") %>% filter(enroll > 200) %>% filter(mmr < 75)

plot_cali_city <- measles %>% filter(state == "California") %>% filter(enroll > 200) %>% filter(mmr >= 75) %>% 
  ggplot(aes(enroll, mmr)) +
  geom_point(alpha = 0.5) +
  geom_point(data = measles_cali_low, aes(colour = city), alpha = 0.5) +
  geom_text_repel(data = measles_cali_low,
                  aes(label = name, colour = city),
                  show.legend = F) +
  scale_colour_viridis_d(option = 'A', name = "") +
  theme_minimal() +
  #theme(plot.background = element_rect(fill = 'grey90')) +
  labs(title = "California: school enrollment 200+", x = "", y = "") +
  theme(legend.position = 'bottom')


# text layer
plot_text <- ggplot() +
  theme_void() +
  annotate(geom = 'text', label = "For those states with data for school MMR vaccination rates, Arkansas and California \nshow relatively low rates of vaccination within larger schools.", x = 0.1, y = 0.8, hjust = 0)

# patchwork layout
(plot_states | plot_text) /
  (plot_ark_city | plot_cali_city)












