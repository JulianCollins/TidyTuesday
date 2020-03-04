# Week 9 - Measles Immunization USA

library(tidyverse)
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
  labs(title = "MMR vaccinations rates by US State and school size", x = "", y = "School enrollment size", fill = "MMR vaccination rate: %") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, face = 'bold', size = 10)) +
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
  guides(colour = guide_legend(nrow = 3)) +
  geom_label_repel(data = measles_ark_low,
                  aes(label = name, colour = city),
                  size = 2.5,
                  family = 'Courier',
                  fill = 'grey70',
                  show.legend = F) +
  scale_colour_viridis_d(option = 'A', name = "") +
  theme_minimal() +
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
  geom_label_repel(data = measles_cali_low,
                  aes(label = name, colour = city),
                  size = 2.5,
                  family = 'Courier',
                  fill = "grey70",
                  show.legend = F) +
  scale_colour_viridis_d(option = 'A', name = "") +
  theme_minimal() +
  labs(title = "California: school enrollment 200+", x = "", y = "") +
  theme(legend.position = 'bottom')


# text layer
df <- data.frame(
  label = "The **tile map [left]** shows states with data for school MMR vaccination rates. Arkansas and California
          show relatively low rates of vaccination within larger schools.
          The two **scatter plots [below]** examine the variation in vaccination rates within these two states. 
          A small number of schools are leading to the lower overall % figures.
          In **Arkansas** there is a fairly wide spread of vaccination rates and a larger cohort of schools with more than 200 pupils (*n = 460*), 
          whilst within **California** most schools in this size bracket (*n = 55*) have vaccination rates at or close to 100%, with a small group
          of schools having rates below 50%.",
  x = 0,
  y = 1,
  hjust = 0,
  vjust = 1,
  orientation = "upright",
  color = "darkorchid4",
  fill = "cornsilk",
  size = 3.25
)

plot_text <- ggplot(df) +
  aes(
    x, y, label = label, color = color, fill = fill, family = "Courier", size = size,
    hjust = hjust, vjust = vjust,
    orientation = orientation
  ) +
  geom_textbox(width = unit(0.95, "npc")) +
  geom_point(color = "darkorchid4", size = 2) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation", "size")) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void()



# patchwork layout
(plot_states | plot_text) /
  (plot_ark_city | plot_cali_city) +
plot_annotation(title = "MMR vaccination rates in US schools, by State*", 
                caption = "*states without recorded MMR vaccination rates are exluded",
                theme = theme(plot.title = element_text(size = 20), 
                plot.caption = element_text(size = 12, face = 'italic'))) &
  theme(text = element_text(family = 'Courier')
  )

ggsave("tt2020_w9_MeaslesUSschools.png", width = 35, height = 25, units = "cm", dpi = 300)











