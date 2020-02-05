# w6 NFL attendances

library(tidyverse)
library(extrafont)


# download data
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

# join tables
attstan <- attendance %>% left_join(standings, by = c("team", "team_name", "year"))

# add team + name
attstan <- attstan %>% mutate(full_name = paste(team, team_name))

# add average annual home attendances
attstan <- attstan %>% group_by(full_name, year) %>% mutate(avg_home_att = round(mean(home / 8), 0)) %>% ungroup()

att_home <- attstan %>% select(full_name, year, avg_home_att, playoffs) %>% distinct() 

att_home_summary <- att_home %>% group_by(full_name, playoffs) %>% mutate(avg_home_att_poff = round(mean(avg_home_att), 0)) %>% ungroup() %>% select(full_name, playoffs, avg_home_att_poff) %>% distinct() 



att_home_summary <- att_home_summary %>% pivot_wider(names_from = playoffs, values_from = avg_home_att_poff) 

att_home_summary <- att_home_summary %>% mutate(playoff_boost = Playoffs - `No Playoffs`)

att_home_summary <- att_home_summary %>% mutate(playoff_boost_grp = ifelse(playoff_boost > 0, "Positive", "Negative"))

#NFL Blue
rgb(4, 36, 85, maxColorValue = 255) # '#042455'

#NFL Red
rgb(201, 0, 12, maxColorValue = 255) # '#C9000C'

#NFL Grey
rgb(234, 234, 234, maxColorValue = 255) # '#EAEAEA'


att_home_summary %>% 
  ggplot(aes(x = fct_reorder(full_name, playoff_boost), y = playoff_boost, fill = playoff_boost_grp)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(-10100, 10100)) +
  scale_fill_manual(values = c('#C9000C', '#042455')) +
  annotate(geom = "curve", x = 4, y = -7500, xend = 2, yend = -9500, curvature = 0.25, arrow = arrow(length = unit(2.5, "mm"))) +
  annotate(geom = "text", x = 4, y = -5500, label = "relocated: 2016", fontface = "bold", family = "Rockwell", size = 3.5) +
  theme_linedraw() +
  theme(panel.border = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = '#EAEAEA')) +
  theme(panel.grid = element_line(colour = "white", size = 5, linetype = "longdash")) +
  labs(title = "Fairweather Fans?", subtitle = "impact of reaching NFL playoffs upon average home attendance", x = "", y = "Playoff boost: 000s of additional spectators per match (average) during playoff seasons") +
  theme(plot.title = element_text(size = 24, face = "bold", family = "Rockwell")) +
  theme(plot.subtitle = element_text(size = 16, face = "bold", family = "Rockwell")) +
  theme(axis.title = element_text(face = "bold", family = "Rockwell")) +
  theme(axis.text = element_text(family = "Rockwell", margin = margin(0,0,50,0)))
ggsave("tt2020_w6_NFLattendances.png", width = 25, height = 20, units = "cm", dpi = 300)





















