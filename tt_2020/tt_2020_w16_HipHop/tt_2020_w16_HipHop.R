# Hip Hop songs

library(tidyverse)
library(extrafont)


# download data
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

polls %>% select(critic_name, critic_country) %>% distinct() %>% count(critic_country)

polls %>% count(critic_name)

polls %>% filter(rank == 1) %>% count(gender)

rankings %>% group_by(year) %>% summarise(tot = sum(n1)) %>% arrange(desc(tot))

# combine vote columns
rankings_long <- rankings %>% select(year, 8:12) %>% pivot_longer(cols = starts_with("n"), names_to = "vote_level", values_to = "vote_count") 

# rename levels in vote_level
rankings_long <- rankings_long %>% mutate(vote_level = fct_recode(vote_level, "1st" = "n1", "2nd" = "n2", "3rd" = "n3", "4th" = "n4", "5th" = "n5"))

rankings_long %>% group_by(year, vote_level) %>% summarise(total = sum(vote_count)) %>% filter(year >= 1982) %>% 
  ggplot(aes(year, vote_level, fill = total)) +
  theme_minimal(base_family = "Fredoka One") +
  geom_tile(colour = "black", size = 0.3, linetype = 6) +
  coord_equal(clip = "off") +
  #scale_fill_distiller(palette = 11, direction = 1, name = "number of votes") +
  scale_fill_viridis_c(alpha = 0.9, option = "E", name = "number of votes") +
  scale_x_continuous(breaks = seq(1985, 2015, 5), labels = seq(1985, 2015, 5)) +      
  theme(legend.position = c(0.8, 0.8)) +
  theme(legend.direction = "horizontal") +
  annotate(geom = "segment", x = 1994, y = 8.75, xend = 1994, yend = 1, colour = "gold3", arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "text", x = 1995, y = 9.5, label = "The Notorious B.I.G. [Juicy] \n& Nas [N.Y. State Of Mind]", size = 3, colour = "gold3", fontface = "bold", family = "Fredoka One") +
  annotate(geom = "segment", x = 1992, y = 7.5, xend = 1992, yend = 1, colour = "gold3", arrow = arrow(length = unit(2.5, "mm"))) +
  annotate(geom = "text", x = 1988, y = 8, label = "Dr Dre ft. Snoop Doggy Dogg [Nuthin’ But A ‘G’ Thang] \n & The Pharcyde [Passin’ Me By]", size = 3, colour = "gold3", fontface = "bold", family = "Fredoka One") +
  annotate(geom = "segment", x = 1989, y = 6, xend = 1989, yend = 1, colour = "gold3", arrow = arrow(length = unit(2.5, "mm"))) +
  annotate(geom = "text", x = 1986, y = 6.25, label = "Public Enemy [Fight The Power]", size = 3, colour = "gold3", fontface = "bold", family = "Fredoka One") +
  theme(panel.grid = element_blank()) +
  labs(title = "Hip-Hop Golden Era", x = "", y = "Number of votes \nby preference", subtitle ="BBC Music surveyed 107 music critics (male: n = 101, US based: n = 73) who voted for their top 5 hip-hop tracks of all time",  caption = "chart: Julian Collins    data: BBC Music") +
  theme(plot.title = element_text(size = 24, vjust = 6)) +
  theme(plot.subtitle = element_text(size = 12, vjust = 10)) +
  theme(axis.title.y = element_text(size = 9, hjust = 0.2))  +
  theme(axis.text.y = element_text(margin = ggplot2::margin(t = 0, r = -25, b = 0, l = 0))) +
  theme(plot.caption = element_text(size = 8, colour = "grey50", hjust = 0.95, vjust = 0.2)) +
  theme(legend.title = element_text(vjust = 0.8))

ggsave("tt_2020_w16_HipHop.png", width = 30, height = 12, units = 'cm', dpi = 300)
  



  

  
