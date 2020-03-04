# Week 10 NHL

library(tidyverse)
library(ggrepel)
library(patchwork)


# download data
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# correct variable name [checked with wikipedia that it is goals and not games] - corrected at source subsequently
#top_250 <- top_250 %>% rename(total_goals = total_games)

# create totals per season per player [need to remove TOT row first]
season_goals_simple <- season_goals %>% filter(team != "TOT") %>% group_by(player, season) %>% 
  summarise(season_goals_tot = sum(goals), 
            goals_even_tot = sum(goals_even), 
            goals_power_play_tot = sum(goals_power_play), 
            goals_short_handed_tot = sum(goals_short_handed),
            goals_game_winner_tot = sum(goals_game_winner))


# create season averages per player
season_goals_simple <- season_goals_simple %>% group_by(player) %>% 
  mutate(season_goals_avg = mean(season_goals_tot, na.rm = T), 
         goals_even_avg = mean(goals_even_tot, na.rm = T), 
         goals_power_play_avg = mean(goals_power_play_tot, na.rm = T), 
         goals_short_handed_avg = mean(goals_short_handed_tot, na.rm = T),
         goals_game_winner_avg = mean(goals_game_winner_tot, na.rm = T))


# simplify dataframe
season_goals_simple_avgs <- season_goals_simple %>% select(player, 8:12) %>% distinct()

# join to total career goals per player
season_goals_simple_avgs <- season_goals_simple_avgs %>% left_join(select(top_250, player, total_goals))

# add label category
season_goals_simple_avgs <- season_goals_simple_avgs %>% mutate(labels = case_when(
                                                                          season_goals_avg > 40 ~ player,
                                                                          T ~ NA_character_))

# charts

# total goals, season average
total <- season_goals_simple_avgs %>% 
  ggplot(aes(total_goals, season_goals_avg)) +
  geom_point(colour = 'blue', alpha = 0.4) +
  theme_minimal() +
  geom_text_repel(aes(label = labels), size = 3, colour = 'blue', fontface = 'bold') +
  theme(panel.border = element_rect(colour = 'blue', linetype = 'dashed', fill = NA)) +
  labs(title = "Average number of goals per season: all goals", x = "total career goals", y = "goals per season (mean)") +
  theme(title = element_text(colour = 'blue', face = 'bold'), axis.text = element_text(colour = 'blue'))


# goals when even strength, season average
even <- season_goals_simple_avgs %>% 
  ggplot(aes(total_goals, goals_even_avg)) +
  geom_point(colour = 'red', alpha = 0.4) +
  theme_minimal() +
  geom_text_repel(aes(label = labels), size = 3, colour = 'red', fontface = 'bold') +
  theme(panel.border = element_rect(colour = 'red', linetype = 'dashed', fill = NA)) +
  labs(title = "Average goals per season: even strength", x = "", y = "even-strength goals per season (mean)") +
  theme(title = element_text(colour = 'red', size = 7, face = 'bold'), axis.text = element_text(colour = 'red'), axis.title = element_text(size = 8)) 


# goals during power play, season average
power <- season_goals_simple_avgs %>% 
  ggplot(aes(total_goals, goals_power_play_avg))  +
  geom_point(colour = 'red', alpha = 0.4) +
  theme_minimal() +
  geom_text_repel(aes(label = labels), size = 3, colour = 'red', fontface = 'bold')+
  theme(panel.border = element_rect(colour = 'red', linetype = 'dashed', fill = NA)) +
  labs(title = "Average goals per season: power play", x = "", y = "power play goals per season (mean)") +
  theme(title = element_text(colour = 'red', size = 7, face = 'bold'), axis.text = element_text(colour = 'red'), axis.title = element_text(size = 8))



# goals short handed, season average
short <- season_goals_simple_avgs %>% 
  ggplot(aes(total_goals, goals_short_handed_avg)) +
  geom_point(colour = 'red', alpha = 0.4) +
  theme_minimal() +
  geom_text_repel(aes(label = labels), size = 3, colour = 'red', fontface = 'bold') +
  theme(panel.border = element_rect(colour = 'red', linetype = 'dashed', fill = NA)) +
  labs(title = "Average goals per season: short handed", x = "", y = "short-handed goals per season (mean)") +
  theme(title = element_text(colour = 'red', size = 7, face = 'bold'), axis.text = element_text(colour = 'red'), axis.title = element_text(size = 8))



# game winning goals, season average
winner <- season_goals_simple_avgs %>% 
  ggplot(aes(total_goals, goals_game_winner_avg)) +
  geom_point(colour = 'red', alpha = 0.4) +
  theme_minimal() +
  geom_text_repel(aes(label = labels), size = 3, colour = 'red', fontface = 'bold') +
  theme(panel.border = element_rect(colour = 'red', linetype = 'dashed', fill = NA)) +
  labs(title = "Average goals per season: game winning goals", x = "", y = "game winning goals per season (mean)") +
  theme(title = element_text(colour = 'red', size = 7, face = 'bold'), axis.text = element_text(colour = 'red'),  axis.title = element_text(size = 8))


# arrange plots
layout <- '
          ABBC
          DBBE
          '

even + total + power + short + winner +
  plot_layout(design = layout) +
  plot_annotation(
    title = "NHL all-time top goal scorers: Gretsky the GOAT, Bossy the Boss?*",
    caption = "*I know nothing about ice hockey", 
    theme = theme(plot.title = element_text(size = 20, colour = 'blue'), plot.caption = element_text(face = 'italic', colour = 'blue'))
  ) & theme(text = element_text(family = 'Rockwell'))

ggsave("tt2020_w10_NHLgoals.png", width = 35, height = 20, units = 'cm', dpi = 300)
