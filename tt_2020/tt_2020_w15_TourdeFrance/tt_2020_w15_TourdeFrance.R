# Tour de France

library(tidyverse)
library(lubridate)
library(broom)
library(GGally)
library(patchwork)

# download data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
stage_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/stage_data.csv')
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')


# add year
tdf_winners <- tdf_winners %>% mutate(tour_year = year(start_date))

# add average winning time - km/hr
tdf_winners <- tdf_winners %>% group_by(tour_year) %>% mutate(winner_avg_spd = sum(distance / time_overall, na.rm = T))


chart_line <- tdf_winners %>% filter(tour_year >= 1950) %>% 
  ggplot(aes(as_factor(tour_year), winner_avg_spd)) +
  geom_path(aes(group = 1), colour = "#0055A4", size = 2) +
  #geom_point(colour = "#EF4135", fill = "#FFFFFF", size = 3, shape = 21) +
  geom_point(colour = "#EF4135", size = 5, shape = 20) +
  geom_point(colour = "#FFFFFF", size = 2, shape = 20) +
  annotate("rect", xmin = "1950", xmax = "1960", ymin = 30, ymax = 45, fill = "#0055A4", alpha = 0.25) +
  annotate("rect", xmin = "2010", xmax = "2019", ymin = 30, ymax = 45, fill = "#0055A4", alpha = 0.25) +
  expand_limits(y = c(30, 45)) +
  scale_x_discrete(breaks = seq(1950, 2010, 10), labels = seq(1950, 2010, 10)) +
  theme_light() +
  labs(x = "", y = "Average Tour speed (km/hr) of winner") 
  


# stages
tdf_stages <- tdf_stages %>% mutate(TypeNew = fct_collapse(Type, 
                                          Mountain = c("Medium mountain stage", "High mountain stage", "Mountain time trial", "Mountain stage", "Mountain Stage", 
                                                       "Hilly stage", "Stage with mountain(s)", "Stage with mountain"),
                                          Flat = c("Flat stage", "Plain stage", "Flat Stage"),
                                          Other = c("Individual time trial", "Team time trial", "Transition stage", "Intermediate stage", "Flat cobblestone stage", "Plain stage with cobblestones", "Half Stage")
                                          ))

FrFlag <- c("Flat" = "#EF4135", "Other" = "#FFFFFF", "Mountain" = "#0055A4")

chart_bar <- tdf_stages %>% group_by(tour_year, TypeNew) %>% filter(tour_year >= 1950) %>% summarise(type_distance = sum(Distance)) %>% mutate(TN = fct_relevel(TypeNew, "Mountain", "Other", "Flat")) %>% 
  ggplot(aes(as_factor(tour_year), type_distance, fill = TN)) +
  geom_col(position = "fill", colour = "grey30", size = 0.3) +
  geom_hline(yintercept = 0.5, linetype = "dotdash", colour = "white", size = 0.5) +
  scale_x_discrete(breaks = seq(1950, 2010, 10), labels = seq(1950, 2010, 10)) +
  scale_fill_manual(values = FrFlag) +
  theme_light() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Length of Tour by Stage type percentage")


# patchwork
chart_line / chart_bar +
  plot_annotation(title = "Tour de France", subtitle = "Tour winners' speed in latest decade is 20% higher than in the 1950s \ndespite mountain stages forming a higher proportion of Tour length",
                  theme = theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size = 14))) &
  theme(text = element_text('Righteous'))



####################################


# linear regression model 1 for average speed of winner
winner_model <- lm(winner_avg_spd ~ birth_country + age + height + weight + tour_year, data = tdf_winners)

summary(winner_model)

tidy(winner_model)

# linear regression model 2 for average speed of winner - country of birth dropped
winner_model2 <- lm(winner_avg_spd ~ age + height + weight + tour_year, data = tdf_winners)

summary(winner_model2)

tidy(winner_model2)


# linear regression model 3 for average speed of winner - add winner_team
winner_model3 <- lm(winner_avg_spd ~ winner_team + age + height + weight + tour_year, data = tdf_winners)

summary(winner_model3) 

tidy(winner_model3) %>% View()









