# TidyTuesday 2020: week 4: Spotify data

library(tidyverse)
library(patchwork)

# data download
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


# count songs by subgenre
spotify_songs_subgenre_counts <- spotify_songs %>% group_by(playlist_subgenre) %>% count()

spotify_songs %>% filter(track_popularity >= 75) %>% count() #2967
spotify_songs %>% filter(track_popularity == 0) %>% count() #2703


# count songs with high popularity by subgenre
spotify_songs_subgenre_top <- spotify_songs %>% filter(track_popularity >= 75) %>% group_by(playlist_subgenre) %>% count() %>% rename(t = n)

# count songs with zero popularity by subgenre
spotify_songs_subgenre_zero <- spotify_songs %>% filter(track_popularity == 0) %>% group_by(playlist_subgenre) %>% count() %>% rename(z = n)

# create single df of counts
spotify_songs_subgenre_counts <- spotify_songs_subgenre_counts %>% left_join(spotify_songs_subgenre_top) %>% left_join(spotify_songs_subgenre_zero)

# add proportion variables
spotify_songs_subgenre_counts <- spotify_songs_subgenre_counts %>% mutate(top_prop = t/n)
spotify_songs_subgenre_counts <- spotify_songs_subgenre_counts %>% mutate(zero_prop = z/n)

# final df
spotify_songs_subgenre_counts <- spotify_songs_subgenre_counts %>% select(1:3,5,4,6)



## plot popular subgenres
p1 <- spotify_songs_subgenre_counts %>% 
  ggplot(aes(fct_reorder(playlist_subgenre, top_prop), top_prop)) +
  geom_col(fill= "darkgreen", alpha = 0.8, width = 0.85) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_linedraw() +
  labs(title = "Spotify song popularity, by sub-genre", subtitle = "proportion of songs with popularity rating of 75 or higher (count = 2967)", x = "", y = "percentage of songs") +
  theme(axis.text = element_text(size = 12))


## plot unpopular subgenres
p2 <- spotify_songs_subgenre_counts %>% 
  ggplot(aes(fct_reorder(playlist_subgenre, zero_prop), zero_prop)) +
  geom_col(fill= "darkred", alpha = 0.8, width = 0.85) +
  scale_y_continuous(labels = scales::percent, breaks = c(0,0.1,0.2)) +
  coord_flip() +
  theme_linedraw() +
  labs(title = "Spotify song unpopularity, by sub-genre", subtitle = "proportion of songs with popularity rating of zero (count = 2703)", x = "", y = "percentage of songs") +
  theme(axis.text = element_text(size = 12))

# combine plots
spotify_patch <- p1 + p2

spotify_patch + plot_annotation(
  title = "Spotify: Memories of Progressive Electro House and New Jack Swing",
  subtitle = "by J. R. Hartley",
  theme = theme(plot.title = element_text(size = 24),
                plot.subtitle = element_text(size = 20))
  )

ggsave("tt2020_w4.png", width = 40, height = 20, units = "cm", dpi = 150)

