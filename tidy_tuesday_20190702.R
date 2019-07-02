## 2019-07-02 Media Franchises

library(tidyverse)

# load data
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# create decade variable
media_franchises <- media_franchises %>% mutate(decade = paste0(year_created - year_created %% 10, "s"))

# manual colour palette
cols <- c("Book sales" = "#DE9DC8", "Box Office" = "#444EC1", "Home Video/Entertainment" = "#DD005D", "TV" = "#AFBDE4", 
"Video Games/Games" = "#090088", "Merchandise, Licensing & Retail" =  "#9A0078", "Music" = "#DE9EDF", "Comic or Manga" = "#524F8C")
    
# bar chart                                            
media_franchises %>% group_by(decade, revenue_category) %>% summarise(totrev = sum(revenue)) %>% 
  ggplot(., aes(revenue_category, totrev, fill = revenue_category)) +
  geom_col() +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "m")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~ decade, ncol = 2) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  labs(title = "Total revenue earned by media franchises, by decade and revenue category", x = "") +
  ylab("\nTotal revenue, US$m\n") +
  theme(plot.title = element_text(size = 18), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 12)) +
  theme(panel.background = element_rect(fill = "#FCFBF7"))
        

# manual colour palette v2
cols2 <- c("Other" = "#DE9DC8", "Box Office" = "#444EC1", "Home Video/Entertainment" = "#DD005D", 
          "Video Games/Games" = "#090088", "Merchandise, Licensing & Retail" =  "#9A0078")

# lollipop chart
media_franchises %>% mutate(rev_cat = fct_lump(revenue_category, 4)) %>% group_by(decade, rev_cat) %>% summarise(totrev = sum(revenue)) %>% 
  ggplot(., aes(rev_cat, totrev)) +
  geom_point(aes(colour = rev_cat), size = 3.5) +
  geom_segment(aes(x = rev_cat, xend = rev_cat, 
                   y = 0, yend = totrev), 
                   linetype="dashed", 
                   size=0.5,
                   colour = "grey40") +  
  scale_colour_manual(values = cols2) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "bn")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  facet_wrap(~ decade, ncol = 2) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  labs(title = "Total revenue earned by media franchises, by decade and revenue category", x = "") +
  ylab("\nTotal revenue, US$bn\n") +
  theme(plot.title = element_text(size = 18), axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(panel.background = element_rect(fill = "#FCFBF7"))
ggsave("tt20190702.png", width = 30, height = 25, units = "cm", dpi = 150)
