## 2019-07-02 - Week 27 - Media Franchises 

library(tidyverse)

# load data
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# create decade variable
media_franchises <- media_franchises %>% mutate(decade = paste0(year_created - year_created %% 10, "s"))

# create time since created variable
media_franchises <- media_franchises %>% mutate(years_since_created = 2018 - year_created)

# create total revenue per franchise
media_franchises <- media_franchises %>% group_by(franchise) %>% mutate(revenue_total = sum(revenue))

# create revenue per year variable
media_franchises <- media_franchises %>% mutate(annualised_revenue = revenue_total / years_since_created)

media_franchises %>% arrange(desc(annualised_revenue)) %>% View()

# manual colour palette
  cols3 <- c("1960s" = "#C06962", "1970s" = "#BADA96", "1980s" = "#EDA115", "1990s" = "#A283A0", "2000s" = "#F8F2B3", "2010s" = "#F1C8A9")

# lollipop chart - annualised revenue
media_franchises %>% filter(annualised_revenue > 1) %>%  
  ggplot(., aes(fct_reorder(franchise, annualised_revenue), annualised_revenue)) +
  geom_point(size = 5, colour = "black") +
  geom_point(aes(colour = decade), size = 4) +
  geom_segment(aes(x = franchise, xend = franchise, 
                   y = 0, yend = annualised_revenue), 
               linetype="dashed", 
               size=0.3,
               colour = "grey40") +  
  scale_colour_manual(values = cols3, name = "Decade of release") +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "bn"), breaks = seq(1:5)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 12)) +
  labs(title = "Annualised revenue earned by media franchises", subtitle = "franchises with > $1bn annualised income", x = "") +
  ylab("\nTotal annualised revenue, US$bn\n") +
  theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 14), axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(plot.background = element_rect(fill = "#FFFBF9"))
ggsave("tt2019_w27.png", width = 24, height = 20, units = "cm", dpi = 150)





########## rejeced charts - grouping without franchise doesn't really make sense #########

# manual colour palette 
cols <- c("Book sales" = "#47761E", "Box Office" = "#FED985", "Home Video/Entertainment" = "#F09E71", "TV" = "#61B5CB", 
          "Video Games/Games" = "#93B592", "Merchandise, Licensing & Retail" =  "#D5A0C4", "Music" = "#C4EB98", "Comic or Manga" = "#449FAF")


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
