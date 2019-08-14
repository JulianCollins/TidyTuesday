library(tidyverse)
library(lubridate)
devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
library(patchwork)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors <- emperors %>% mutate(reign_length = interval(reign_start, reign_end) / years(1))

# manual correction to deal with Augustus' BCE reign_start
#emperors[1,17] <- 40.83

#emperors$dynasty <- factor(emperors$dynasty, levels = c("Julio-Claudian", "Flavian", "Nerva-Antonine", "Severan", "Gordian", "Constantinian", "Valentinian", "Theodosian"), ordered = T)

emperors <- emperors %>% mutate(C = if_else(year(death) < 0100, "1st Century AD", if_else(year(death) >= 0100 & year(death) < 0200, "2nd Century AD", if_else(year(death) >= 0200, "3rd Century AD", NA_character_)))) 

waffle_C1 <- emperors %>% filter(C == "1st Century AD") %>% waffle_iron(aes_d(group = cause))
waffle_C2 <- emperors %>% filter(C == "2nd Century AD") %>% waffle_iron(aes_d(group = cause))
waffle_C3 <- emperors %>% filter(C == "3rd Century AD") %>% waffle_iron(aes_d(group = cause))

p1 <- ggplot(waffle_C1, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_viridis_d(option = "B", alpha = 0.8) + 
  theme_waffle() +
  theme(legend.position = "none") +
  labs(title = "", x = "", y = "1st Century") +
  theme(axis.title.y = element_text(size = 18, family = "serif"))

p2 <- ggplot(waffle_C2, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_viridis_d(option = "B", alpha = 0.8) + 
  theme_waffle() +
  theme(legend.position = "none") +
  labs(title = "", x = "", y = "2nd Century") +
  theme(axis.title.y = element_text(size = 18, family = "serif"))

p3 <- ggplot(waffle_C3, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_viridis_d(option = "B", alpha = 0.8) + 
  theme_waffle() +
  theme(legend.title = element_blank()) +
  labs(title = "", x = "", y = "3rd Century") +
  theme(axis.title.y = element_text(size = 18, family = "serif")) +
  theme(legend.text = element_text(family = "serif", size = 16)) +
  theme(legend.margin = margin(0, 0.1, 0.1, 0, "cm"))


  p1 + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(1,1,3), heights = c(1,1,1)) + 
  plot_annotation(title = "\"Not that I loved Caesar less, but that I loved Rome more\"",
                  subtitle = "  Roman Emperors, cause of death by century",
                  theme = theme(
                                plot.title = element_text(size = 32, family = "serif", face = "italic"),
                                plot.subtitle = element_text(size = 24, family = "serif")
                                )
                  )
