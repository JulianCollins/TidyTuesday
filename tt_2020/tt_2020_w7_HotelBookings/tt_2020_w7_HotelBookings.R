# week 7 - Hotel Bookings

library(tidyverse)
library(extrafont)
library(patchwork)

# download data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

# months
hotels$arrival_date_month <- factor(hotels$arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), ordered = T)

hotels$arrival_date_month <- fct_recode(hotels$arrival_date_month, Jan = "January", Feb = "February", Mar = "March", Apr = "April", May = "May", Jun = "June", Jul = "July", Aug = "August", Sep = "September", Oct = "October", Nov = "November", Dec = "December")

# add country count
hotels <- hotels %>% group_by(country) %>% mutate(country_cnt = n()) %>% ungroup()

# add country count - check-outs only
hotels <- hotels %>% group_by(country) %>% mutate(country_cnt_checkout = ifelse(reservation_status == "Check-Out", n(), NA_integer_)) %>% ungroup()

# add length of stay total
hotels <- hotels %>% mutate(stay_nights_all = stays_in_weekend_nights + stays_in_week_nights)

# add length of stay categories
hotels <- hotels %>% mutate(hol_length = ifelse(stay_nights_all >= 1 & stays_in_week_nights <= 3, "Short Break",
                                              ifelse(stay_nights_all >= 4 & stay_nights_all <=7, "Week",
                                                ifelse(stay_nights_all >= 8 & stay_nights_all <=16, "Fortnight", "Other"))))


# only complete year is 2016, so filter other months out
hotels_2016 <- hotels %>% filter(arrival_date_year == 2016)

# will only use completed bookings in charts
hotels_2016 <- hotels_2016 %>% filter(reservation_status == "Check-Out")

# five countries make up 2/3 of bookings, so focus on these
hotels_2016 <- hotels_2016 %>% filter(country %in% c("DEU", "ESP", "FRA", "GBR", "PRT"))

# add counts per country/month/hotel/length > makes dealing with gaps easier
hotels_2016 <- hotels_2016 %>% group_by(country, arrival_date_month, hotel, hol_length) %>% mutate(cnt = n()) %>% ungroup()

# simplify df
hotels_2016 <- hotels_2016 %>% select(country, arrival_date_month, hotel, hol_length, cnt) %>% distinct()

# rename month
hotels_2016 <- hotels_2016 %>% rename(month = arrival_date_month)

# create empty grid of all combinations
hotels_2016_expanded <- expand(hotels_2016, country, month, hotel, hol_length)

# join expanded df to data
hotels_2016_expanded <- hotels_2016_expanded %>% left_join(hotels_2016, by = c("country", "month", "hotel", "hol_length"))

# replace NA with zero
hotels_2016_expanded$cnt[is.na(hotels_2016_expanded$cnt)] <- 0 

# manually specify colours for each country
country_colours <- c("DEU" = 'grey10', "ESP" = 'orange', "FRA" = 'blue2', "GBR" = 'red1', "PRT" = 'forestgreen')


hol_short <- hotels_2016_expanded %>% filter %>% filter(hol_length == "Short Break") %>% 
  ggplot(aes(month, cnt, group = country, fill = country, colour = country)) +
  geom_area(alpha = 0.4) +
  scale_fill_manual(values = country_colours) +
  scale_colour_manual(values = country_colours) +
  facet_wrap(~ hotel, ncol = 1, scales = 'free') +
  theme_minimal() +
  labs(title = "Short Break", x = "", y = "Number of completed bookings") +
  theme(plot.title = element_text(size = 16, family = 'Skia')) +
  theme(strip.text = element_blank()) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #theme(strip.text = element_text(size = 12, face = 'bold')) +
  #theme(strip.text = element_text(family = 'Skia')) +
  theme(axis.text = element_text(family = 'Skia')) +
  theme(axis.title = element_text(family = 'Skia')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(10,2,2,2, "mm"))
  

#theme(strip.background = element_rect(fill = 'grey40'))
  

hol_week <- hotels_2016_expanded %>% filter(hol_length == "Week") %>% 
  ggplot(aes(month, cnt, group = country, fill = country, colour = country)) +
  geom_area(alpha = 0.4) +
  scale_fill_manual(values = country_colours, name = "Visitors' country of origin") +
  scale_colour_manual(values = country_colours, guide = FALSE) +
  facet_wrap(~ hotel, ncol = 1, scales = 'free') +
  theme_minimal() +
  labs(title = "Week", x = "", y = "") +
  theme(plot.title = element_text(size = 16, family = 'Skia')) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text = element_blank()) +
  #theme(strip.text = element_text(size = 12, face = 'bold')) +
  #theme(strip.text = element_text(family = 'Skia')) +
  theme(axis.text = element_text(family = 'Skia')) +
  theme(axis.title = element_text(family = 'Skia')) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(family = 'Skia')) +
  theme(legend.title = element_text(family = 'Skia')) +
  theme(plot.margin = margin(10,2,2,2, "mm"))



hol_fortnight <- hotels_2016_expanded %>% filter(hol_length == "Fortnight") %>% 
  ggplot(aes(month, cnt, group = country, fill = country, colour = country)) +
  geom_area(alpha = 0.4) +
  scale_fill_manual(values = country_colours) +
  scale_colour_manual(values = country_colours) +
  facet_wrap(~ hotel, ncol = 1, scales = 'free', strip.position = 'right') +
  theme_minimal() +
  labs(title = "Fortnight", x = "", y = "") +
  theme(strip.text = element_text(size = 14, face = 'bold')) +
  theme(plot.title = element_text(size = 16, family = 'Skia')) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text = element_text(family = 'Skia')) +
  theme(axis.text = element_text(family = 'Skia')) +
  theme(axis.title = element_text(family = 'Skia')) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(10,2,2,2, "mm"))


# assemble plots
hol_short + hol_week + hol_fortnight + plot_layout(nrow = 1) + plot_annotation(
  title = "A Holiday for all Seasons",
  subtitle = "Who goes where? And when? \nHoliday length by month for two hotels in Portugal. \nCountry of residence of visitors comprising 2/3 of these hotels' customers.",
  caption = "data restricted to 2016 only - other years incomplete",
  theme = theme(plot.title = element_text(size = 22, family = "Skia"), plot.subtitle = element_text(size = 16, family = "Skia"), plot.caption = element_text(size = 10, family = "Skia"))
)

ggsave("tt2020_w7_HotelBookings.png", width = 30, height = 20, units = "cm", dpi = 300)




