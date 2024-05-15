library(tidyverse)
library(lubridate)
library(ggthemes)

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# 1.0 Data Manipulation ----

filter_countries <- c("Germany", "United Kingdom", "France", "Spain", "United States")

covid_cum_cases <- covid_data_tbl %>% 
  select(location, date, total_cases) %>% 
  filter(location %in% filter_countries,
         year(date) < 2022 |
         year(date) == 2022 & month(date) < 5 & day(date) <= 19) %>% 
  mutate(across(total_cases, ~replace_na(., 0))) %>% 
  mutate(label_text = scales::number(total_cases,
                                     big.mark = "."))

europe_cases <- covid_data_tbl %>% 
  select(continent,date,total_cases) %>% 
  filter(continent == "Europe",
         year(date) < 2022 |
         year(date) == 2022 & month(date) < 5 & day(date) <= 19) %>% 
  mutate(across(total_cases, ~replace_na(., 0))) %>%
  group_by(continent,date) %>% 
  summarize(total_cases = sum(total_cases)) %>% 
  mutate(label_text_E = scales::number(total_cases,
                                     big.mark = "."))

custom_label_country <- covid_cum_cases %>% 
  filter(date == "2022-04-19" & location == "United States")

custom_label_europe <- europe_cases %>% 
  filter(date == "2022-04-19")

# 2.0 Data Visualization ----

covid_cum_cases %>%
  
  ggplot(aes(date, total_cases, color = location)) +
  
  # Geometries
  geom_smooth(method = "loess", span = 0.05, size = 0.7) +
  geom_smooth(data = europe_cases, aes(color = continent), method = "loess", span = 0.05, size = 0.7) +
  geom_label(data = custom_label_country, aes(label = label_text),
             nudge_x = -80, show.legend = FALSE) +
  geom_label(data = custom_label_europe, aes(label = label_text_E, color = continent),
             nudge_x = -100, nudge_y = -30, show.legend = FALSE) + 
  
  # Scales
  scale_y_continuous(labels = scales::number_format(scale = 1/1e6,
                                                    suffix = "M")) +
  scale_x_date(breaks = "1 month",date_labels = "%B '%y", position = "bottom") +
  
  # Themes
  theme_dark() + 
  scale_color_viridis_d(option = "H") + 

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom") +
  
  guides(color = guide_legend(nrow = 2)) +

  # Labels
  labs(
  title = "COVID-19 confirmed cases worldwide",
  subtitle = "As of 19/04/2022",
  x = "",
  y = "Cumulative Cases",
  color = "Continent / Country"
  )
  