# Librerías ====
library(tidyverse)
library(scales)

# Lectura de datos ====
cases <- read_csv("data/raw/owid-covid-data.csv")
vaccines <- read_csv("data/raw/country_vaccinations.csv")

# Transformación de datos ====

# Promedio de vacunas diarias por país ====
daily_info <- vaccines %>% 
  select(country, daily_vaccinations) %>% 
  group_by(country) %>% 
  summarise(daily_mean = mean(daily_vaccinations, na.rm  = TRUE)) %>% 
  arrange(desc(daily_mean)) %>% 
  head(20)

daily_info %>% 
  arrange(daily_mean) %>% 
  mutate(country = factor(country, levels = country)) %>% 
  ggplot(aes(country, daily_mean)) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = daily_mean), color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1500000), breaks = seq(0, 1500000, by = 250000), labels = label_comma()) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = "País", y = "", title = "Promedio de vacunas diarias", subtitle = "Top 20 países")
  



data <- left_join(vaccines, cases, by = c("iso_code", "date"))
