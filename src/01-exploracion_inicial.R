# Librerías ====
library(tidyverse)
library(lubridate)

# Lectura de datos ====
df <- read_csv("data/raw/country_vaccinations.csv")

# Exploración inicial
df %>% select(iso_code) %>% unique()
df %>% select(country) %>% unique()

uk <- c("England", "United Kingdom")
latam <- c("United States", "Mexico", "Canada", "")

df %>% filter(country %in% uk, !is.na(total_vaccinations))

df %>% filter(!is.na(daily_vaccinations_per_million)) %>% 
  group_by(country) %>% 
  summarise(total_per_hundred = sum(daily_vaccinations_per_million)) %>% 
  arrange(desc(total_per_hundred))






