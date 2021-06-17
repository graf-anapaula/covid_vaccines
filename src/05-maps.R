# Library ====
library(tidyverse)
library(maps)

# Lectura de datos ====
data <- read_csv('data/interim/gdp_cases_vac.csv')

# World data ====
world <- map_data("world")

data_world <- world %>% 
  mutate(country = region,
         iso_code = countrycode::countrycode(country, 'country.name', 'iso3c'))

# Joins ====
df <- left_join(data_world, data, by = 'iso_code') %>% 
  select(-country.y, countr = country.x)
write_csv(df, 'data/interim/maps.csv')

df %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent_fully_vaccinated), color = "white") +
  scale_fill_viridis_c(option = "D")

df %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent_vaccinated), color = "white") +
  scale_fill_viridis_c(option = "D")

df %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = gdp), color = "white") +
  scale_fill_viridis_c(option = "D")

df %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent_deaths), color = "white") +
  scale_fill_viridis_c(option = "D") <

df %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent_cases), color = "white") +
  scale_fill_viridis_c(option = "B")

data %>% filter(iso_code %in% c('MEX', 'USA')) %>% 
  View()

