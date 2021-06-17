# Librerías ====
library(tidyverse)
library(countrycode)

# Nuevas tablas =====
vacunas <- read_csv("data/interim/vacunas_por_tipo.csv")
poblacion <- read_csv("data/raw/WPP2019_TotalPopulationBySex.csv")

# Limpieza ====
poblacion_clean <- poblacion %>% 
  janitor::clean_names() %>% 
  filter(time == 2020, variant == 'Medium') %>% 
  mutate(iso_code = countrycode::countrycode(location, 'country.name', 'iso3c'), 
         pop_total = pop_total * 1e3) %>% 
  filter(!is.na(iso_code)) %>% 
  select(iso_code, country = location, time, pop_total)

vacunas_max <- vacunas %>% 
  mutate(people_vaccinated = if_else(!is.na(people_vaccinated), people_vaccinated, 0),
         people_fully_vaccinated = if_else(!is.na(people_fully_vaccinated), people_fully_vaccinated, 0)) %>% 
  group_by(iso_code, country) %>% 
  summarise(people_vaccinated = max(people_vaccinated, na.rm = TRUE),
            people_fully_vaccinated = max(people_fully_vaccinated))

# Joins ==== 
df <- left_join(vacunas_max, poblacion_clean, by = 'iso_code') 

data <- df %>% group_by(country.x, iso_code) %>% 
  summarise(percent_vaccinated = people_vaccinated / pop_total,
            percent_fully_vaccinated = people_fully_vaccinated / pop_total,
            pop_total = mean(pop_total)) %>% 
  select(country = country.x, iso_code, percent_vaccinated, percent_fully_vaccinated, 
         pop_total)

write_csv(data, 'data/interim/vacunados_porcentajes.csv')

data <- read_csv(here::here('data/interim/vacunados_porcentajes.csv'))
data_scaled <- data %>% 
  arrange(desc(percent_fully_vaccinated)) %>% 
  mutate('Personas vacunadas' = scales::percent(percent_vaccinated, 1L),
         'Personas totalmente vacunadas' = scales::percent(percent_fully_vaccinated, 1L)) %>% 
  select('País' = country.x, 'Personas vacunadas', 'Personas totalmente vacunadas') 

table <- DT::datatable(data_scaled, fillContainer = FALSE, options = list(pageLength = 10))
class(table)


