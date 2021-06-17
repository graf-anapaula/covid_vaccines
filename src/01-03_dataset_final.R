# Library ====
library(tidyverse)

# Lectura de datos ====
df <- read_csv("data/interim/vacunados_porcentajes.csv")
gdp <- read_csv("data/raw/GDP_oecd.csv")

# Limpieza porcentaje_vacunados ====
paises_not <- c("Wales", "Scotland", "Northern Ireland", "England", "Guernsey",
                "Jersey", "Northern Cyprus")
'%not_in%' <- Negate('%in%')

df_clean <- df %>% 
  filter(country %not_in% paises_not) %>% 
  group_by(iso_code, country) %>% 
  summarise(percent_vaccinated = min(percent_vaccinated),
            percent_fully_vaccinated = min(percent_fully_vaccinated),
            pop_total = min(pop_total))
# Limpieza GDP ====
gdp_clean <- gdp %>% janitor::clean_names() %>% 
  group_by(location) %>% 
  filter(time == max(time),
         measure == 'MLN_USD') %>% 
  select(iso_code = location, gdp = value)

# Unión GDP y % de vacunas =====
vacunas_gdp <- left_join(df_clean, gdp_clean, by = 'iso_code')

write_csv(vacunas_gdp,"data/interim/vacunas_gdp.csv")

# Siguiente data set ====
data <- read_csv("data/interim/vacunas_gdp.csv")
casos <- read_csv("data/raw/owid-covid-data.csv")

casos_clean <- casos %>% group_by(iso_code, location) %>% 
  summarise(total_cases = max(total_cases, na.rm = TRUE),
           total_deaths = max(total_deaths, na.rm = TRUE)) %>% 
  rename(country = location)

casos_vac <- left_join(data, casos_clean, by = c('iso_code')) %>% 
  select(-country.y) %>% 
  rename(country = country.x) %>% 
  mutate(percent_cases = total_cases / pop_total,
         percent_deaths = total_deaths / pop_total)

write_csv(casos_vac, 'data/interim/gdp_cases_vac.csv')
