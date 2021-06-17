# Librerías ====
library(tidyverse)
library(lubridate)
library(ggthemes)
# Load
library(wesanderson)

# Lectura de datos ====
vaccines <- read_csv("data/interim/country_vaccinations.csv") 
covid_cases <- read_csv("data/raw/owid-covid-data.csv")

# Limpieza BD ====
vaccines_clean <- vaccines %>% mutate(date = lubridate::as_datetime(date, format = '%d/%m/%y'))
write_csv(vaccines_clean, "data/interim/country_vaccinations.csv")
data <- left_join(vaccines, covid_cases, by = c("iso_code", "date"))

 
df <- data %>% select(country:daily_vaccinations, total_cases:total_deaths_per_million)

daily <- df %>% filter(!is.na(total_vaccinations.x)) %>% 
  select(country, iso_code, date, daily_vaccinations, new_cases) %>% 
  group_by(country, iso_code, week = week(date)) %>% 
  summarise(daily_vaccines = sum(daily_vaccinations, na.rm = TRUE),
            new_cases = sum(new_cases, na.rm = TRUE)) 

write_csv(daily, "data/interim/dalily_mx_usa.csv")

daily %>% 
  filter(iso_code %in% c('MEX', 'USA'), week < 49) %>%
  ggplot(aes(week, daily_vaccines, color = country)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format())
daily %>% 
  filter(iso_code %in% c('MEX', 'USA'), week < 49) %>%
  ggplot(aes(week, new_cases)) +
  geom_point(color = 'grey')  +
  geom_line(aes(color = country)) +
  scale_y_continuous(labels = scales::comma_format()) +
  ylab("Vacunas aplicadas") +
  theme(legend.position = 'top') +
  labs(color = "Country") +
  ggtitle("Número de vacunas aplicadas semanalmente") +
  xlab("") +
  scale_x_continuous(breaks = c(4, 8, 12, 16),
                     labels = c("Enero", "Febrero", "Marzo", "Abril")) 


# Investigar por qué




