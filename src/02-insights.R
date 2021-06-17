# Librerías ====
library(tidyverse)
library(scales)
library(hrbrthemes)
library(sysfonts)
library(ggthemes)
library(stringr)
library(RColorBrewer)

# Lectura de datos ====
cases <- read_csv("data/raw/owid-covid-data.csv")
vaccines <- read_csv("data/raw/country_vaccinations.csv")
population <- janitor::clean_names(read_csv("data/raw/WPP2019_TotalPopulationBySex.csv")) %>% 
  filter(time == 2021, variant == "Medium") %>% 
  select(country = location, pop_total)

# Fonts ====
font_add_google('Montserrat', family = "Montserrat")
display.brewer.pal(n = 5, name = 'Dark2')

# Transformación de datos ====

# Promedio de vacunas diarias por país ====
daily_info <- vaccines %>% 
  select(country, daily_vaccinations) %>% 
  group_by(country) %>% 
  summarise(daily_mean = mean(daily_vaccinations, na.rm  = TRUE)) %>% 
  filter(country != "England") %>% 
  arrange(desc(daily_mean)) %>%
  head(20)

top_5 <- daily_info %>% head(5) %>% select(country) %>% pull()

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

daily_info_low <- vaccines %>% 
  select(country, daily_vaccinations) %>% 
  group_by(country) %>% 
  summarise(daily_mean = mean(daily_vaccinations, na.rm  = TRUE)) %>% 
  arrange(daily_mean) %>% 
  head(20)

daily_info_low %>% 
  mutate(country = factor(country, levels = country)) %>% 
  ggplot(aes(country, daily_mean)) +
  geom_segment(aes(x = country, xend = country, y = 0, yend = daily_mean), color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = "País", y = "", title = "Promedio de vacunas diarias", subtitle = "Los 20 países más bajos")


paises <- daily_info %>% 
  head(5) %>% 
  select(country) %>% 
  pull()

crecimiento <- vaccines %>% 
  select(country, date, daily_vaccinations) %>% 
  group_by(country, date) %>% 
  summarise(daily_mean = mean(daily_vaccinations, na.rm  = TRUE)) %>% 
  filter(country %in% paises) 

crecimiento %>% mutate(País = country) %>% 
  ggplot(aes(date, daily_mean, color = País)) +
  geom_line(size = 1) +
  scale_y_comma() +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Crecimiento de las Vacunas a lo largo del 2021") +
  scale_color_brewer(palette = "Dark2") +
  theme_calc()
  



vac_pob <- left_join(vaccines, population, by = "country")

vac_poblacion %>% mutate(pob_vacunada = people_vaccinated / ((pop_total) * 10e2))
data <- left_join(vaccines, cases, by = c("iso_code", "date", "country" = "location"))

vac_casos <- data %>% select(country, date, total_cases, new_cases, daily_vaccinations, 
                people_vaccinated.x, people_fully_vaccinated.x) %>% 
  filter(!is.na(people_vaccinated.x))


vac_casos %>% filter(country == "Mexico") %>% 
  ggplot() + 
  geom_line(aes(date, total_cases), color = "grey", size = 1) +
  geom_point(aes(date, total_cases), color = "black", shape = 21, fill = '#739e82', size = 1.5) +
  geom_line(aes(date, people_vaccinated.x), color = "grey") +
  geom_point(aes(date, people_vaccinated.x), color = "black", shape = 21, fill = 'coral', size = 1.5) +
  scale_y_comma() +
  theme(text = element_text(size = 12, 
                            family = font_rc_light)
  ) +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%B") +
  theme_calc() +
  ggtitle("Casos COVID vs. Vacunas aplicadas en México")

vac_casos %>% filter(country == "Mexico") %>% 
  ggplot() +
  geom_line(aes(date, daily_vaccinations), color = "grey", size = 1) +
  geom_point(aes(date, daily_vaccinations), color = "black", shape = 21, fill = '#739e82', size = 1.5) +
  geom_line(aes(date, new_cases), color = "grey") +
  geom_point(aes(date, new_cases), color = "black", shape = 21, fill = 'coral', size = 1.5) +
  scale_y_comma() +
  theme(text = element_text(size = 12, 
                            family = font_rc_light)
  ) +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%B") +
  theme_calc() +
  ggtitle("Vacunas aplicadas vs. casos nuevos reportados en México")
  

vac_casos %>% filter(country == "Mexico") %>% 
  ggplot() +
  geom_line(aes(date, new_cases), color = "grey") +
  geom_point(aes(date, new_cases), color = "black", shape = 21, fill = 'coral', size = 1.5) +
  scale_y_comma() +
  theme(text = element_text(size = 12, 
                            family = font_rc_light)
  ) +
  ylab("") + xlab("") +  theme_calc() +
  ggtitle("Número de vacunas aplicadas en México 2021")

vac_casos %>% filter(country %in% top_5) %>% 
  ggplot() +
  facet_wrap(~ country) +
  geom_point(aes(date, new_cases), color = 'coral') +
  geom_line(aes(date, new_cases), color = 'grey') +
  geom_point(aes(date, daily_vaccinations), color = '#739e82') +
  geom_line(aes(date, daily_vaccinations), color = 'grey') +
  scale_y_comma() +
  theme_calc() + ylab("")

vac_casos %>% filter(country == "China")

population <- population %>% 
  mutate(country = replace(country, country == "United States of America", "United States"))

vac_pob <- vaccines %>% 
  group_by(country, iso_code) %>% 
  summarise(total_vac = max(total_vaccinations, na.rm = TRUE), date = ma) %>% 
  left_join(population, by = "country") %>% 
  mutate(pop_total = pop_total * 1000)

# Vacunas top_5 no están por porcentaje en el top_5
vac_pob %>% mutate(vacunados = total_vac / pop_total) %>% 
  arrange(desc(vacunados)) %>% 
  filter(country %in% top_5)

# 
vac_pob %>% mutate(vacunados = total_vac / pop_total) %>% 
  select(country, vacunados) %>% 
  arrange(desc(vacunados)) %>% 
  head(20) %>% 
  mutate(country = factor(country, levels = country)) %>% 
  ggplot(aes(country, vacunados)) +
  geom_segment(aes(country, xend = country, y = 0, yend = vacunados)) +
  geom_point() +
  coord_flip() +
  ylab("Proporción de la población vacunada") + scale_y_percent() +
  xlab("") +
  theme_calc()

# Porcentaje de vacunas por población====
porc_pob_vacunada <- vac_pob %>% mutate(vacunados = total_vac / pop_total) %>%
  arrange(desc(vacunados)) %>%
  write_csv("data/interim/porcentaje_vacunados_por_poblacion.csv")

porc_pob_vacunada %>% mutate(rank = seq(1, 155, by = 1), .before = country) %>% 
  filter(country == "Mexico")



# Secretario: Más casos y vacunas
vac_casos <- left_join(vac_casos, vac_pob, by = "country")



vac_casos %>% filter(country %in% c("Mexico", "Chile")) %>% 
  ggplot(aes(date, color = country)) + 
  geom_line(aes(y = new_cases)) 

vac_casos %>% filter(country %in% c("Mexico", "Chile")) %>% 
  ggplot(aes(date, color = country)) + 
  geom_line(aes(y = daily_vaccinations)) +
  scale_y_comma() +
  ylab(NULL) + xlab(NULL)

por_mex <- vac_casos %>% 
  mutate(vacunas = total_vac / pop_total) %>% 
  filter(country %in% c("Mexico")) %>% 
  group_by(country) %>% 
  summarise(vacunas = mean(vacunas)) %>%
  pull()

