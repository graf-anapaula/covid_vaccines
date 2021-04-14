library(splitstackshape)

df <- read_csv("data/raw/country_vaccinations.csv")



data <- cSplit(df, 'vaccines', sep=", ", type.convert=FALSE) %>% 
  select(country, vaccines_1:vaccines_5) %>% 
  unique()

tipo_de_vacuna <- data %>% pivot_longer(vaccines_1:vaccines_5, names_to = "alt", values_to = "tipo_vacuna") %>% 
  filter(!is.na(tipo_vacuna)) %>% 
  select(country, tipo_vacuna) %>% 
  mutate(vacuna = 1) %>% 
  pivot_wider(names_from = tipo_vacuna, values_from = vacuna) 
  







