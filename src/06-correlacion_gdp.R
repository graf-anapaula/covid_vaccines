# Librerías ====
library(tidyverse)
library(broom)
# Lectura de datos ====
df <- read_csv("data/interim/vacunas_gdp.csv")

# Calculo correlacion====
df <- df %>% filter(!is.na(gdp))
cor(df$percent_vaccinated, df$gdp)
cor(df$percent_fully_vaccinated, df$gdp)

# Test ====
cor.test(df$percent_fully_vaccinated, df$gdp, method = 'pearson') %>% 
  tidy() 
  # write_csv('src/reporte/datasets/test_fully.csv')

cor.test(df$percent_vaccinated, df$gdp, method = 'pearson') %>% 
  tidy() 
  # write_csv('src/reporte/datasets/test_vac')


# COnclusiones: No podemos afirmar que son diferentes de cero

# Visualización que explique relación =====
df %>% ggplot(aes(gdp, percent_vaccinated)) +
  geom_point() +
  geom_text(aes(label = country), check_overlap = T) +
  # scale_x_continuous(labels = scales::dollar_format(prefix = '$', suffix = 'M')) +
  scale_x_continuous(labels = f <- function(gdp) paste0('$', gdp/1e6, 'M')) +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Personas vacunadas vs. GDP") +
  xlab("GDP") + ylab("Vacunas por población")

'%notin%' <- Negate('%in%')
china_usa <- c("China", "United States", "India")

df %>% filter(country %notin% china_usa) %>% 
  ggplot(aes(gdp, percent_vaccinated)) +
  geom_point() +
  geom_text(aes(label = country), check_overlap = T)+
  scale_x_continuous(labels = f <- function(gdp) paste0('$', gdp/1e6, 'M')) +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Personas vacunadas vs. GDP") +
  xlab("GDP") + ylab("Vacunas por población")

data <- read_csv("data/raw/GDP_oecd.csv") %>% janitor::clean_names()

usa_ger <- c("USA", "FRA")
data %>% filter(location %in% usa_ger, time >= 2018)
