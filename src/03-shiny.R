# Library ====
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)

# Lectura de datos ====
data <- read_csv("data/raw/country_vaccinations.csv")

# Shiny ====
col_names <- data %>% select(total_vaccinations:daily_vaccinations_per_million) %>% 
  colnames()


ui <- fluidPage(
  mainPanel( 
    leafletOutput(outputId = "mymap"), 
    absolutePanel(top = 60, left = 20, 
                  selectInput("selectID", "Datos:", col_names)
    )))



mymap <- renderLeaflet({
  leaflet(data) %>% 
    setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
    addTiles() %>% 
    addCircles(data = data, lat = ~ latitude, lng = ~ longitude, 
               weight = 1, radius = ~sqrt(mag)*25000, 
               popup = ~as.character(mag), 
               label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), 
               color = ~pal(mag), fillOpacity = 0.5)
})


















