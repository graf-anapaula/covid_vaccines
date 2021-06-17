# Librerías ====
library(leaflet)
library(tidyverse)
library(maps)
library(sp)
library(maptools)
library(rgeos)
# make sure to use the latest maps package
# it was recently updated at the time of the answer
# Lectura de datos ====

vaccines <- read_csv("data/interim/vacunas_gdp.csv") %>% 
  mutate(country = replace(country, country == "United States", "USA"),
         country = replace(country, country == "United Kingdom", "UK"))

world <- map_data("world") 
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

countries <- vaccines %>%  select(country) %>% unique() %>% pull()

countries_map <- map_data("world", region = countries) 
countries_data <- countries_map %>% 
  group_by(region) %>% 
  summarise(long = mean(long), lat = mean(lat), group = min(group)) %>% 
  select(country = region, long, lat, group) %>% 
  left_join(vaccines, by = "country") %>% 
  mutate(gdp = value) %>% 
  select(-c(value, time)) 


data_world <- world %>% 
  mutate(country = region) %>% 
  left_join(vaccines, by = "country")


# write_csv(countries_data, "data/interim/vacunas_gdp_map.csv")

# Falta INFORMACiÓN
data_world %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = vacunados), color = "white") +
  scale_fill_viridis_c(option = "D")

world <- map("world", fill=TRUE, plot=FALSE)

world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)
cnt <- c("Russia", "Afghanistan", "Albania", "Algeria", "Argentina", "Armenia",
         "Azerbaijan", "Bangladesh", "Belarus")
target <- subset(world_map, country %in% cnt)
leaflet(target) %>% 
  addTiles() %>% 
  addPolygons(weight = 5, color = "green", fill = )





