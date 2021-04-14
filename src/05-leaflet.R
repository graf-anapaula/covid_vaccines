library(leaflet)
library(tidyverse)
library(maps)
library(sp)
library(maptools)
library(rgeos)
# make sure to use the latest maps package
# it was recently updated at the time of the answer

vaccines <- read_csv("data/interim/vacunas_por_tipo.csv")

world <- map("world") 

leaflet(world)

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





