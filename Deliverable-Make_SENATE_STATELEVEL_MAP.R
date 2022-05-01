library(ggplot2)
library(dplyr)
library(dataverse) 
library(tidyverse)
library(leaflet)
library(tidyverse)
library(modelr)
library(tidyr)
library(sf)
library(inlmisc)
library(AddSearchButton)
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(tigris)
library(shiny)
library(leaflet.extras)
library(leaflet.providers)
options(tigris_use_cache = TRUE)

#Load Clipped .shp file:    SenateDat <- read_sf("ClippedSenateMapPolygons.shp")

#Round values to two decimals 
SenateDat$rounded <- round(SenateDat$cllprd2, digits = 2)
SenateDat$rounded <- SenateDat$rounded*100
table(SenateDat$rounded)
#Make map
Statemap2 <- leaflet(SenateDat) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", cllprd2)(cllprd2),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(
                "State: "
                , SenateDat$State
                , "<br>"
                , SenateDat$rounded
                , "% "
                ,"doubting election"
                , "<br>"
              )
  ) %>%
  addResetMapButton() %>%
  addTiles() %>%
  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>% 
  setView(-98, 39, zoom = 3.5)
#View Map
Statemap2
