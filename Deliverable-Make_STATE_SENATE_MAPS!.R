library(sf)
library(leaflet)
library(tigris)
library(dplyr)
library(leaflet.providers)
library(leaflet.extras)
#Read Shapefiles for State Senates from Dataverse:    StateSenates <- read_sf("Upper_House_Data.shp")

StateSenates$rounded <- round(StateSenates$cellpred22, digits = 2)
table(StateSenates$rounded)
StateSenates$rounded <- 100*StateSenates$rounded

table(StateSenates$id)
table(StateSenates$State)

StateSenates$Code <- paste(StateSenates$State, StateSenates$id, sep= "-")


StateSenateMap <- leaflet(StateSenates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", cellpred22)(cellpred22),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(
                "District: "
                , StateSenates$Code
                , "<br>"
                , StateSenates$rounded
                , "% "
                ,"doubting election"
                , "<br>"
              )) %>%
  addResetMapButton() %>%
  addTiles() %>%
  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>% 
  setView(-98, 39, zoom = 3.5)

StateSenateMap
