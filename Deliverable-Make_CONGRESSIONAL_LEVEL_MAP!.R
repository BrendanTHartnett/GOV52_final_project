library(sf)
library(leaflet)
library(tigris)
library(dplyr)
library(leaflet.providers)
library(leaflet.extras)

#Load Map data
    #Available on the Dataverse (due to large size)
#             CongressDat <- read_sf("[INSERT DATAVERSE FILE DOWNLOAD]ClippedCongressionalMapPolygons.shp")

CongressMap <- leaflet(CongressDat) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", cllpr22)(cllpr22),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(
                "District: "
                , z$Code
                , "<br>"
                , z$rounded
                , "% "
                ,"doubting election"
                , "<br>"
              )) %>%
  addResetMapButton() %>%
  addTiles() %>%
  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = TRUE)) %>% 
  setView(-98, 39, zoom = 3.5)
CongressMap
