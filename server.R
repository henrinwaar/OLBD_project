library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(geojson)
library(htmltools)
library(RCurl)
library(XML)
library(xml2)


giveHTMLContent <- function(surface, description, adress, image){
  content = paste(sep = "<br/>",
                  paste0("<img src =" , image, ' width="40" height="40">'),
                  adress,
                  paste("Surface = ", surface, "m^2", sep = " "),
                  description
                  )
  return(content)
}

function(input, output) {

  ## Interactive Map ###########################################
  # Create the map
  
  output$map <- renderLeaflet({
    leaflet() %>%
    addTiles()%>% 
    
      ## --!!!-- A compléter avec les variables ---!!!---
      ## data$longitude --> df$longitude, data$latitude --> df$latitude, "Ceci est un parc" --> df$description, "http://127.0.0.1:7915/tree.png" --> df$image, data$surface_to --> df$surface, data$nom_ev --> df$name
      addMarkers(lng = data$longitude, data$latitude, icon = makeIcon("tree.png", 40, 40), popup = giveHTMLContent(data$surface_to, "Ceci est un parc", data$adresse, "http://127.0.0.1:7915/tree.png"), label = htmlEscape(data$nom_ev) )%>% 
    setView(lng = 2.3522219, lat = 48.856614, zoom = 15)
  })
  
}