library(leaflet)
library(htmltools)


function(input, output) {

  ## Interactive Map ###########################################
  # Create the map
  
  output$map <- renderLeaflet({
    leaflet() %>%
    addTiles()%>% 
    addMarkers(lng = as.double(dataFinal$longitude), lat = as.double(dataFinal$latitude), icon = makeIcon("tree.png", 40, 40), 
               popup = giveHTMLContent(dataFinal$name, dataFinal$surface, dataFinal$abstract, dataFinal$address, dataFinal$image), 
               label = htmlEscape(utf8decode(gsub("_", " ", dataFinal$name))) )%>% 
    setView(lng = 2.3522219, lat = 48.856614, zoom = 15)
  })
  
}