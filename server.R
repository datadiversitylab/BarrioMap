###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################


server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addTiles() %>%
      addProviderTiles("OpenStreetMap") %>%
      addScaleBar(position = 'bottomleft')
  })
  
  observe({
    
    lat <- as.numeric(input$latitude)
    lng <- as.numeric(input$longitude)
    scale <- input$scale
    if (input$scale == "custom") {
      scale <- as.numeric(input$custom_scale)
    }
    isolate({
    leafletProxy("map") %>%
      setView(lng = lng, lat = lat, zoom = scale)
    })
  })
  
  #Update the numeric input when the user moves the map around
  observe({
    updateNumericInputIcon(session = session,
                           inputId = "longitude",
                           value = input$map_center$lng)
    updateNumericInputIcon(session = session,
                           inputId = "latitude",
                           value = input$map_center$lat)
  })
  
}
