###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################


server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addTiles() %>%       # Add default OpenStreetMap map tiles 
      
      #      setView(lng = as.numeric(input$longitude) , lat = as.numeric(input$latitude) , zoom = input$scale) %>% 
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
    
    
    leafletProxy("map") %>%
      setView(lng = lng, lat = lat, zoom = scale)
  })
  
}
