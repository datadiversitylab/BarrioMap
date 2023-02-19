###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################


server <- function(input, output, session) {
  
  #Render the initial map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, 
                                     zoomSnap = 0.01,
                                     crs = leafletCRS(
                                       scales = 1
                                     )
    ))%>% 
      addTiles() %>%
      addProviderTiles("OpenStreetMap") %>%
      addScaleBar(position = 'bottomleft') %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 5)
  })
  
  
  observe({
    updateNumericInput(session = session,
                       inputId = "longitude",
                       value = input$map_center$lng)
    updateNumericInput(session = session,
                       inputId = "latitude",
                       value = input$map_center$lat)
  })
  
  observe({
    ## Value of the scale in meters/px
    scale <- input$scale
    ## Get the zoom level for a given number of meters
    zl = log2(( 40075016.686 * abs(cos(input$map_center$lat * pi/180)))/input$scale) - 8
    output$scaleL <- renderText({ paste("Testing:", round(metesrPerPixel, 5), "m/pixel" ) })
    
    ## Get the meters for a given map
    metesrPerPixel = 40075016.686 * abs(cos(input$map_center$lat * pi/180)) / 2^(input$map_zoom+8)
    output$zoomL <- renderText({ paste("Testing:", round(zl, 5), "Zoom level" ) })
    
    lat <- as.numeric(input$latitude)
    lng <- as.numeric(input$longitude)
    
    #Render the new map
    isolate({
      leafletProxy("map") %>%
        setView(lng = lng, lat = lat, zoom = zl) %>% 
        leaflet(options = leafletOptions(minZoom = zl, maxZoom = zl))
    })
  })
  
  
}
