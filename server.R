###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

library(dplyr)
library(leaflet)
library(leaflet.extras)


server <- function(input, output, session) {
  
  # Reactive values for latitude and longitude
  rv <- reactiveValues(latitude = 0 , longitude = 0)
  
  #Render the initial map
  output$map <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, 
                                     zoomSnap = 0.01,
                                     crs = leafletCRS(
                                       scales = 1
                                     ),
                                     attributionControl=FALSE
    )) %>% 
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addScaleBar(position = 'bottomleft') %>%
      setView(lng = -110.9742, lat = 32.2540, zoom = 5)%>%
      addControlGPS(
        options = gpsOptions(
          position = "topright",
          activate = TRUE, 
          autoCenter = TRUE,
          setView = TRUE)) 
    
    #non-functional
    #%>% addSearchOSM(options = searchOptions(autoCollapse = FALSE, minLength = 2))
  }) 
  
  
  observe({
    updateNumericInput(session = session,
                       inputId = "longitude",
                       value = input$map_center$lng)
    updateNumericInput(session = session,
                       inputId = "latitude",
                       value = input$map_center$lat)
  })
  
  observeEvent(input$refresh, {
    ## Value of the scale in meters/px
    scale <- input$scale
    ## Get the zoom level for a given number of meters
    zl = log2(( 40075016.686 * abs(cos(input$map_center$lat * pi/180)))/input$scale) - 8
    output$scaleL <- renderText({ paste("Testing:", round(metesrPerPixel, 5), "m/pixel" ) })
    
    ## Get the meters for a given map
    metesrPerPixel = 40075016.686 * abs(cos(input$map_center$lat * pi/180)) / 2^(input$map_zoom+8)
    output$zoomL <- renderText({ paste("Testing:", round(zl, 5), "Zoom level" ) })
    
    rv$lat <- as.numeric(input$latitude)
    rv$lng <- as.numeric(input$longitude)

    #Render the new map
    shiny::isolate({
      leafletProxy("map") %>%
        setView(lng = rv$lng, lat = rv$lat, zoom = zl) %>% 
        leaflet(options = leafletOptions(minZoom = zl, maxZoom = zl)) %>% 
        leaflet.extras::activateGPS()
    })
  })
  
  
}
