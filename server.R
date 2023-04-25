###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

library(dplyr)
library(leaflet)
library(shiny)
library(leaflet.extras2)
library(leaflet.extras)
library(shinyjs)
library(htmlwidgets)

source('functions/functions.R')


server <- function(input, output, session) {

  observeEvent(input$page, {
    # page information
    if(input$page == "a4"){
      width_map  <<- 2480
      height_map <<- 3508
    }
    
    if(input$page == "a3"){
      width_map  <<- 4961
      height_map <<- 3508
    }
    
    width_map_or <<- width_map
    height_map_or <<- height_map
  })
  
  observeEvent(input$orientation, {
    
    if(input$orientation == "v"){
      width_map  <<- width_map_or
      height_map <<- height_map_or
    }
    if(input$orientation == "h"){
      width_map  <<- height_map_or
      height_map <<- width_map_or
    }
    
  })
  
  # Reactive values for latitude and longitude
  rv <- reactiveValues(latitude = 0 , longitude = 0)
  
  #Render the initial map
  output$map <- leaflet::renderLeaflet({
    if(input$usecoordinates){
      leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       zoomSnap = 0.1,
                                       crs = leafletCRS(
                                         scales = 1
                                       ),
                                       attributionControl=FALSE
      )) %>% 
        addTiles() %>%
        #addProviderTiles(providers$Stamen.TonerLines) %>%
        addScaleBar(position = 'bottomleft') %>%
        setView(lng = -110.9742, lat = 32.2540, zoom = 10) %>%
        addControlGPS(
          options = gpsOptions(
            position = "topright",
            activate = TRUE, 
            autoCenter = TRUE,
            setView = TRUE))
    }else{
      leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       zoomSnap = 0.1,
                                       crs = leafletCRS(
                                         scales = 1
                                       ),
                                       attributionControl=FALSE
      )) %>% 
        addTiles() %>%
        #addProviderTiles(providers$Stamen.TonerLines) %>%
        addScaleBar(position = 'bottomleft') %>%
        setView(lng = -110.9742, lat = 32.2540, zoom = 10) %>%
        addControlGPS(
          options = gpsOptions(
            position = "topright",
            activate = TRUE, 
            autoCenter = TRUE,
            setView = TRUE))%>% 
        addSearchOSM(options = searchOptions(autoCollapse = FALSE, minLength = 2))
    }
  }) 
  
  observeEvent(input$usecoordinates,{
    if(input$usecoordinates == FALSE){
      shinyjs::disable("longitude")
      shinyjs::disable("latitude")
      
    }else{
      shinyjs::enable("longitude")
      shinyjs::enable("latitude")
    }
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
    
    ## Estimate the zoom level for a given scale
    zl <- log2(input$dpi * 1/0.0254 * 156543.03 * cos(input$map_center$lat) / as.numeric(input$scale) )
    
    #Make a map for the rectangle
    recMap <- leaflet(width = width_map, height = height_map) %>%
      addTiles() %>%
      setView(lng = input$map_center$lng, lat = input$map_center$lat, zoom = zl) 
    
    # Estimate the bounding box
    rects <- returnRectangles(map = recMap, nRecLon = input$hpages, nRecVert = input$vpages)
    
    # Render the new map with updated view and rectangle coordinates
    for(i in 1:nrow(rects)){
    leafletProxy("map") %>%
      clearShapes() %>%
      addRectangles(
        lng1=rects[i,1], lat1=rects[i,3],
        lng2=rects[i,2], lat2=rects[i,4],
        fillColor = "transparent")
    }
    
  })
  
  # Update rectangle coordinates when the map view changes
  observe({
    
    if (!is.null(input$map_bounds)) {
      ## Estimate the zoom level for a given scale
      zl <- log2(input$dpi * 1/0.0254 * 156543.03 * cos(input$map_center$lat) / as.numeric(input$scale) )
      
      #Make a map for the rectangle
      recMap <- leaflet(width = width_map, height = height_map) %>%
        addTiles() %>%
        setView(lng = input$map_center$lng, lat = input$map_center$lat, zoom = zl)
      
      # Estimate the bounding box
      rects <- returnRectangles(map = recMap, nRecLon = input$hpages, nRecVert = input$vpages)
      print(rects)
      
      # Update the rectangle coordinates
      proxy <- leafletProxy("map") %>%
        clearShapes()
      for(i in 1:nrow(rects)){
        proxy %>%
        addRectangles(
          lng1=rects[i,1], lat1=rects[i,3],
          lng2=rects[i,2], lat2=rects[i,4],
          fillColor = "transparent")
      }

    }
    
  })
  
}
