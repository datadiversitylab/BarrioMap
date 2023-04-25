###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

library(dplyr)
library(leaflet)
library(shiny)
library(leaflet.extras)
library(shinyjs)

source('functions/functions.R')


server <- function(input, output, session) {

  
  # Reactive values
  rv <- reactiveValues(
    latitude = 32.2540,
    longitude = -110.9742,
    dpi = 150,
    pageH = 3508,
    pageW = 2480,
    vpages = 1,
    hpages = 1,
    scale = 5840,
    page = "a4",
    usecoordinates = TRUE
  )
  
  # Update reactive values when UI elements are modified
  observeEvent(input$latitude, { rv$latitude <- input$latitude })
  observeEvent(input$longitude, { rv$longitude <- input$longitude })
  observeEvent(input$dpi, { rv$dpi <- input$dpi })
  observeEvent(input$pageH, { rv$pageH <- input$pageH })
  observeEvent(input$pageW, { rv$pageW <- input$pageW })
  observeEvent(input$vpages, { rv$vpages <- input$vpages })
  observeEvent(input$hpages, { rv$hpages <- input$hpages })
  observeEvent(input$page, { rv$page <- input$page })
  observeEvent(input$orientation, { rv$orientation <- input$orientation })
  observeEvent(input$scale, { rv$scale <- input$scale })
  observeEvent(input$usecoordinates, { rv$usecoordinates <- input$usecoordinates })
  
  #Render the initial map
  output$map <- leaflet::renderLeaflet({
    if(input$usecoordinates){
      leaflet(options = leafletOptions(zoomControl = TRUE, 
                                       #zoomSnap = 0.1,
                                       crs = leafletCRS(
                                         scales = 1
                                       ),
                                       attributionControl = FALSE
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
      leaflet(options = leafletOptions(zoomControl = TRUE, 
                                       #zoomSnap = 0.1,
                                       crs = leafletCRS(
                                         scales = 1
                                       ),
                                       attributionControl = FALSE
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
      shinyjs::hide("longitude")
      shinyjs::hide("latitude")
    }else{
      shinyjs::show("longitude")
      shinyjs::show("latitude")
    }
  })
  
  observeEvent(rv$page,{
    if(rv$page != "other"){
      shinyjs::hide("pageH")
      shinyjs::hide("pageW")
      shinyjs::show("orientation")

    }else{
      shinyjs::show("pageH")
      shinyjs::show("pageW")
      shinyjs::hide("orientation")
      
    }
  })
  
  observeEvent(list(input$page,
                    input$orientation), {
    if (input$page == "a4") {
      if (input$orientation == "v"){
      rv$pageH <- 3508 
      rv$pageW <- 2480
      }else{
        rv$pageW <- 3508 
        rv$pageH <- 2480
      }
      
    } else if (input$page == "a3") {
      
      if (input$orientation == "v"){
      rv$pageH <- 3508
      rv$pageW <- 4961
      }else{
        rv$pageW <- 3508
        rv$pageH <- 4961
      }
      
      
    } else {
      rv$pageH <- input$pageH
      rv$pageW <- input$pageW
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
  
  
  observe({
    
    ## Estimate the zoom level for a given scale
    zl <- log2(rv$dpi * 1/0.0254 * 156543.03 * cos(rv$latitude) / as.numeric(rv$scale) )
  
    #Make a map for the rectangle
    recMap <- leaflet(width = rv$pageW, height = rv$pageH) %>%
      addTiles() %>%
      setView(lng = rv$longitude, lat = rv$latitude, zoom = zl) 
    
    # Estimate the bounding box
    rects <- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)
    
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
    
      ## Estimate the zoom level for a given scale
      zl <- log2(rv$dpi * 1/0.0254 * 156543.03 * cos(rv$latitude) / as.numeric(rv$scale) )
      
      #Make a map for the rectangle
      recMap <- leaflet(width = rv$pageW, height = rv$pageH) %>%
        addTiles() %>%
        setView(lng = rv$longitude, lat = rv$latitude, zoom = zl) 
      
      # Estimate the bounding box
      rects <- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)

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
  })
}
