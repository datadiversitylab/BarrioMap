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
library(osmdata)
library(ggplot2)
library(ggmap)
library(sp)
library(waiter)

source('functions/functions.R')


server <- function(input, output, session) {
  
  w <- Waiter$new(
    id = "map",
    html = spin_3(), 
    color = transparent(.5)
  )
  
  # Reactive values
  rv <- reactiveValues(
    latitude = 32.2540,
    longitude = -110.9742,
    pageH = 0.267,
    pageW = 0.18,
    vpages = 1,
    hpages = 1,
    scale = 5840,
    page = "a4",
    usecoordinates = TRUE
  )
  
  # Update reactive values when UI elements are modified
  observeEvent(input$latitude, { rv$latitude <- input$latitude })
  observeEvent(input$longitude, { rv$longitude <- input$longitude })
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
      rv$pageH <- 0.267 
      rv$pageW <- 0.18
      }else{
        rv$pageW <- 0.267 
        rv$pageH <- 0.18
      }
      
    } else if (input$page == "a3") {
      
      if (input$orientation == "v"){
      rv$pageH <- 0.420
      rv$pageW <- 0.297
      }else{
        rv$pageW <- 0.420
        rv$pageH <- 0.297
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
    zl <- log2(591657550.5 / as.numeric(rv$scale))
    mbox_scale = as.numeric(rv$scale)
    pixel_v = meter2screenpixel(rv$pageH * mbox_scale, orient= "v", zl, rv$latitude)
    pixel_h = meter2screenpixel(rv$pageW * mbox_scale, orient= "h", zl, rv$latitude)
    
    cat(pixel_v/pixel_h)
    
    #Make a map for the rectangle
    recMap <- leaflet(width = pixel_h, height = pixel_v) %>%
      addTiles() %>%
      setView(lng = rv$longitude, lat = rv$latitude, zoom = zl) 
    
    # Estimate the bounding box
    rects <<- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)
    
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
      zl <- log2(591657550.5 / as.numeric(rv$scale))
      mbox_scale = as.numeric(rv$scale)
      pixel_v = meter2screenpixel(rv$pageH * mbox_scale, orient= "v", zl, rv$latitude)
      pixel_h = meter2screenpixel(rv$pageW * mbox_scale, orient= "h", zl, rv$latitude)
      
      cat(pixel_v/pixel_h)
      
      #Make a map for the rectangle
      recMap <- leaflet(width = pixel_h, height = pixel_v) %>%
        addTiles() %>%
        setView(lng = rv$longitude, lat = rv$latitude, zoom = zl) 
      
      # Estimate the bounding box
      rects <<- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)

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
  

    output$print <- downloadHandler(
      filename = "barrio.pdf",
      content = function(file) {
        rects2 <- cbind(
          rects[,1],
          rects[,3],
          rects[,2],
          rects[,4]
        )
        w$show()
        osmdata_plot(rects2, prefix = "barrio")
        w$hide()
        file.copy("www/barrio.pdf", file)
      }
    )

}
