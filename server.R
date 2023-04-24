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


getBox <- function(m){
  view <- m$x$setView
  lat <- view[[1]][1]
  lng <- view[[1]][2]
  zoom <- view[[2]]
  zoom_width <- 360 / 2^zoom
  lng_width <- m$width / 256 * zoom_width
  lat_height <- m$height / 256 * zoom_width
  return(c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2))
}


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
    coords <- getBox(recMap)
    
    print(coords)
    print(input$map_bounds)
    
    lng1= coords[2] #east
    lng2= coords[1] #west
    lat2= coords[3] #south
    lat1= coords[4] #north
    
    # Render the new map with updated view and rectangle coordinates
   leafletProxy("map") %>%
      #setView(lng = rv$lng, lat = rv$lat, zoom = zl) %>% 
      clearShapes() %>%
      addRectangles(
        lng1=lng1, lat1=lat1,
        lng2=lng2, lat2=lat2,
        fillColor = "transparent") 
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
      coords <- getBox(recMap)
      
      print(coords)
      print(input$map_bounds)
      
      lng1= coords[2] #east
      lng2= coords[1] #west
      lat2= coords[3] #south
      lat1= coords[4] #north
      
      # Update the rectangle coordinates
      leafletProxy("map") %>%
        clearShapes() %>%
        addRectangles(
          lng1=lng1, lat1=lat1,
          lng2=lng2, lat2=lat2,
          fillColor = "transparent")
    }
    
  })


  


  

  
}