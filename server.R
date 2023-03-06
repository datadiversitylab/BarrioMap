###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(mapview)

server <- function(input, output, session) {
  
  # Reactive values for latitude and longitude
  rv <- reactiveValues(latitude = 0 , longitude = 0)
  
  #Render the initial map
  output$map <- leaflet::renderLeaflet({
    if(input$usecoordinates){
    leaflet(options = leafletOptions(zoomControl = FALSE, 
                                     zoomSnap = 0.001,
                                     crs = leafletCRS(
                                       scales = 1
                                     ),
                                     attributionControl=FALSE
    )) %>% 
      addTiles() %>%
      #addProviderTiles(providers$Stamen.TonerLines) %>%
      addScaleBar(position = 'bottomleft') %>%
      setView(lng = -110.9742, lat = 32.2540, zoom = 5) %>%
      addControlGPS(
        options = gpsOptions(
          position = "topright",
          activate = TRUE, 
          autoCenter = TRUE,
          setView = TRUE))
    }else{
      leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       zoomSnap = 0.001,
                                       crs = leafletCRS(
                                         scales = 1
                                       ),
                                       attributionControl=FALSE
      )) %>% 
        addTiles() %>%
        #addProviderTiles(providers$Stamen.TonerLines) %>%
        addScaleBar(position = 'bottomleft') %>%
        setView(lng = -110.9742, lat = 32.2540, zoom = 5) %>%
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
    zl = log2(input$dpi * 1/0.0254 * 156543.03 * cos(input$map_center$lat) / as.numeric(input$scale) )
    
    ## Get the meters for a given map 
    levelofZoomEst = log2(( 40075016.686 * abs(cos(input$map_center$lat * pi/180)))/zl) - 8
    #output$zoomEL <- renderText({ paste("Zoom level (estimated) = ", round(levelofZoomEst, 5)) })
    output$zoomL <- renderText({ paste("Zoom level = ", round(zl, 5)) })
    
    ## Get the resolution
    resolution = 156543.03 * cos(input$map_center$lat) / (2 ^ input$map_zoom)
    output$resolutionL <- renderText({ paste("Resolution = ", round(resolution, 5), "m/pixel" ) })
    
    ## Get the scale
    scale = (input$dpi * 1/0.0254* resolution) * cos(input$map_center$lat)
    output$scaleL <- renderText({ paste0("Scale = 1:", round(scale, 3) ) })
    output$scaleL <- renderText({ paste0("1 screen cm is ", round(scale/10, 3), " m" ) })
    
    
    rv$lat <- as.numeric(input$latitude)
    rv$lng <- as.numeric(input$longitude)

    #Render the new map
#    shiny::isolate({
#      leafletProxy("map") %>%
#        setView(lng = rv$lng, lat = rv$lat, zoom = zl) %>% 
#        leaflet(options = leafletOptions(minZoom = zl, maxZoom = zl)) %>% 
#        leaflet.extras::activateGPS()
#    })
    
    
    # Render a new map and store it in the reactive value
    rv$map <- leaflet(options = leafletOptions(minZoom = zl, maxZoom = zl, attributionControl=FALSE)) %>%
      addTiles() %>%
      addProviderTiles("OpenStreetMap") %>%
      addScaleBar(position = 'bottomleft') %>%
      setView(lng = rv$lng, lat = rv$lat, zoom = zl)
    
    # Update the output with the new map
    output$map <- renderLeaflet({rv$map})
  
    

    # create download pdf
    
    
    
    output$dl <- downloadHandler(
      filename = "map.pdf",
      
      content = function(file) {
        #        pageSize <- input$pagesize
        #        pageDims <- getPageSize(pageSize)
        mapshot( rv$map, 
                 file = file,                
                 
                 #                vwidth = pageDims[1], 
                 #                vheight = pageDims[2],
                 vwidth = input$dimension[1], 
                 vheight = input$dimension[2]
                 #               cliprect = "viewport"
                 
        )
        
        
      }
    )
    
    
    
    
  })
  
  # define function to get page size dimensions
  getPageSize <- function(pageSize) {
    if (pageSize == "A4") {
      return(c(width = 2480, height = 3508))
    } else {
      return(c(width = 2550, height = 3300))
    }
  }
  
  
  # create download png
  output$dl2 <- downloadHandler(
    filename = "map.png",
    
    content = function(file) {
      pageSize <- input$pagesize
      pageDims <- getPageSize(pageSize)
      mapshot( rv$map,
               file = file)})
  
  # create download html
  output$dl3 <- downloadHandler(
    filename = "map.html",
    
    content = function(file) {
      pageSize <- input$pagesize
      pageDims <- getPageSize(pageSize)
 
      
      htmlwidgets::saveWidget(rv$map, file=file)})
  
  
  
  
}
