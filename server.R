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


server <- function(input, output, session) {

  # update the map width dynamically
  output$mapjs <- renderUI({
    tags$script(HTML(paste0(
      'document.getElementById("map").style.width="', input$mapWidth, '%";'
    )))
  })
  


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
            setView = TRUE))%>%
      addEasyprint(options = easyprintOptions(
        #dpi = input$dpi,
        title = 'Give me that map',
        position = 'bottomleft',
        exportOnly = TRUE,
        # hideClasses = list("leaflet-overlay-pane", "leaflet-popup"),
         hidden = TRUE, hideControlContainer = TRUE,
        filename = "mapit",
        tileLayer = "basemap",
        tileWait = 5000,
        defaultSizeTitles = list(
          "CurrentSize" = "The current map extent",
          "A4Landscape" = "A4 (Landscape) extent with w:1045, h:715",
          "A4Portrait" = "A4  (Portrait) extent with w:715, h:1045"
        ),
        # sizeModes = c("A4Portrait","A4Landscape"),
        sizeModes = list("CurrentSize" = "CurrentSize",
                         "A4Landscape" = "A4Landscape",
                         "A4Portrait" = "A4Portrait",
                         "Custom Landscape"=list(
                           width= 1800,
                           height= 700,
                           name = "A custom landscape size tooltip",
                           className= 'customCssClass'),
                         "Custom Portrait"=list(
                           width= 700,
                           height= 1800,
                           name = "A custom portrait size tooltip",
                           className= 'customCssClass1')
        ),
        customWindowTitle = "Some Fancy Title",
        customSpinnerClass = "shiny-spinner-placeholder",
        spinnerBgColor = "#b48484"
      )) 
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
        addSearchOSM(options = searchOptions(autoCollapse = FALSE, minLength = 2))%>%
      addEasyprint(options = easyprintOptions(
        #dpi = input$dpi,
        title = 'Give me that map',
        position = 'bottomleft',
        exportOnly = TRUE,
        # hideClasses = list("leaflet-overlay-pane", "leaflet-popup"),
         hidden = TRUE, hideControlContainer = TRUE,
        filename = "mapit",
        tileLayer = "basemap",
        tileWait = 5000,
        defaultSizeTitles = list(
          "CurrentSize" = "The current map extent",
          "A4Landscape" = "A4 (Landscape) extent with w:1045, h:715",
          "A4Portrait" = "A4  (Portrait) extent with w:715, h:1045"
        ),
        customWindowTitle = "Some Fancy Title",
        customSpinnerClass = "shiny-spinner-placeholder",
        spinnerBgColor = "#b48484"
      ))
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
    
    # Estimate the bounding box
    lng1=input$map_bounds[[2]] #east
    lat1=input$map_bounds[[1]] #north
    lng2=input$map_bounds[[4]] #west
    lat2=input$map_bounds[[3]] #south
    # Render the new map with updated view and rectangle coordinates
   leafletProxy("map", session) %>%
      setView(lng = rv$lng, lat = rv$lat, zoom = zl) 
})

# DPI support: https://github.com/trafficonese/leaflet.extras2/blob/print_dpi/R/easyprint.R

    observeEvent(input$print, {
      leafletProxy("map", session) %>%
        easyprintMap(sizeModes = input$scene, filename = paste0("BarrioMap_scale_1_", input$scale), dpi = input$dpi)
    })  

}