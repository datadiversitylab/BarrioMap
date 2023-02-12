library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("mapper"),
  sidebarLayout(
    sidebarPanel(
      #input lat lng
      textInput("latitude", "Latitude"),
      textInput("longitude", "Longitude"),
      # Scale selection box
      selectInput("scale", "Select a scale:", 
                  c('1/8” = 1’0”' = 1/8,
                    '1/16” = 1’0”' = 1/16,
                    '1” = 10’' = 1,
                    '1” = 20’' = 2,
                    '1” = 30’' = 3,
                    '1” = 100’' = 100)
      )
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles()  %>% 
      setView(lng = as.numeric(input$longitude) , lat = as.numeric(input$latitude) , zoom = input$scale) %>% 
      addProviderTiles("OpenStreetMap") %>% 
      addScaleBar(position = 'bottomleft') %>%
      addLayersControl(overlayGroups = c(), options = layersControlOptions(collapsed = FALSE))
  })
  
  observe({
    lat <- as.numeric(input$latitude)
    lng <- as.numeric(input$longitude)
    if (is.na(lat) || is.na(lng)) return()
    leafletProxy("map") %>%
      setView(lng = lng , lat = lat, zoom = input$scale)
  })
  
}

shinyApp(ui, server)

