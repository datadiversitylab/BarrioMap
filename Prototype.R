library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("mapper"),
  sidebarLayout(
    sidebarPanel(
      #input lat lng
      textInput("latitude", "Latitude", value = 32.2540),
      textInput("longitude", "Longitude", value = -110.9742),
      # Scale selection box
      selectInput("scale", "Select a scale:", 
                  c('1/8” = 1’0”' = 1/8,
                    '1/16” = 1’0”' = 1/16,
                    '1” = 10’' = 1,
                    '1” = 20’' = 2,
                    '1” = 30’' = 3,
                    '1” = 100’' = 100,
                    'Custom' = 'custom')
      ),
      #custom text
      conditionalPanel(
        condition = "input.scale == 'custom'",
        textInput("custom_scale", "Enter a custom scale", value=3)
      )
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

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

shinyApp(ui, server)

