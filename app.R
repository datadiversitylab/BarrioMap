library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("mapper"),
  sidebarLayout(
    sidebarPanel(
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
    leaflet() %>% addTiles()  %>% 
      setView(lng = -110.9742 , lat = 32.2540, zoom = input$scale) %>% 
      addProviderTiles("OpenStreetMap") %>% 
      addScaleBar(position = 'bottomleft')
  })
  
}

shinyApp(ui, server)