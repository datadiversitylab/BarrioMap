###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

ui <- fluidPage(
  titlePanel("mapper"),
  sidebarLayout(
    sidebarPanel(
      #input lat lng
      numericInput("latitude", "Latitude", value = 32.2540),
      numericInput("longitude", "Longitude", value = -110.9742),
      
      # Scale selection box
      numericInput("scale", "m/px", 10, min = 1, max = 160000),
      
      #Some text in the app for testing
      textOutput("scaleL"),
      textOutput("zoomL")
    ),
    mainPanel(
      leafletOutput("map", height = "500px", width = "100%")
    )
  )
)

