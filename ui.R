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
      textInput("latitude", "Latitude", value = 32.2540),
      textInput("longitude", "Longitude", value = -110.9742),
      
      # Scale selection box
      numericInput("scale", "m/px", 10, min = 1, max = 160000),
      
      #Some text in the app for testing
      textOutput("scaleL"),
      textOutput("zoomL")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

