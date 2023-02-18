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
      textInput("latitude", "Latitude"),
      textInput("longitude", "Longitude"),
      
      #Removed default lat long values
      
  
      
      # Scale selection box
      numericInput("scale", "m/px", 10, min = 1, max = 160000),
      
      #Some text in the app for testing
      textOutput("scaleL"),
      textOutput("zoomL"),
      
      #button
      actionButton("locate", "Locate"),
      

    ),
    mainPanel(
      leafletOutput("map", height = "500px", width = "100%")
    )
  )
)

