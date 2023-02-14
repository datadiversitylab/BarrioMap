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
      selectInput("scale", "Select a scale:", 
                  c('1/8” = 1’0”' = 8,
                    '1/16” = 1’0”' = 16,
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

