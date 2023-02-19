#Load the relevant libraries
library(shiny)
library(leaflet)

#Load components
source('ui.R')
source('server.R')

#Run the app
shinyApp(ui, server)

