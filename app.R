#Load the relevant libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(mapview)
library(webshot)
library(shinyjs)


#Load components
source('ui.R')
source('server.R')

#Run the app
shinyApp(ui, server)

