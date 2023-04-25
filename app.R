#Load the relevant libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(bslib)
library(dplyr)
library(shinyjs)


#Load components
source('ui.R')
source('server.R')

#Run the app
shinyApp(ui, server)

