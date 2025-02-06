#Load the relevant libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(shinyjs)
library(osmdata)
library(ggplot2)
library(ggmap)
library(sp)
library(ggspatial)
library(sf)
library(tmaptools)
library(shinybusy)

#Load components
source('ui.R')
source('server.R')

#Run the app
shinyApp(ui, server)

