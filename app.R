#Load the relevant libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(shinyjs)
library(htmlwidgets)

library(mapview)
library(htmlwidgets)
webshot::install_phantomjs(force=TRUE)


#Load components
source('ui.R')
source('server.R')

#Run the app
shinyApp(ui, server)

