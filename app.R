#Load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(shinyjs)
library(osmdata)
library(osmextract)
library(ggplot2)
library(ggmap)
library(sp)
library(ggspatial)
library(sf)
library(tmaptools)
library(shinybusy)
library(jsonlite)

# Load shared functions and constants before ui.R so color palettes
# (ROAD_COLORS etc.) are available when ui.R is evaluated.
source('functions/functions.R')
source('ui.R')
source('server.R')

# Remove code files older than 30 days.
cleanExpiredCodes()

# Pre-download the Arizona extract before the first user connects.
# Any region a user exports later is saved to the same cache directory,
# so it becomes pre-loaded from the next restart onward.
preloadOsmRegions(c("Arizona"))

#Run the app
shinyApp(ui, server)
