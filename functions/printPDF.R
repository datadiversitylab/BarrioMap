library(osmdata)
library(ggplot2)
library(ggmap)
library(sp)

source('functions/functions.R')

recs <- rbind(c(-110, 32.1,-111, 32.2), 
              c(-110, 32.1,-111, 32.4)
              )
osmdata_plot(recs)
