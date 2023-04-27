library(osmdata)

q1 <- opq(bbox = c (-110, 32.1, -111, 32.2)) %>%
  add_osm_feature(key = 'highway'
                  , value = 'cycleway'
                  ) 
cway_sev <- osmdata_sp(q1)
sp::plot(cway_sev$osm_lines)



shapefiles <- opq(bbox = getbb("Limete, Kinshasa")) %>%
  add_osm_feature(key = "highway", value = "cycleway") %>%
  osmdata_sf()

mad_map <- get_map(getbb("Limete, Kinshasa"), maptype = "roadmap", source = "osm", scale = 2)
testMap <- ggmap(mad_map) +
  geom_sf(
    data = shapefiles$osm_lines,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(x = "", y = "")

ggsave(testMap, filename =  "test.pdf")


## Sarthak's final update

library(osmdata)
library(ggplot2)
library(ggmap)
library(sp)
library(maptools)

bbox1 = c (-110.9, 32.15, -111, 32.2)
bbox2 = c (-110.9, 32.15, -111, 32.4)

bbox_list <- list(bbox1, bbox2)

get_and_plot <- function(bbox_list){
  
  for (i in 1:length(bbox_list)) {
    
    q1 <- opq(bbox = bbox_list[[i]]) %>%
      add_osm_feature(key = 'highway', value = c("highway", "cycleway", "footway"))
    
    q2 <- opq(bbox = bbox1) %>%
      add_osm_feature(key = "waterway", value = "river")
    
    
    q3 <- opq(bbox = bbox1) %>%
      add_osm_feature(key = "building", value = "yes")
    
    
    
    
    cway_sev <- osmdata_sp(q1)
    wway_sev <- osmdata_sp(q2)
    bldg_sev <- osmdata_sp(q3)
    
    # Step 1: Call the pdf command to start the plot
    pdf(file = paste0("C:/Users/ASUS/OneDrive/Desktop/Plotty_",i, ".pdf"))
    #,   # The directory you want to save the file in
    #    width = 4, # The width of the plot in inches
    #    height = 4) # The height of the plot in inches
    
    # Step 2: Create the plot with R code
    plot(cway_sev$osm_lines,col="#964B00", axes = TRUE)
    plot(cway_sev$osm_polygons,col="#964B00", add=TRUE)
    plot(wway_sev$osm_lines, col="blue", add=TRUE)
    plot(bldg_sev$osm_polygons, col = "red", add = TRUE)
    plot(gridlines(cway_sev$osm_lines), add = TRUE, col = grey(.9))
    
    
    
    # Step 3: Run dev.off() to create the file!
    dev.off()
    
  
  }
}
  
get_and_plot(bbox_list)
