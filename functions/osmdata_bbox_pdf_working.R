library(osmdata)
library(ggplot2)
library(ggmap)
library(sp)

bbox1 = c (-110, 32.1, -111, 32.2)
bbox2 = c (-110, 32.1, -111, 32.4)

bbox_list <- list(bbox1, bbox2)

get_and_plot <- function(bbox_list){
  
  for (i in 1:length(bbox_list)) {
    
    q1 <- opq(bbox = bbox_list[[i]]) %>%
      add_osm_feature(key = 'highway', value = 'cycleway')
    cway_sev <- osmdata_sp(q1)
    
    # Step 1: Call the pdf command to start the plot
    pdf(file = paste0("C:/Users/ASUS/OneDrive/Desktop/Plotty_",i, ".pdf"),   # The directory you want to save the file in
        width = 4, # The width of the plot in inches
        height = 4) # The height of the plot in inches
    
    # Step 2: Create the plot with R code
    sp::plot(cway_sev$osm_lines)
    
    # Step 3: Run dev.off() to create the file!
    dev.off()
    
  
  }
}
  
get_and_plot(bbox_list)
