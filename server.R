###################
# server.R
# 
# Server controller. 
# Updated to correctly compute the map zoom level based on the user-defined scale,
# latitude, and a chosen (or default) DPI.
###################
library(dplyr)
library(leaflet)
library(ggplot2)

source('functions/functions.R')

server <- function(input, output, session) {
  

  # Reactive values
  rv <- reactiveValues(
    latitude = 32.2540,
    longitude = -110.9742,
    pageH = 0.267,
    pageW = 0.18,
    vpages = 1,
    hpages = 1,
    scale = 5840,
    page = "a4",
    usecoordinates = TRUE,
    dpi = 300
  )
  # Update reactive values when UI elements are modified
  observeEvent(input$latitude,       { rv$latitude <- input$latitude })
  observeEvent(input$longitude,      { rv$longitude <- input$longitude })
  observeEvent(input$pageH,          { rv$pageH <- input$pageH })
  observeEvent(input$pageW,          { rv$pageW <- input$pageW })
  observeEvent(input$vpages,         { rv$vpages <- input$vpages })
  observeEvent(input$hpages,         { rv$hpages <- input$hpages })
  observeEvent(input$page,           { rv$page <- input$page })
  observeEvent(input$orientation,    { rv$orientation <- input$orientation })
  observeEvent(input$scale,          { rv$scale <- input$scale })
  observeEvent(input$usecoordinates, { rv$usecoordinates <- input$usecoordinates })
  observeEvent(input$dpi,            { rv$dpi <- input$dpi })
  
  # Render the initial map
  output$map <- leaflet::renderLeaflet({
    if(input$usecoordinates){
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        crs = leafletCRS(scales = 1),
        attributionControl = FALSE
      )) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%
        addTiles() %>%
        addScaleBar(position = 'bottomleft') %>%
        setView(lng = -110.9742, lat = 32.2540, zoom = 10)
    } else {
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        crs = leafletCRS(scales = 1),
        attributionControl = FALSE
      )) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%
        addTiles() %>%
        addScaleBar(position = 'bottomleft') %>%
        setView(lng = -110.9742, lat = 32.2540, zoom = 10)
    }
  }) 
  
  # Hide/Show latitude & longitude inputs based on "usecoordinates"
  observeEvent(input$usecoordinates, {
    if (input$usecoordinates == FALSE) {
      shinyjs::hide("longitude")
      shinyjs::hide("latitude")
    } else {
      shinyjs::show("longitude")
      shinyjs::show("latitude")
    }
  })
  
  observeEvent(input$searchbox, {
    req(!input$usecoordinates)   # only if user is using the search approach
    req(nzchar(input$searchbox)) # searchbox not empty
    
    # geocode_OSM returns lat, lon
    coords <- tmaptools::geocode_OSM(q = input$searchbox)
    
    # If we got at least one geocoded result:
    if (length(coords) > 0) {
      # Take the first match (or any row you prefer)
      lon <- as.numeric(coords[[2]][1])
      lat <- as.numeric(coords[[2]][2])

      # Center the leaflet map on that result:
      leafletProxy("map") %>%
        setView(lng = lon, lat = lat, zoom = 10)
    } else {
      showNotification("Could not find location. Try a different search term.")
    }
  })
  
  # Hide/Show page size inputs based on user selection
  observeEvent(rv$page, {
    if (rv$page != "other") {
      shinyjs::hide("pageH")
      shinyjs::hide("pageW")
      shinyjs::show("orientation")
    } else {
      shinyjs::show("pageH")
      shinyjs::show("pageW")
      shinyjs::hide("orientation")
    }
  })
  
  # Dynamically set default page size for A4 / A3
  observeEvent(list(input$page, input$orientation), {
    if (input$page == "a4") {
      if (input$orientation == "v") {
        rv$pageH <- 0.267 
        rv$pageW <- 0.18
      } else {
        rv$pageW <- 0.267 
        rv$pageH <- 0.18
      }
      
    } else if (input$page == "a3") {
      if (input$orientation == "v") {
        rv$pageH <- 0.420
        rv$pageW <- 0.297
      } else {
        rv$pageW <- 0.420
        rv$pageH <- 0.297
      }
    } else {
      rv$pageH <- input$pageH
      rv$pageW <- input$pageW
    }
  })
  
  # Update latitude/longitude inputs when the user drags/zooms the map
  observeEvent(input$map_center, {
    # This reactive event triggers when the user drags/zooms the map
    # But we only update the numeric inputs if fixframe is NOT checked
    req(!input$fixframe)
    
    updateNumericInput(
      session = session,
      inputId = "longitude",
      value = input$map_center$lng
    )
    updateNumericInput(
      session = session,
      inputId = "latitude",
      value = input$map_center$lat
    )
  })
  
  # First observer: draws rectangle(s) on the map
  observe({
    # Calculate the Leaflet zoom level from user scale, latitude, and DPI
    zl <- calcZoom(
      scale_meters_per_inch = as.numeric(rv$scale),
      lat = rv$latitude,
      dpi = as.numeric(rv$dpi)  # or let your user input this
    )
    
    # Convert the user page size (m) into pixel dimensions for the bounding rectangle
    mbox_scale <- as.numeric(rv$scale)
    pixel_v <- meter2screenpixel(rv$pageH * mbox_scale, orient = "v", zl, rv$latitude)
    pixel_h <- meter2screenpixel(rv$pageW * mbox_scale, orient = "h", zl, rv$latitude)
    
    # Debug
    cat("pixel ratio v/h:", pixel_v / pixel_h, "\n")
    
    # Build a small "offscreen" leaflet map to compute bounding boxes
    recMap <- leaflet(width = pixel_h, height = pixel_v) %>%
      addTiles() %>%
      setView(lng = rv$longitude, lat = rv$latitude, zoom = zl)
    
    # Use your custom returnRectangles function to compute bounding coords
    rects <<- returnRectangles(
      map = recMap,
      nRecLon = rv$hpages,
      nRecVert = rv$vpages
    )
    
    # Add these rectangle(s) to the main "map"
    leafletProxy("map") %>%
      clearShapes() %>%
      {
        for (i in 1:nrow(rects)) {
          addRectangles(
            .,
            lng1 = rects[i, 1], lat1 = rects[i, 3],
            lng2 = rects[i, 2], lat2 = rects[i, 4],
            fillColor = "transparent"
          )
        }
      }
  })
  
  # Second observer: update rectangles if the map view changes
  observe({
    zl <- calcZoom(
      scale_meters_per_inch = as.numeric(rv$scale),
      lat = rv$latitude,
      dpi = 300
    )
    
    mbox_scale <- as.numeric(rv$scale)
    pixel_v <- meter2screenpixel(rv$pageH * mbox_scale, orient = "v", zl, rv$latitude)
    pixel_h <- meter2screenpixel(rv$pageW * mbox_scale, orient = "h", zl, rv$latitude)
    
    cat("pixel ratio v/h:", pixel_v / pixel_h, "\n")
    
    recMap <- leaflet(width = pixel_h, height = pixel_v) %>%
      addTiles() %>%
      setView(lng = rv$longitude, lat = rv$latitude, zoom = zl)
    
    rects <<- returnRectangles(
      map = recMap,
      nRecLon = rv$hpages,
      nRecVert = rv$vpages
    )
    
    proxy <- leafletProxy("map") %>% clearShapes()
    for (i in 1:nrow(rects)) {
      proxy %>%
        addRectangles(
          lng1 = rects[i, 1], lat1 = rects[i, 3],
          lng2 = rects[i, 2], lat2 = rects[i, 4],
          fillColor = "transparent"
        )
    }
  })
  
  # Download Handler - Exporting PDF with user-defined page size (in meters)
  
  output$print <- downloadHandler(
    filename = function() { "barrio.pdf" },
    content  = function(file) {

      # Convert user's page size from meters -> inches
      width_in  <- rv$pageW * 39.3701
      height_in <- rv$pageH * 39.3701
      
      #
      # CREATE INSTRUCTIONS PAGE (PAGE 1)
      #
      instr_lines <- c("Barrio PDF Instructions:")
      # row-major logic: row = floor((i-1)/rv$hpages) + 1
      #                  col = ((i-1) %% rv$hpages) + 1
      for (i in seq_len(nrow(rects))) {
        row_i <- floor((i - 1) / rv$hpages) + 1
        col_i <- ((i - 1) %% rv$hpages) + 1
        
        bb <- rects[i, ]
        instr_lines <- c(
          instr_lines,
          paste0(
            "Page ", i + 2,  # because Page 1= instructions, Page 2= overview
            " => Panel (", row_i, ", ", col_i, ")",
            " with bounding box (", 
            paste(round(bb, 5), collapse = ", "), 
            ")"
          )
        )
      }
      
      instructions_df <- data.frame(
        x = 0,
        y = seq(0, - (length(instr_lines) - 1)),
        label = instr_lines
      )
      
      instructionsPlot <- ggplot(instructions_df, aes(x, y, label = label)) +
        geom_text(hjust = 0, vjust = 1, size = 5, family = "sans") +
        xlim(0, 100) +
        ylim(-length(instr_lines), 1) +
        theme_void(base_size = 14) +
        ggtitle("Barrio PDF Instructions") +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.margin = margin(30, 30, 30, 30)
        )
      
      ggsave(
        filename = "instructions.pdf",
        plot     = instructionsPlot,
        device   = "pdf",
        width    = width_in,
        height   = height_in,
        units    = "in"
      )
      
      #
      # CREATE OVERVIEW PAGE (PAGE 2)
      #
      all_lng <- c(rects[,1], rects[,2])
      all_lat <- c(rects[,3], rects[,4])
      min_lng <- min(all_lng)
      max_lng <- max(all_lng)
      min_lat <- min(all_lat)
      max_lat <- max(all_lat)
      
      overview_bbox <- c(min_lng, min_lat, max_lng, max_lat)
      osmQuery_overview <- opq(bbox = overview_bbox)
      
      if ("roads" %in% input$features && !("buildings" %in% input$features)) {
        osmQuery_overview <- osmQuery_overview %>% add_osm_feature(key = "highway")
      }
      if ("buildings" %in% input$features && !("roads" %in% input$features)) {
        osmQuery_overview <- osmQuery_overview %>% add_osm_feature(key = "building")
      }
      if ("roads" %in% input$features && "buildings" %in% input$features) {
        osmQuery_overview <- osmQuery_overview %>% add_osm_features(features = list(
          "highway"  = c("motorway", "primary", "secondary", "tertiary", 
                         "residential", "unclassified", "service", "living_street", "footway"),
          "building" = c("yes", "house", "apartments", "commercial", "retail", 
                         "industrial", "church", "garage", "school", "hotel", 
                         "warehouse", "hospital", "stadium")
        ))
      }
      
      osm_ov <- osmQuery_overview %>% osmdata_sf()
      roads_ov     <- osm_ov$osm_lines
      buildings_ov <- osm_ov$osm_polygons
      
      all_panels_sf <- lapply(seq_len(nrow(rects)), function(i) {
        bb <- rects[i, ]
        st_polygon(list(matrix(c(
          bb[1], bb[3],
          bb[1], bb[4],
          bb[2], bb[4],
          bb[2], bb[3],
          bb[1], bb[3]
        ), ncol = 2, byrow = TRUE)))
      })
      overview_panels <- st_as_sf(st_sfc(all_panels_sf), crs = 4326)
      
      # Panel centers + labels with row-major numbering
      centers_list <- lapply(seq_len(nrow(rects)), function(i) {
        bb <- rects[i, ]
        cx <- (bb[1] + bb[2]) / 2
        cy <- (bb[3] + bb[4]) / 2
        st_point(c(cx, cy))
      })
      panelCenters_sfc <- st_sfc(centers_list, crs = 4326)
      
      # Build row/col label
      panelCenters_sf <- st_as_sf(
        data.frame(
          label = sapply(seq_len(nrow(rects)), function(i) {
            row_i <- floor((i - 1) / rv$hpages) + 1
            col_i <- ((i - 1) %% rv$hpages) + 1
            paste0("Panel (", row_i, ", ", col_i, ") => Pg ", i + 2)
          })
        ),
        geometry = panelCenters_sfc
      )
      
      overviewPlot <- ggplot() +
        geom_sf(data = roads_ov,     color = "darkgray", size = 0.5, alpha = 0.7) +
        geom_sf(data = buildings_ov, fill  = "gray90",   color = "gray40", size = 0.3, alpha = 0.8) +
        geom_sf(data = overview_panels, fill = NA, color = "red", size = 1) +
        geom_sf_text(data = panelCenters_sf, aes(label = label), size = 4, color = "blue") +
        
        annotation_scale(location = "bl", width_hint = 0.2) +
        annotation_north_arrow(location = "tl", which_north = "true",
                               style = north_arrow_fancy_orienteering()) +
        
        coord_sf(
          xlim = c(min_lng, max_lng),
          ylim = c(min_lat, max_lat),
          expand = FALSE
        ) +
        ggtitle("Overview Map (Page 2): All Panels Shown in Red") +
        theme_minimal(base_size = 14) +
        theme(
          legend.position   = "bottom",
          plot.title        = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.margin       = margin(10, 10, 10, 10),
          axis.title        = element_blank(),
          axis.text         = element_blank(),
          panel.grid.major  = element_blank(),
          panel.grid.minor  = element_blank()
        )
      
      ggsave(
        filename = "overview.pdf",
        plot     = overviewPlot,
        device   = "pdf",
        width    = width_in,
        height   = height_in,
        units    = "in"
      )
      
      #
      # CREATE EACH PANEL PAGE (page #3+)
      #
      panel_files <- character(0)
      for (i in seq_len(nrow(rects))) {
        bb <- c(
          rects[i, 1],
          rects[i, 3],
          rects[i, 2],
          rects[i, 4]
        )
        
        row_i <- floor((i - 1) / rv$hpages) + 1
        col_i <- ((i - 1) %% rv$hpages) + 1
        
        panelQuery <- opq(bbox = bb)
        if ("roads" %in% input$features && !("buildings" %in% input$features)) {
          panelQuery <- panelQuery %>% add_osm_feature(key = "highway")
        }
        if ("buildings" %in% input$features && !("roads" %in% input$features)) {
          panelQuery <- panelQuery %>% add_osm_feature(key = "building")
        }
        if ("roads" %in% input$features && "buildings" %in% input$features) {
          panelQuery <- panelQuery %>% add_osm_features(features = list(
            "highway"  = c("motorway", "primary", "secondary", "tertiary", 
                           "residential", "unclassified", "service", "living_street", "footway"),
            "building" = c("yes", "house", "apartments", "commercial", "retail", 
                           "industrial", "church", "garage", "school", "hotel", 
                           "warehouse", "hospital", "stadium")
          ))
        }
        
        osm_panel <- panelQuery %>% osmdata_sf()
        roads_sf     <- osm_panel$osm_lines
        buildings_sf <- osm_panel$osm_polygons
        
        panel_polygon <- st_as_sf(st_sfc(st_polygon(list(matrix(c(
          bb[1], bb[2],
          bb[1], bb[4],
          bb[3], bb[4],
          bb[3], bb[2],
          bb[1], bb[2]
        ), ncol = 2, byrow = TRUE)))), crs = 4326)
        
        panelPlot <- ggplot() +
          geom_sf(data = roads_sf,     color = "darkgray", size = 0.5, alpha = 0.7) +
          geom_sf(data = buildings_sf, fill = "gray90",    color = "gray40", size = 0.3, alpha = 0.8) +
          geom_sf(data = panel_polygon, fill = NA, color = "black", size = 1) +
          
          annotation_scale(location = "bl", width_hint = 0.2) +
          annotation_north_arrow(location = "tl", which_north = "true",
                                 style = north_arrow_fancy_orienteering()) +
          
          coord_sf(
            xlim = c(bb[1], bb[3]),
            ylim = c(bb[2], bb[4]),
            expand = FALSE
          ) +
          ggtitle(paste0("Panel (", row_i, ", ", col_i, ") => Page ", i + 2)) +
          theme_minimal(base_size = 14) +
          theme(
            legend.position   = "bottom",
            plot.title        = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.margin       = margin(10, 10, 10, 10),
            axis.title        = element_blank(),
            axis.text         = element_blank(),
            panel.grid.major  = element_blank(),
            panel.grid.minor  = element_blank()
          )
        
        panel_pdf <- paste0("panel_", i, ".pdf")
        ggsave(
          filename = panel_pdf,
          plot     = panelPlot,
          device   = "pdf",
          width    = width_in,
          height   = height_in,
          units    = "in"
        )
        
        panel_files <- c(panel_files, panel_pdf)
      }
      
      #
      # MERGE: instructions.pdf (page1) + overview.pdf (page2) + panels (page3+)
      #
      tmp_files <- c("instructions.pdf", "overview.pdf", panel_files)
      qpdf::pdf_combine(input = tmp_files, output = "barrio_temp.pdf")
      file.copy("barrio_temp.pdf", file, overwrite = TRUE)
      
      
    }
  )
}

