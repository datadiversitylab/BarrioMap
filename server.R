###################
# server.R
###################
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(osmdata)
library(osmextract)
library(sf)
library(ggspatial)
library(jsonlite)
library(terra)

source('functions/functions.R')

server <- function(input, output, session) {

  # -----------------------------------------------------------
  # Reactive state
  # -----------------------------------------------------------

  rv <- reactiveValues(
    latitude        = 32.2540,
    longitude       = -110.9742,
    pageH           = 0.267,
    pageW           = 0.18,
    vpages          = 1,
    hpages          = 1,
    scale           = 5840,
    page            = "a4",
    usecoordinates  = TRUE,
    dpi             = 300,
    rects           = NULL,
    show_north      = TRUE,
    show_scale      = TRUE,
    show_coords     = TRUE,
    show_legend     = TRUE,
    # User data
    drawn_features  = NULL,
    user_points     = NULL,
    user_polygons   = NULL,
    user_raster     = NULL,
    drawn_id_counter = 0L
  )

  # Mirror relevant UI inputs to rv
  observe({
    for (nm in c("latitude","longitude","pageH","pageW","vpages","hpages",
                 "page","orientation","scale","usecoordinates","dpi",
                 "show_north","show_scale","show_coords","show_legend")) {
      local({
        n <- nm
        observeEvent(input[[n]], { rv[[n]] <- input[[n]] }, ignoreNULL = FALSE)
      })
    }
  })

  # -----------------------------------------------------------
  # Interactive map
  # -----------------------------------------------------------

  output$map <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl        = FALSE,
      attributionControl = FALSE
    )) %>%
      htmlwidgets::onRender(
        "function(el, x) { L.control.zoom({position:'bottomright'}).addTo(this); }"
      ) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("CartoDB.Positron",   group = "Light") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = -110.9742, lat = 32.2540, zoom = 10) %>%
      addLayersControl(
        baseGroups    = c("OSM (default)", "Light", "Dark"),
        overlayGroups = c("drawn", "user_points", "user_polygons", "raster"),
        options       = layersControlOptions(collapsed = TRUE)
      ) %>%
      addDrawToolbar(
        targetGroup       = "drawn",
        polylineOptions   = FALSE,
        circleOptions     = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions    = drawPolygonOptions(
          shapeOptions = drawShapeOptions(fillColor = "#FF9800", color = "#E65100",
                                          fillOpacity = 0.35, weight = 2)
        ),
        rectangleOptions  = drawRectangleOptions(
          shapeOptions = drawShapeOptions(fillColor = "#FF9800", color = "#E65100",
                                          fillOpacity = 0.35, weight = 2)
        ),
        markerOptions     = drawMarkerOptions(),
        editOptions       = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      )
  })

  # Coordinate display toggle
  observeEvent(input$usecoordinates, {
    if (isTRUE(input$usecoordinates)) {
      shinyjs::show("latitude"); shinyjs::show("longitude")
    } else {
      shinyjs::hide("latitude"); shinyjs::hide("longitude")
    }
  })

  # Geocode search box
  observeEvent(input$searchbox, {
    req(!isTRUE(input$usecoordinates), nzchar(trimws(input$searchbox)))
    coords <- tryCatch(tmaptools::geocode_OSM(q = input$searchbox), error = function(e) NULL)
    if (!is.null(coords) && length(coords) > 0) {
      lon <- as.numeric(coords[[2]][1]); lat <- as.numeric(coords[[2]][2])
      leafletProxy("map") %>% setView(lng = lon, lat = lat, zoom = 13)
    } else {
      showNotification("Location not found. Try a different search term.", type = "warning")
    }
  })

  # Page size presets
  observeEvent(rv$page, {
    if (rv$page != "other") {
      shinyjs::hide("pageH"); shinyjs::hide("pageW"); shinyjs::show("orientation")
    } else {
      shinyjs::show("pageH"); shinyjs::show("pageW"); shinyjs::hide("orientation")
    }
  })

  observeEvent(list(input$page, input$orientation), {
    if (input$page == "a4") {
      if (input$orientation == "v") { rv$pageH <- 0.267; rv$pageW <- 0.18
      } else                        { rv$pageH <- 0.18;  rv$pageW <- 0.267 }
    } else if (input$page == "a3") {
      if (input$orientation == "v") { rv$pageH <- 0.420; rv$pageW <- 0.297
      } else                        { rv$pageH <- 0.297; rv$pageW <- 0.420 }
    } else {
      rv$pageH <- input$pageH; rv$pageW <- input$pageW
    }
  })

  # Sync map center → coordinate inputs (when not locked)
  observeEvent(input$map_center, {
    req(!isTRUE(input$fixframe))
    updateNumericInput(session, "longitude", value = input$map_center$lng)
    updateNumericInput(session, "latitude",  value = input$map_center$lat)
  })

  # Draw rectangles on map based on scale + page + grid settings
  observe({
    zl      <- calcZoom(as.numeric(rv$scale), rv$latitude, as.numeric(rv$dpi))
    sc      <- as.numeric(rv$scale)
    pix_v   <- meter2screenpixel(rv$pageH * sc, "v", zl, rv$latitude)
    pix_h   <- meter2screenpixel(rv$pageW * sc, "h", zl, rv$latitude)
    recMap  <- leaflet(width = pix_h, height = pix_v) %>%
      addTiles() %>% setView(lng = rv$longitude, lat = rv$latitude, zoom = zl)
    rv$rects <- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)
    proxy    <- leafletProxy("map") %>% clearShapes()
    for (i in 1:nrow(rv$rects)) {
      proxy %>% addRectangles(lng1 = rv$rects[i,1], lat1 = rv$rects[i,3],
                              lng2 = rv$rects[i,2], lat2 = rv$rects[i,4],
                              fillColor = "transparent", color = "#1a5c3a",
                              weight = 2, dashArray = "6,3")
    }
  })

  # -----------------------------------------------------------
  # Draw toolbar handlers
  # -----------------------------------------------------------

  observeEvent(input$map_draw_new_feature, {
    feat      <- input$map_draw_new_feature
    geom_type <- feat$geometry$type
    coords    <- feat$geometry$coordinates
    leaflet_id <- feat$properties[["_leaflet_id"]] %||% rv$drawn_id_counter + 1L

    rv$drawn_id_counter <- rv$drawn_id_counter + 1L
    geom <- drawnGeomToSf(geom_type, coords)
    if (is.null(geom)) return()

    new_row <- sf::st_sf(
      label      = "",
      feature_id = as.integer(leaflet_id),
      geom_type  = geom_type,
      geometry   = geom
    )

    if (is.null(rv$drawn_features)) {
      rv$drawn_features <- new_row
    } else {
      rv$drawn_features <- rbind(rv$drawn_features, new_row)
    }

    # Update popup on the drawn feature
    popup_html <- makeEditPopup("drawn", leaflet_id, "")
    if (geom_type == "Point") {
      coords_pt <- coords
      leafletProxy("map") %>%
        addMarkers(lng = coords_pt[[1]], lat = coords_pt[[2]],
                   group = "drawn", popup = popup_html,
                   layerId = paste0("drawn_", leaflet_id))
    }
  })

  observeEvent(input$map_draw_deleted_features, {
    req(!is.null(rv$drawn_features))
    deleted_ids <- sapply(input$map_draw_deleted_features$features,
                          function(f) f$properties[["_leaflet_id"]])
    rv$drawn_features <- rv$drawn_features[
      !rv$drawn_features$feature_id %in% as.integer(deleted_ids), ]
    if (nrow(rv$drawn_features) == 0) rv$drawn_features <- NULL
  })

  # Save label from popup
  observeEvent(input$save_label, {
    info <- input$save_label
    new_label <- as.character(info$lbl %||% "")
    id        <- as.integer(info$id)
    src       <- as.character(info$src)

    if (src == "drawn" && !is.null(rv$drawn_features)) {
      idx <- which(rv$drawn_features$feature_id == id)
      if (length(idx) > 0) rv$drawn_features$label[idx[1]] <- new_label
    } else if (src == "points" && !is.null(rv$user_points)) {
      idx <- which(rv$user_points$.row_id == id)
      if (length(idx) > 0) rv$user_points$label[idx[1]] <- new_label
    } else if (src == "polygons" && !is.null(rv$user_polygons)) {
      idx <- which(rv$user_polygons$.row_id == id)
      if (length(idx) > 0) rv$user_polygons$label[idx[1]] <- new_label
    }
    leafletProxy("map") %>% clearPopups()
    showNotification("Label saved.", duration = 2)
  })

  # -----------------------------------------------------------
  # Upload handlers
  # -----------------------------------------------------------

  # CSV / TXT points
  observeEvent(input$csv_file, {
    req(input$csv_file)
    df <- tryCatch(
      read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
      error = function(e) { showNotification(paste("Could not read CSV:", conditionMessage(e)), type = "error"); NULL }
    )
    if (is.null(df)) return()

    col_lat <- grep("^lat(itude)?$", names(df), ignore.case = TRUE, value = TRUE)[1]
    col_lon <- grep("^(lon|lng)(gitude)?$", names(df), ignore.case = TRUE, value = TRUE)[1]

    if (is.na(col_lat) || is.na(col_lon)) {
      showNotification("CSV needs columns named 'lat' and 'lon' (or 'latitude'/'longitude').", type = "error")
      return()
    }

    pts <- tryCatch(
      sf::st_as_sf(df, coords = c(col_lon, col_lat), crs = 4326, remove = FALSE),
      error = function(e) { showNotification(paste("Could not create points:", conditionMessage(e)), type = "error"); NULL }
    )
    if (is.null(pts)) return()

    if (!"label" %in% names(pts)) pts$label <- ""
    pts$.row_id <- seq_len(nrow(pts))
    rv$user_points <- pts

    popups <- mapply(function(id, lbl) makeEditPopup("points", id, lbl),
                     pts$.row_id, pts$label)
    leafletProxy("map") %>%
      clearGroup("user_points") %>%
      addCircleMarkers(data = rv$user_points, group = "user_points",
                       color = "#E91E63", radius = 6, fillOpacity = 0.8,
                       popup = popups)

    showNotification(paste(nrow(pts), "points loaded."), duration = 3)
  })

  # Shapefile (upload as ZIP)
  observeEvent(input$shp_file, {
    req(input$shp_file)
    td <- file.path(tempdir(), paste0("shp_", format(Sys.time(), "%H%M%S")))
    dir.create(td, recursive = TRUE)

    tryCatch(utils::unzip(input$shp_file$datapath, exdir = td), error = function(e) NULL)
    shp_path <- list.files(td, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]

    if (is.na(shp_path)) {
      showNotification("No .shp file found in the ZIP. Make sure your ZIP contains the .shp file.", type = "error")
      return()
    }

    poly_sf <- tryCatch(
      sf::st_transform(sf::st_read(shp_path, quiet = TRUE), 4326),
      error = function(e) { showNotification(paste("Could not read shapefile:", conditionMessage(e)), type = "error"); NULL }
    )
    if (is.null(poly_sf)) return()

    if (!"label" %in% names(poly_sf)) poly_sf$label <- ""
    poly_sf$.row_id <- seq_len(nrow(poly_sf))
    rv$user_polygons <- poly_sf

    popups <- mapply(function(id, lbl) makeEditPopup("polygons", id, lbl),
                     poly_sf$.row_id, poly_sf$label)
    leafletProxy("map") %>%
      clearGroup("user_polygons") %>%
      addPolygons(data = rv$user_polygons, group = "user_polygons",
                  fillColor = "#FF9800", color = "#E65100",
                  fillOpacity = 0.35, weight = 2,
                  popup = popups)

    showNotification(paste(nrow(poly_sf), "polygon features loaded."), duration = 3)
  })

  # Raster (GeoTIFF)
  observeEvent(input$raster_file, {
    req(input$raster_file)
    r <- tryCatch(
      terra::rast(input$raster_file$datapath),
      error = function(e) { showNotification(paste("Could not read raster:", conditionMessage(e)), type = "error"); NULL }
    )
    if (is.null(r)) return()

    if (!terra::same.crs(r, "EPSG:4326"))
      r <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) r)

    rv$user_raster <- r

    alpha_val <- if (!is.null(input$raster_alpha)) input$raster_alpha else 0.7
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(r, opacity = alpha_val, group = "raster")

    showNotification("Raster loaded. Note: raster is shown on the map but not exported to PDF.", duration = 5)
  })

  # Update raster opacity when slider changes
  observeEvent(input$raster_alpha, {
    req(rv$user_raster)
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(rv$user_raster, opacity = input$raster_alpha, group = "raster")
  })

  # Reactive summary of user data
  output$data_summary <- renderUI({
    counts <- c(
      if (!is.null(rv$drawn_features))  paste0(nrow(rv$drawn_features), " drawn feature(s)") else NULL,
      if (!is.null(rv$user_points))     paste0(nrow(rv$user_points), " CSV point(s)") else NULL,
      if (!is.null(rv$user_polygons))   paste0(nrow(rv$user_polygons), " polygon feature(s)") else NULL,
      if (!is.null(rv$user_raster))     "1 raster layer" else NULL
    )
    if (length(counts) == 0) return(tags$p(style = "font-size:11px;color:#aaa;margin:4px 0;", "Nothing loaded yet."))
    tagList(lapply(counts, function(c) {
      tags$div(style = "font-size:11px;color:#1a5c3a;padding:2px 0;", HTML(paste0("\u2022 ", c)))
    }))
  })

  # Clear all user data
  observeEvent(input$clear_data_btn, {
    rv$drawn_features <- NULL
    rv$user_points    <- NULL
    rv$user_polygons  <- NULL
    rv$user_raster    <- NULL
    leafletProxy("map") %>%
      clearGroup("drawn") %>%
      clearGroup("user_points") %>%
      clearGroup("user_polygons") %>%
      clearGroup("raster")
    showNotification("All user data cleared.", duration = 3)
  })

  # -----------------------------------------------------------
  # Color inputs (dynamic per selected layer)
  # -----------------------------------------------------------

  output$layer_color_inputs <- renderUI({
    req(input$features)
    tagList(lapply(input$features, function(f) {
      def <- LAYER_DEFS[[f]]
      if (is.null(def)) return(NULL)
      if (def$type == "polygon") {
        fill_id   <- paste0(f, "_fill")
        border_id <- paste0(f, "_border")
        preview_fill_id   <- paste0(fill_id,   "_preview")
        preview_border_id <- paste0(border_id, "_preview")
        tags$div(
          style = "margin-bottom:10px;padding:8px;background:#fafafa;border-radius:6px;border:1px solid #eee;",
          tags$div(style = "font-size:11px;font-weight:600;color:#1a5c3a;margin-bottom:6px;", def$label),
          fluidRow(
            column(6,
              tags$label("Fill", style = "font-size:10px;color:#666;"),
              tags$div(style = "display:flex;align-items:center;gap:6px;",
                tags$div(id = preview_fill_id, style = paste0("width:22px;height:22px;border-radius:3px;background:", def$fill, ";border:1px solid #ccc;flex-shrink:0;")),
                tags$input(id = fill_id, type = "text", value = def$fill,
                           placeholder = "#RRGGBB",
                           style = "width:100%;padding:4px;border:1px solid #ddd;border-radius:4px;font-size:12px;",
                           oninput = paste0("document.getElementById('", preview_fill_id, "').style.background=this.value;Shiny.setInputValue('", fill_id, "',this.value)"))
              )
            ),
            column(6,
              tags$label("Outline", style = "font-size:10px;color:#666;"),
              tags$div(style = "display:flex;align-items:center;gap:6px;",
                tags$div(id = preview_border_id, style = paste0("width:22px;height:22px;border-radius:3px;background:", def$border, ";border:1px solid #ccc;flex-shrink:0;")),
                tags$input(id = border_id, type = "text", value = def$border,
                           placeholder = "#RRGGBB",
                           style = "width:100%;padding:4px;border:1px solid #ddd;border-radius:4px;font-size:12px;",
                           oninput = paste0("document.getElementById('", preview_border_id, "').style.background=this.value;Shiny.setInputValue('", border_id, "',this.value)"))
              )
            )
          )
        )
      } else {
        color_id    <- paste0(f, "_color")
        preview_id  <- paste0(color_id, "_preview")
        tags$div(
          style = "margin-bottom:10px;padding:8px;background:#fafafa;border-radius:6px;border:1px solid #eee;",
          tags$div(style = "font-size:11px;font-weight:600;color:#1a5c3a;margin-bottom:6px;", def$label),
          tags$div(style = "display:flex;align-items:center;gap:6px;",
            tags$div(id = preview_id, style = paste0("width:22px;height:22px;border-radius:3px;background:", def$color, ";border:1px solid #ccc;flex-shrink:0;")),
            tags$input(id = color_id, type = "text", value = def$color,
                       placeholder = "#RRGGBB",
                       style = "width:100%;padding:4px;border:1px solid #ddd;border-radius:4px;font-size:12px;",
                       oninput = paste0("document.getElementById('", preview_id, "').style.background=this.value;Shiny.setInputValue('", color_id, "',this.value)"))
          )
        )
      }
    }))
  })

  # -----------------------------------------------------------
  # Map code restore
  # -----------------------------------------------------------

  observeEvent(input$restore_map_btn, {
    req(nzchar(trimws(input$map_code_input %||% "")))
    params <- loadMapCode(input$map_code_input)
    if (is.null(params)) {
      showNotification("Code not found or expired (codes are valid for 30 days).", type = "error", duration = 5)
      return()
    }
    rv$latitude  <- params$latitude
    rv$longitude <- params$longitude
    rv$scale     <- params$scale
    rv$pageH     <- params$pageH
    rv$pageW     <- params$pageW
    rv$vpages    <- params$vpages
    rv$hpages    <- params$hpages
    rv$dpi       <- params$dpi

    updateNumericInput(session, "latitude",  value = params$latitude)
    updateNumericInput(session, "longitude", value = params$longitude)
    updateSelectInput(session,  "scale",     selected = as.character(params$scale))
    updateNumericInput(session, "vpages",    value = params$vpages)
    updateNumericInput(session, "hpages",    value = params$hpages)
    updateNumericInput(session, "dpi",       value = params$dpi)
    if (!is.null(params$features))
      updateCheckboxGroupInput(session, "features", selected = params$features)
    if (!is.null(params$show_north))
      updateCheckboxInput(session, "show_north",  value = params$show_north)
    if (!is.null(params$show_scale))
      updateCheckboxInput(session, "show_scale",  value = params$show_scale)
    if (!is.null(params$show_coords))
      updateCheckboxInput(session, "show_coords", value = params$show_coords)
    if (!is.null(params$show_legend))
      updateCheckboxInput(session, "show_legend", value = params$show_legend)

    # Zoom to stored scale level (not the default 10)
    zl_restore <- round(calcZoom(as.numeric(params$scale), params$latitude,
                                  as.numeric(params$dpi)))
    leafletProxy("map") %>%
      setView(lng = params$longitude, lat = params$latitude, zoom = zl_restore)

    showNotification("Map restored!", type = "message", duration = 3)
  })

  # -----------------------------------------------------------
  # Community gallery
  # -----------------------------------------------------------

  output$gallery_ui <- renderUI({
    gallery <- getGallery()
    count   <- getMapCount()

    stat_box <- tags$div(
      style = "text-align:center;padding:20px 0 10px;",
      tags$div(style = "font-size:2.5em;font-weight:800;color:#1a5c3a;", format(count, big.mark=",")),
      tags$div(style = "font-size:14px;color:#666;", "maps generated with BarrioMap")
    )

    if (length(gallery) == 0) {
      return(tagList(
        stat_box,
        tags$p(style = "text-align:center;color:#aaa;margin-top:20px;font-size:14px;",
               "No shared maps yet. Download a map and check 'Share to gallery'.")
      ))
    }

    cards <- lapply(gallery, function(e) {
      lat_lbl <- sprintf("%.3f\u00b0 %s", abs(e$lat), if (e$lat >= 0) "N" else "S")
      lon_lbl <- sprintf("%.3f\u00b0 %s", abs(e$lon), if (e$lon >= 0) "E" else "W")
      tags$div(
        class = "bm-gallery-card",
        tags$div(class = "bm-gc-code", e$code),
        tags$div(class = "bm-gc-info",
          tags$span(paste0(lat_lbl, ", ", lon_lbl)),
          tags$span(style = "color:#1a5c3a;", paste0("1:", format(as.numeric(e$scale), big.mark=","))),
          tags$span(style = "color:#aaa;font-size:10px;", e$date)
        )
      )
    })

    tagList(
      stat_box,
      tags$h5(style = "color:#333;margin:20px 0 12px;font-weight:600;", "Recently shared maps"),
      tags$div(class = "bm-gallery", tagList(cards))
    )
  })

  # -----------------------------------------------------------
  # PDF export
  # -----------------------------------------------------------

  output$print <- downloadHandler(
    filename = function() "barrio.pdf",
    content  = function(file) {

      req(rv$rects)
      rects     <- rv$rects
      is_single <- (rv$hpages == 1 && rv$vpages == 1)
      n_panels  <- nrow(rects)

      total_steps <- 2 + (if (!is_single) 2 else 0) + 2 * n_panels +
                     (if (isTRUE(input$show_legend)) 1 else 0) + 1
      progress <- shiny::Progress$new(max = total_steps)
      progress$set(message = "Generating your map", value = 0)
      on.exit(progress$close(), add = TRUE)

      export_dir <- tempfile("barrio_export_")
      dir.create(export_dir)
      on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)

      width_in  <- rv$pageW * 39.3701
      height_in <- rv$pageH * 39.3701

      all_lng  <- c(rects[,1], rects[,2])
      all_lat  <- c(rects[,3], rects[,4])
      min_lng  <- min(all_lng); max_lng <- max(all_lng)
      min_lat  <- min(all_lat); max_lat <- max(all_lat)
      ctr_lat  <- (min_lat + max_lat) / 2
      ctr_lng  <- (min_lng + max_lng) / 2
      lat_lbl  <- sprintf("%.4f\u00b0 %s", abs(ctr_lat), if (ctr_lat >= 0) "N" else "S")
      lng_lbl  <- sprintf("%.4f\u00b0 %s", abs(ctr_lng), if (ctr_lng >= 0) "E" else "W")

      map_code <- generateMapCode()
      footer   <- paste0("datadiversitylab.github.io/barriomap/  \u00b7  OpenStreetMap (ODbL)  \u00b7  ",
                         format(Sys.Date(), "%B %d, %Y"), "  \u00b7  Code: ", map_code)

      # Collect user colors
      features_list <- input$features %||% character(0)
      layer_colors  <- list()
      for (f in features_list) {
        def <- LAYER_DEFS[[f]]
        if (is.null(def)) next
        if (def$type == "polygon") {
          layer_colors[[paste0(f,"_fill")]]   <- getColor(input[[paste0(f,"_fill")]],   def$fill)
          layer_colors[[paste0(f,"_border")]] <- getColor(input[[paste0(f,"_border")]], def$border)
        } else {
          layer_colors[[paste0(f,"_color")]]  <- getColor(input[[paste0(f,"_color")]],  def$color)
        }
      }

      # ggplot builder for each map page
      buildPlot <- function(features_data, xlim, ylim, title,
                             panel_outline_sf = NULL, show_pnums = FALSE, pcenters_sf = NULL) {
        p <- ggplot()

        # OSM layers (polygons first, then lines, then points)
        draw_order <- c(
          names(features_data)[sapply(names(features_data), function(f) LAYER_DEFS[[f]]$type == "polygon")],
          names(features_data)[sapply(names(features_data), function(f) LAYER_DEFS[[f]]$type == "line")],
          names(features_data)[sapply(names(features_data), function(f) LAYER_DEFS[[f]]$type == "point")]
        )
        for (f in draw_order) {
          data <- features_data[[f]]
          def  <- LAYER_DEFS[[f]]
          if (is.null(data) || nrow(data) == 0) next
          if (def$type == "polygon") {
            fill_c   <- layer_colors[[paste0(f,"_fill")]]   %||% def$fill
            border_c <- layer_colors[[paste0(f,"_border")]] %||% def$border
            p <- p + geom_sf(data = data, fill = fill_c, color = border_c,
                             linewidth = def$lwd, alpha = def$alpha)
          } else if (def$type == "line") {
            line_c <- layer_colors[[paste0(f,"_color")]] %||% def$color
            p <- p + geom_sf(data = data, color = line_c, linewidth = def$lwd, alpha = def$alpha)
          } else {
            pt_c <- layer_colors[[paste0(f,"_color")]] %||% def$color
            p <- p + geom_sf(data = data, color = pt_c, size = 1.5, alpha = def$alpha)
          }
        }

        # User polygons
        if (!is.null(rv$user_polygons) && nrow(rv$user_polygons) > 0) {
          p <- p + geom_sf(data = rv$user_polygons, fill = "#FF980040",
                           color = "#E65100", linewidth = 0.5)
          p <- p + geom_sf_text(
            data = rv$user_polygons[nzchar(rv$user_polygons$label), ],
            aes(label = label), size = 2, color = "#E65100"
          )
        }

        # User points
        if (!is.null(rv$user_points) && nrow(rv$user_points) > 0) {
          p <- p + geom_sf(data = rv$user_points, color = "#E91E63", size = 2)
          pts_with_labels <- rv$user_points[nzchar(rv$user_points$label), ]
          if (nrow(pts_with_labels) > 0)
            p <- p + geom_sf_text(data = pts_with_labels, aes(label = label),
                                   size = 2, nudge_y = 0.0003, color = "#880E4F")
        }

        # Panel outlines (overview only)
        if (!is.null(panel_outline_sf))
          p <- p + geom_sf(data = panel_outline_sf, fill = NA, color = "#cc2200", linewidth = 0.7)
        if (show_pnums && !is.null(pcenters_sf))
          p <- p + geom_sf_text(data = pcenters_sf, aes(label = label),
                                size = 3, color = "#cc2200", fontface = "bold")

        # Map elements
        if (isTRUE(input$show_scale))
          p <- p + annotation_scale(location = "bl", width_hint = 0.25,
                                    bar_cols = c("#333","#fff"), text_cex = 0.65)
        if (isTRUE(input$show_north))
          p <- p + annotation_north_arrow(
            location = "tr", which_north = "true",
            style    = north_arrow_nautical(fill = c("#333","#fff"),
                                            line_col = "#333", text_col = "#333"),
            height = unit(1.1,"cm"), width = unit(1.1,"cm")
          )

        axis_theme <- if (isTRUE(input$show_coords)) {
          theme(axis.text  = element_text(size = 6, color = "#555"),
                axis.ticks = element_line(color = "#bbb", linewidth = 0.3))
        } else {
          theme(axis.text = element_blank(), axis.ticks = element_blank())
        }

        p +
          coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          labs(title = title, caption = footer) +
          theme_minimal(base_size = 9) +
          theme(
            legend.position  = "none",
            panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.2),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(fill = NA, color = "#444", linewidth = 0.4),
            axis.title       = element_blank(),
            plot.title       = element_text(face = "bold", size = 10, hjust = 0.5,
                                            margin = margin(6,0,3,0)),
            plot.caption     = element_text(size = 5, color = "#aaa", hjust = 0.5,
                                            margin = margin(4,0,0,0)),
            plot.margin      = margin(5,5,5,5),
            plot.background  = element_rect(fill = "white", color = NA)
          ) +
          axis_theme
      }

      # COVER PAGE
      progress$inc(amount = 1, detail = "Building the cover page")

      panel_lines <- if (is_single) {
        sprintf("Area: %.4f\u00b0 to %.4f\u00b0 E/W,  %.4f\u00b0 to %.4f\u00b0 N/S",
                min_lng, max_lng, min_lat, max_lat)
      } else {
        paste(sapply(seq_len(n_panels), function(i) {
          row_i <- floor((i-1)/rv$hpages) + 1; col_i <- ((i-1) %% rv$hpages) + 1
          bb <- rects[i,]
          sprintf("Pg %d \u2014 Panel (%d,%d): %.3f\u00b0, %.3f\u00b0 to %.3f\u00b0, %.3f\u00b0",
                  i+1, row_i, col_i, bb[1], bb[3], bb[2], bb[4])
        }), collapse = "\n")
      }
      panel_line_list <- strsplit(panel_lines, "\n")[[1]]

      cover_green <- "#1a5c3a"
      coverPlot <- ggplot() +
        annotate("rect", xmin=0, xmax=1, ymin=0.87, ymax=1, fill=cover_green, color=NA) +
        annotate("text", x=0.5, y=0.945, label="Barrio\u00a0Map",
                 color="white", size=16, fontface="bold", hjust=0.5) +
        annotate("text", x=0.5, y=0.893, label="Open-source mapping for communities",
                 color="#a8d5ba", size=4.2, hjust=0.5) +
        annotate("segment", x=0.08, xend=0.92, y=0.842, yend=0.842, color="#ddd", linewidth=0.4) +
        annotate("text", x=0.08, y=0.808, label="Location",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.08, y=0.775, label=paste0(lat_lbl, ",  ", lng_lbl),
                 color="#222", size=5, hjust=0) +
        annotate("text", x=0.58, y=0.808, label="Scale",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.58, y=0.775,
                 label=paste0("1:", format(as.numeric(rv$scale), big.mark=",")),
                 color="#222", size=5, hjust=0) +
        annotate("text", x=0.08, y=0.738, label="Generated",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.08, y=0.705, label=format(Sys.time(), "%B %d, %Y at %I:%M %p"),
                 color="#222", size=4.5, hjust=0) +
        annotate("text", x=0.58, y=0.738, label="Page size",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.58, y=0.705,
                 label=paste0(toupper(rv$page), " \u00b7 ", rv$dpi, " DPI"),
                 color="#222", size=4.5, hjust=0) +
        annotate("segment", x=0.08, xend=0.92, y=0.672, yend=0.672, color="#ddd", linewidth=0.4) +
        annotate("text", x=0.5, y=0.643, label="Your map code",
                 color="#888", size=3.2, hjust=0.5, fontface="bold") +
        annotate("rect", xmin=0.27, xmax=0.73, ymin=0.583, ymax=0.633,
                 fill="#f0f8f4", color=cover_green, linewidth=0.7) +
        annotate("text", x=0.5, y=0.608, label=map_code,
                 color=cover_green, size=13, fontface="bold", hjust=0.5) +
        annotate("text", x=0.5, y=0.555,
                 label="Enter this code at datadiversitylab.github.io/barriomap/ to restore this map (30 days).",
                 color="#555", size=3.2, hjust=0.5) +
        annotate("segment", x=0.08, xend=0.92, y=0.522, yend=0.522, color="#ddd", linewidth=0.4) +
        annotate("text", x=0.08, y=0.497,
                 label=if (is_single) "Area covered" else paste0(n_panels, " panels"),
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text",
                 x = rep(0.08, length(panel_line_list)),
                 y = 0.478 - 0.022 * seq_along(panel_line_list),
                 label = panel_line_list,
                 color="#333", size=2.8, hjust=0) +
        annotate("segment", x=0, xend=1, y=0.09, yend=0.09, color="#ddd", linewidth=0.3) +
        annotate("text", x=0.5, y=0.05,
                 label="datadiversitylab.github.io/barriomap/  \u00b7  Data from OpenStreetMap (ODbL)",
                 color="#bbb", size=2.8, hjust=0.5, fontface="italic") +
        xlim(0,1) + ylim(0,1) +
        theme_void() +
        theme(plot.background = element_rect(fill="white", color=NA), plot.margin=margin(0,0,0,0))

      ggsave(file.path(export_dir, "cover.pdf"), plot=coverPlot,
             device="pdf", width=width_in, height=height_in, units="in")

      # FETCH ALL DATA (one call for full extent)
      progress$inc(amount = 1,
                   detail = "Fetching map data (first visit to a new region takes longer)")

      full_bbox     <- c(min_lng, min_lat, max_lng, max_lat)
      all_features  <- tryCatch(
        getOsmFeatures(full_bbox, features_list),
        error = function(e) stop(safeError(paste0(
          "Could not fetch map data (", conditionMessage(e), "). Please try again.")))
      )

      # OVERVIEW (multi-panel only)
      if (!is_single) {
        progress$inc(amount = 1, detail = "Drawing the overview page")

        panels_sf <- sf::st_sf(geometry = sf::st_sfc(lapply(seq_len(n_panels), function(i) {
          bb <- rects[i,]
          sf::st_polygon(list(matrix(c(bb[1],bb[3],bb[1],bb[4],bb[2],bb[4],
                                       bb[2],bb[3],bb[1],bb[3]), ncol=2, byrow=TRUE)))
        }), crs = 4326))

        centers_sf <- sf::st_sf(
          label = sapply(seq_len(n_panels), function(i) {
            row_i <- floor((i-1)/rv$hpages)+1; col_i <- ((i-1) %% rv$hpages)+1
            paste0("(", row_i, ",", col_i, ")\nPg ", i+1)
          }),
          geometry = sf::st_sfc(lapply(seq_len(n_panels), function(i) {
            bb <- rects[i,]
            sf::st_point(c((bb[1]+bb[2])/2, (bb[3]+bb[4])/2))
          }), crs = 4326)
        )

        ov <- buildPlot(all_features, c(min_lng,max_lng), c(min_lat,max_lat),
                        "Overview \u2014 all panels",
                        panel_outline_sf = panels_sf,
                        show_pnums = TRUE, pcenters_sf = centers_sf)
        ggsave(file.path(export_dir,"overview.pdf"), plot=ov, device="pdf",
               width=width_in, height=height_in, units="in")
      }

      # PANEL PAGES
      panel_files <- character(0)
      for (i in seq_len(n_panels)) {
        bb <- c(rects[i,1], rects[i,3], rects[i,2], rects[i,4])
        row_i <- floor((i-1)/rv$hpages)+1; col_i <- ((i-1) %% rv$hpages)+1

        progress$inc(amount = 1, detail = paste0("Clipping data for panel ", i, " of ", n_panels))
        bbox_sfc <- sf::st_as_sfc(sf::st_bbox(c(xmin=bb[1],ymin=bb[2],xmax=bb[3],ymax=bb[4]), crs=4326))
        panel_features <- lapply(all_features, function(d) {
          if (is.null(d) || nrow(d) == 0) return(NULL)
          tryCatch(suppressWarnings(sf::st_crop(d, bbox_sfc)), error = function(e) d)
        })

        progress$inc(amount = 1, detail = paste0("Drawing panel ", i, " of ", n_panels))
        title_str <- if (is_single) {
          paste0("1:", format(as.numeric(rv$scale), big.mark=","), "  \u00b7  ", lat_lbl, ", ", lng_lbl)
        } else {
          paste0("Panel (", row_i, ",", col_i, ")  \u2014  Page ", i+1)
        }
        pp <- buildPlot(panel_features, c(bb[1],bb[3]), c(bb[2],bb[4]), title_str)
        pdf_path <- file.path(export_dir, paste0("panel_",i,".pdf"))
        ggsave(pdf_path, plot=pp, device="pdf", width=width_in, height=height_in, units="in")
        panel_files <- c(panel_files, pdf_path)
      }

      # LEGEND PAGE
      legend_file <- character(0)
      active_features <- names(all_features)[!sapply(all_features, is.null)]
      if (isTRUE(input$show_legend) && length(active_features) > 0) {
        progress$inc(amount = 1, detail = "Building the legend")
        n_items <- length(active_features)
        y_vals  <- seq(0.66, by = -0.09, length.out = n_items)
        lp <- ggplot() +
          annotate("rect", xmin=0, xmax=1, ymin=0.85, ymax=1, fill=cover_green, color=NA) +
          annotate("text", x=0.5, y=0.921, label="Map Legend",
                   color="white", size=10, fontface="bold", hjust=0.5)
        for (i in seq_along(active_features)) {
          f   <- active_features[i]; def <- LAYER_DEFS[[f]]; y <- y_vals[i]
          if (def$type == "polygon") {
            fc <- layer_colors[[paste0(f,"_fill")]] %||% def$fill
            bc <- layer_colors[[paste0(f,"_border")]] %||% def$border
            lp <- lp + annotate("rect", xmin=0.08, xmax=0.17, ymin=y-0.025, ymax=y+0.025,
                                fill=fc, color=bc, linewidth=0.4)
          } else if (def$type == "line") {
            lc <- layer_colors[[paste0(f,"_color")]] %||% def$color
            lp <- lp + annotate("segment", x=0.08, xend=0.17, y=y, yend=y, color=lc, linewidth=2)
          } else {
            pc <- layer_colors[[paste0(f,"_color")]] %||% def$color
            lp <- lp + annotate("point", x=0.125, y=y, color=pc, size=3)
          }
          lp <- lp + annotate("text", x=0.21, y=y, label=def$label,
                               hjust=0, size=5, color="#222")
        }
        lp <- lp +
          annotate("text", x=0.5, y=0.25, label="Data: OpenStreetMap contributors (ODbL)",
                   size=3.5, color="#888", hjust=0.5) +
          annotate("text", x=0.5, y=0.17, label=paste0("Map code: ", map_code),
                   size=5, color=cover_green, fontface="bold", hjust=0.5) +
          annotate("text", x=0.5, y=0.09, label="datadiversitylab.github.io/barriomap/",
                   size=3.5, color="#bbb", hjust=0.5, fontface="italic") +
          xlim(0,1) + ylim(0,1) +
          theme_void() +
          theme(plot.background = element_rect(fill="white", color=NA))
        legend_path <- file.path(export_dir, "legend.pdf")
        ggsave(legend_path, plot=lp, device="pdf", width=width_in, height=height_in, units="in")
        legend_file <- legend_path
      }

      # MERGE
      progress$inc(amount = 1, detail = "Putting the PDF together")
      ov_file   <- if (!is_single) file.path(export_dir,"overview.pdf") else character(0)
      tmp_files <- c(file.path(export_dir,"cover.pdf"), ov_file, panel_files, legend_file)
      merged    <- file.path(export_dir, "barrio_temp.pdf")
      qpdf::pdf_combine(input = tmp_files, output = merged)

      if (!file.exists(merged) || file.info(merged)$size == 0)
        stop(safeError("The PDF export did not complete. Please try again."))

      # Save code, increment counter, optionally add to gallery
      saveMapCode(map_code, list(
        latitude    = rv$latitude, longitude   = rv$longitude,
        scale       = rv$scale,    pageH        = rv$pageH,
        pageW       = rv$pageW,    page         = rv$page,
        vpages      = rv$vpages,   hpages       = rv$hpages,
        dpi         = rv$dpi,      features     = features_list,
        show_north  = rv$show_north, show_scale = rv$show_scale,
        show_coords = rv$show_coords, show_legend = rv$show_legend
      ))
      incrementMapCount()
      if (isTRUE(input$share_to_gallery))
        addToGallery(map_code, rv$latitude, rv$longitude, rv$scale,
                     format(Sys.Date(), "%Y-%m-%d"))

      file.copy(merged, file, overwrite = TRUE)

      # Show code modal
      showModal(modalDialog(
        title = NULL, footer = modalButton("Got it!"), easyClose = TRUE,
        tags$div(
          style = "text-align:center;padding:10px 20px 20px;",
          tags$div(style = "font-size:12px;color:#888;margin-bottom:8px;",
                   "Your map is ready \u2014 save this code"),
          tags$div(
            style = paste0("font-size:2.2em;font-weight:800;letter-spacing:6px;",
                           "color:#1a5c3a;padding:14px 20px;background:#f0f8f4;",
                           "border-radius:8px;border:2px solid #1a5c3a;display:inline-block;"),
            map_code
          ),
          tags$p(style = "color:#555;font-size:13px;margin-top:10px;",
                 "Enter at datadiversitylab.github.io/barriomap/ to restore this map for 30 days."),
          tags$hr(),
          checkboxInput("share_to_gallery_modal",
                        "Add to community gallery (optional, anonymous)", FALSE)
        )
      ))
    }
  )

  # -----------------------------------------------------------
  # Data bundle download
  # -----------------------------------------------------------

  output$download_data <- downloadHandler(
    filename = function() paste0("barriomap_bundle_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content  = function(file) {
      bd <- tempfile("bm_bundle_"); dir.create(bd)
      on.exit(unlink(bd, recursive = TRUE), add = TRUE)
      files_in_bundle <- character(0)

      if (!is.null(rv$drawn_features) && nrow(rv$drawn_features) > 0) {
        f <- file.path(bd, "drawn.geojson")
        write_safe_geojson(rv$drawn_features, f)
        files_in_bundle <- c(files_in_bundle, f)
      }
      if (!is.null(rv$user_points)) {
        f <- file.path(bd, "points.geojson")
        write_safe_geojson(rv$user_points, f)
        files_in_bundle <- c(files_in_bundle, f)
      }
      if (!is.null(rv$user_polygons)) {
        f <- file.path(bd, "polygons.geojson")
        write_safe_geojson(rv$user_polygons, f)
        files_in_bundle <- c(files_in_bundle, f)
      }

      readme_path <- file.path(bd, "README.txt")
      writeLines(c(
        "BarrioMap Data Bundle",
        "=====================",
        paste("Generated:", format(Sys.time())),
        paste("Location: ", rv$latitude, ",", rv$longitude),
        paste("Scale: 1:", rv$scale),
        "",
        "Files in this bundle:",
        "  drawn.geojson    - Features drawn on the map",
        "  points.geojson   - Uploaded CSV points",
        "  polygons.geojson - Uploaded shapefile polygons",
        "",
        "Open GeoJSON files in QGIS, ArcGIS, or any GIS tool.",
        "Data from OpenStreetMap (c) contributors, ODbL license.",
        "",
        "Tool: datadiversitylab.github.io/barriomap/"
      ), readme_path)
      files_in_bundle <- c(files_in_bundle, readme_path)

      meta_path <- file.path(bd, "metadata.json")
      writeLines(jsonlite::toJSON(list(
        generated = format(Sys.time()), tool = "BarrioMap",
        url       = "datadiversitylab.github.io/barriomap/",
        latitude  = rv$latitude, longitude = rv$longitude,
        scale     = rv$scale,    features  = input$features
      ), auto_unbox = TRUE, pretty = TRUE), meta_path)
      files_in_bundle <- c(files_in_bundle, meta_path)

      utils::zip(zipfile = file, files = files_in_bundle, flags = "-j")
    }
  )
}
