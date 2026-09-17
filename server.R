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

# Query Nominatim for place suggestions.
nominatim_suggest <- function(q, limit = 5) {
  url <- paste0(
    "https://nominatim.openstreetmap.org/search?q=",
    utils::URLencode(trimws(q), reserved = TRUE),
    "&format=json&limit=", limit, "&addressdetails=0&accept-language=en"
  )
  tryCatch(jsonlite::fromJSON(url, simplifyVector = TRUE), error = function(e) NULL)
}

server <- function(input, output, session) {

  # -----------------------------------------------------------
  # Reactive state
  # -----------------------------------------------------------

  rv <- reactiveValues(
    latitude         = 32.2540,
    longitude        = -110.9742,
    pageH            = 0.267,
    pageW            = 0.18,
    vpages           = 1,
    hpages           = 1,
    scale            = 31680,
    page             = "a4",
    dpi              = 300,
    rects            = NULL,
    show_north       = TRUE,
    show_scale       = TRUE,
    show_coords      = TRUE,
    show_legend      = TRUE,
    drawn_features   = NULL,
    user_points      = NULL,
    user_polygons    = NULL,
    user_raster      = NULL,
    drawn_id_counter = 0L,
    export_path      = NULL,
    export_ext       = ".pdf"
  )

  observe({
    for (nm in c("latitude","longitude","pageH","pageW","vpages","hpages",
                 "page","orientation","scale","dpi",
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
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      htmlwidgets::onRender(
        "function(el,x){L.control.zoom({position:'bottomright'}).addTo(this);}"
      ) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("CartoDB.Positron",   group = "Light") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = -110.9742, lat = 32.2540, zoom = 12) %>%
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
        markerOptions   = drawMarkerOptions(),
        editOptions     = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
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

  # Sync map center to lat/lon inputs when not locked
  observeEvent(input$map_center, {
    req(!isTRUE(input$fixframe))
    updateNumericInput(session, "longitude", value = input$map_center$lng)
    updateNumericInput(session, "latitude",  value = input$map_center$lat)
  })

  # Draw page rectangles on the map
  observe({
    zl     <- calcZoom(as.numeric(rv$scale), rv$latitude, as.numeric(rv$dpi))
    sc     <- as.numeric(rv$scale)
    pix_v  <- meter2screenpixel(rv$pageH * sc, "v", zl, rv$latitude)
    pix_h  <- meter2screenpixel(rv$pageW * sc, "h", zl, rv$latitude)
    recMap <- leaflet(width = pix_h, height = pix_v) %>%
      addTiles() %>% setView(lng = rv$longitude, lat = rv$latitude, zoom = zl)
    rv$rects <- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)

    outline_col <- getColor(input$outline_color, "#1a5c3a")
    proxy <- leafletProxy("map") %>% clearShapes()
    for (i in 1:nrow(rv$rects)) {
      proxy %>% addRectangles(
        lng1 = rv$rects[i,1], lat1 = rv$rects[i,3],
        lng2 = rv$rects[i,2], lat2 = rv$rects[i,4],
        fillColor = "transparent", color = outline_col,
        weight = 3, dashArray = "8,4", opacity = 0.9
      )
    }
  })

  # -----------------------------------------------------------
  # Search + autocomplete
  # -----------------------------------------------------------

  search_debounced <- debounce(reactive(input$searchbox), 400)

  output$search_suggestions <- renderUI({
    q <- search_debounced()
    if (is.null(q) || nchar(trimws(q)) < 3) return(NULL)
    results <- nominatim_suggest(q)
    if (is.null(results) || nrow(results) == 0) return(NULL)
    top_n <- min(nrow(results), 5)
    tags$div(class = "bm-suggestions-container",
      lapply(seq_len(top_n), function(i) {
        r       <- results[i, ]
        display <- substr(r$display_name, 1, 70)
        tags$div(class = "bm-suggestion",
                 onclick = sprintf(
                   "Shiny.setInputValue('select_suggestion',{lat:'%s',lon:'%s'},{priority:'event'})",
                   r$lat, r$lon
                 ),
                 display)
      })
    )
  })

  observeEvent(input$search_btn, {
    req(nzchar(trimws(input$searchbox %||% "")))
    coords <- tryCatch(tmaptools::geocode_OSM(q = input$searchbox), error = function(e) NULL)
    if (!is.null(coords) && length(coords) > 0) {
      lon <- as.numeric(coords[[2]][1]); lat <- as.numeric(coords[[2]][2])
      updateNumericInput(session, "latitude",  value = lat)
      updateNumericInput(session, "longitude", value = lon)
      leafletProxy("map") %>% setView(lng = lon, lat = lat, zoom = 14)
      updateTextInput(session, "searchbox", value = "")
    } else {
      showNotification("Location not found. Try a different search term.", type = "warning")
    }
  })

  observeEvent(input$select_suggestion, {
    lat <- as.numeric(input$select_suggestion$lat)
    lon <- as.numeric(input$select_suggestion$lon)
    updateNumericInput(session, "latitude",  value = lat)
    updateNumericInput(session, "longitude", value = lon)
    leafletProxy("map") %>% setView(lng = lon, lat = lat, zoom = 14)
    updateTextInput(session, "searchbox", value = "")
  })

  # -----------------------------------------------------------
  # Draw toolbar handlers
  # -----------------------------------------------------------

  observeEvent(input$map_draw_new_feature, {
    feat       <- input$map_draw_new_feature
    geom_type  <- feat$geometry$type
    coords     <- feat$geometry$coordinates
    leaflet_id <- feat$properties[["_leaflet_id"]] %||% (rv$drawn_id_counter + 1L)
    rv$drawn_id_counter <- rv$drawn_id_counter + 1L

    geom <- drawnGeomToSf(geom_type, coords)
    if (is.null(geom)) return()

    new_row <- sf::st_sf(
      label      = "",
      feature_id = as.integer(leaflet_id),
      geom_type  = geom_type,
      geometry   = geom
    )
    rv$drawn_features <- if (is.null(rv$drawn_features)) new_row else rbind(rv$drawn_features, new_row)

    if (geom_type == "Point") {
      popup_html <- makeEditPopup("drawn", leaflet_id, "")
      leafletProxy("map") %>%
        addMarkers(lng = coords[[1]], lat = coords[[2]],
                   group = "drawn", popup = popup_html,
                   layerId = paste0("drawn_", leaflet_id))
    }
  })

  observeEvent(input$map_draw_deleted_features, {
    req(!is.null(rv$drawn_features))
    del_ids <- sapply(input$map_draw_deleted_features$features,
                      function(f) f$properties[["_leaflet_id"]])
    rv$drawn_features <- rv$drawn_features[
      !rv$drawn_features$feature_id %in% as.integer(del_ids), ]
    if (nrow(rv$drawn_features) == 0) rv$drawn_features <- NULL
  })

  # Save label and immediately re-render the affected group
  observeEvent(input$save_label, {
    info      <- input$save_label
    new_label <- as.character(info$lbl %||% "")
    id        <- as.integer(info$id)
    src       <- as.character(info$src)
    lbl_opts  <- labelOptions(permanent = TRUE, noHide = TRUE, direction = "top",
                              textOnly = TRUE,
                              style = list("font-size" = "11px", "font-weight" = "600"))

    if (src == "drawn" && !is.null(rv$drawn_features)) {
      idx <- which(rv$drawn_features$feature_id == id)
      if (length(idx) > 0) rv$drawn_features$label[idx[1]] <- new_label

    } else if (src == "points" && !is.null(rv$user_points)) {
      idx <- which(rv$user_points$.row_id == id)
      if (length(idx) > 0) rv$user_points$label[idx[1]] <- new_label
      pts    <- rv$user_points
      popups <- mapply(function(i2, l) makeEditPopup("points", i2, l),
                       pts$.row_id, pts$label, USE.NAMES = FALSE)
      leafletProxy("map") %>%
        clearGroup("user_points") %>%
        addCircleMarkers(data = pts, group = "user_points",
                         color = "#E91E63", radius = 6, fillOpacity = 0.8,
                         popup = popups, label = ~label, labelOptions = lbl_opts)

    } else if (src == "polygons" && !is.null(rv$user_polygons)) {
      idx <- which(rv$user_polygons$.row_id == id)
      if (length(idx) > 0) rv$user_polygons$label[idx[1]] <- new_label
      polys  <- rv$user_polygons
      popups <- mapply(function(i2, l) makeEditPopup("polygons", i2, l),
                       polys$.row_id, polys$label, USE.NAMES = FALSE)
      ctr    <- tryCatch(sf::st_centroid(polys), error = function(e) NULL)
      proxy  <- leafletProxy("map") %>%
        clearGroup("user_polygons") %>%
        addPolygons(data = polys, group = "user_polygons",
                    fillColor = "#FF9800", color = "#E65100",
                    fillOpacity = 0.35, weight = 2, popup = popups)
      if (!is.null(ctr)) {
        labeled_ctr <- ctr[nzchar(polys$label), ]
        if (nrow(labeled_ctr) > 0)
          proxy %>% addLabelOnlyMarkers(data = labeled_ctr, group = "user_polygons",
                                        label = ~label,
                                        labelOptions = labelOptions(
                                          permanent = TRUE, noHide = TRUE,
                                          direction = "center", textOnly = TRUE,
                                          style = list("font-size" = "11px",
                                                       "font-weight" = "600")))
      }
    }

    leafletProxy("map") %>% clearPopups()
    showNotification("Label saved.", duration = 2)
  })

  # -----------------------------------------------------------
  # Upload handlers
  # -----------------------------------------------------------

  observeEvent(input$csv_file, {
    req(input$csv_file)
    df <- tryCatch(read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
                   error = function(e) {
                     showNotification(paste("CSV error:", conditionMessage(e)), type = "error")
                     NULL
                   })
    if (is.null(df)) return()

    col_lat <- grep("^lat(itude)?$", names(df), ignore.case = TRUE, value = TRUE)[1]
    col_lon <- grep("^(lon|lng)(gitude)?$", names(df), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(col_lat) || is.na(col_lon)) {
      showNotification("CSV needs columns named lat and lon (or latitude/longitude).",
                       type = "error"); return()
    }

    pts <- tryCatch(sf::st_as_sf(df, coords = c(col_lon, col_lat), crs = 4326, remove = FALSE),
                    error = function(e) {
                      showNotification(paste("Points error:", conditionMessage(e)), type = "error")
                      NULL
                    })
    if (is.null(pts)) return()
    if (!"label" %in% names(pts)) pts$label <- ""
    pts$.row_id      <- seq_len(nrow(pts))
    rv$user_points   <- pts

    popups <- mapply(function(i2, l) makeEditPopup("points", i2, l),
                     pts$.row_id, pts$label, USE.NAMES = FALSE)
    lbl_opts <- labelOptions(permanent = TRUE, noHide = TRUE, direction = "top",
                             textOnly = TRUE, style = list("font-size" = "11px",
                                                           "font-weight" = "600"))
    leafletProxy("map") %>%
      clearGroup("user_points") %>%
      addCircleMarkers(data = rv$user_points, group = "user_points",
                       color = "#E91E63", radius = 6, fillOpacity = 0.8,
                       popup = popups, label = ~label, labelOptions = lbl_opts)
    showNotification(paste(nrow(pts), "points loaded."), duration = 3)
  })

  observeEvent(input$shp_file, {
    req(input$shp_file)
    td <- file.path(tempdir(), paste0("shp_", format(Sys.time(), "%H%M%S")))
    dir.create(td, recursive = TRUE)
    tryCatch(utils::unzip(input$shp_file$datapath, exdir = td), error = function(e) NULL)
    shp_path <- list.files(td, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
    if (is.na(shp_path)) {
      showNotification("No .shp file found in the ZIP.", type = "error"); return()
    }
    poly_sf <- tryCatch(sf::st_transform(sf::st_read(shp_path, quiet = TRUE), 4326),
                        error = function(e) {
                          showNotification(paste("Shapefile error:", conditionMessage(e)), type = "error")
                          NULL
                        })
    if (is.null(poly_sf)) return()
    if (!"label" %in% names(poly_sf)) poly_sf$label <- ""
    poly_sf$.row_id  <- seq_len(nrow(poly_sf))
    rv$user_polygons <- poly_sf

    popups <- mapply(function(i2, l) makeEditPopup("polygons", i2, l),
                     poly_sf$.row_id, poly_sf$label, USE.NAMES = FALSE)
    ctr    <- tryCatch(sf::st_centroid(poly_sf), error = function(e) NULL)
    proxy  <- leafletProxy("map") %>%
      clearGroup("user_polygons") %>%
      addPolygons(data = rv$user_polygons, group = "user_polygons",
                  fillColor = "#FF9800", color = "#E65100",
                  fillOpacity = 0.35, weight = 2, popup = popups)
    if (!is.null(ctr)) {
      labeled <- ctr[nzchar(poly_sf$label), ]
      if (nrow(labeled) > 0)
        proxy %>% addLabelOnlyMarkers(data = labeled, group = "user_polygons",
                                      label = ~label,
                                      labelOptions = labelOptions(permanent = TRUE,
                                                                  noHide = TRUE, direction = "center",
                                                                  textOnly = TRUE))
    }
    showNotification(paste(nrow(poly_sf), "polygon features loaded."), duration = 3)
  })

  observeEvent(input$raster_file, {
    req(input$raster_file)
    r <- tryCatch(terra::rast(input$raster_file$datapath),
                  error = function(e) {
                    showNotification(paste("Raster error:", conditionMessage(e)), type = "error")
                    NULL
                  })
    if (is.null(r)) return()
    if (!terra::same.crs(r, "EPSG:4326"))
      r <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) r)
    rv$user_raster <- r
    alpha_val      <- input$raster_alpha %||% 0.7
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(r, opacity = alpha_val, group = "raster")
    showNotification("Raster loaded. Rasters are shown on the map but not in the PDF.",
                     duration = 5)
  })

  observeEvent(input$raster_alpha, {
    req(rv$user_raster)
    leafletProxy("map") %>%
      clearGroup("raster") %>%
      addRasterImage(rv$user_raster, opacity = input$raster_alpha, group = "raster")
  })

  output$data_summary <- renderUI({
    counts <- c(
      if (!is.null(rv$drawn_features))  paste0(nrow(rv$drawn_features), " drawn feature(s)") else NULL,
      if (!is.null(rv$user_points))     paste0(nrow(rv$user_points), " CSV point(s)") else NULL,
      if (!is.null(rv$user_polygons))   paste0(nrow(rv$user_polygons), " polygon feature(s)") else NULL,
      if (!is.null(rv$user_raster))     "1 raster layer" else NULL
    )
    if (length(counts) == 0)
      return(tags$p(style = "font-size:11px;color:#aaa;margin:4px 0;", "Nothing loaded yet."))
    tagList(lapply(counts, function(c)
      tags$div(style = "font-size:11px;color:#1a5c3a;padding:2px 0;", HTML(paste0("\u2022 ", c)))
    ))
  })

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

      make_swatch <- function(id, hex) {
        list(
          tags$div(id = paste0(id, "_preview"),
                   style = paste0("width:20px;height:20px;border-radius:3px;background:", hex,
                                  ";border:1px solid #ccc;flex-shrink:0;")),
          tags$input(id = id, type = "text", value = hex, placeholder = "#RRGGBB",
                     style = "flex:1;padding:3px 6px;border:1px solid #ddd;border-radius:4px;font-size:11px;",
                     oninput = paste0("document.getElementById('", id, "_preview').style.background=this.value;",
                                      "Shiny.setInputValue('", id, "',this.value)"))
        )
      }

      if (def$type == "polygon") {
        tags$div(style = "margin-bottom:8px;padding:6px;background:#fafafa;border-radius:5px;border:1px solid #eee;",
          tags$div(style = "font-size:10px;font-weight:700;color:#1a5c3a;margin-bottom:4px;", def$label),
          fluidRow(
            column(6,
              tags$label("Fill", style = "font-size:10px;color:#666;"),
              tags$div(style = "display:flex;align-items:center;gap:5px;",
                       make_swatch(paste0(f, "_fill"), def$fill))
            ),
            column(6,
              tags$label("Outline", style = "font-size:10px;color:#666;"),
              tags$div(style = "display:flex;align-items:center;gap:5px;",
                       make_swatch(paste0(f, "_border"), def$border))
            )
          )
        )
      } else {
        tags$div(style = "margin-bottom:8px;padding:6px;background:#fafafa;border-radius:5px;border:1px solid #eee;",
          tags$div(style = "font-size:10px;font-weight:700;color:#1a5c3a;margin-bottom:4px;", def$label),
          tags$div(style = "display:flex;align-items:center;gap:5px;",
                   make_swatch(paste0(f, "_color"), def$color))
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
      showNotification("Code not found or expired. Codes are valid for 30 days.",
                       type = "error", duration = 5)
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
    if (!is.null(params$map_name) && nzchar(params$map_name))
      updateTextInput(session, "map_name", value = params$map_name)
    if (!is.null(params$map_desc) && nzchar(params$map_desc))
      updateTextAreaInput(session, "map_desc", value = params$map_desc)

    # Lock the frame and set zoom to the stored display zoom (not the PDF zoom)
    updateCheckboxInput(session, "fixframe", value = TRUE)
    display_zoom <- min(as.integer(params$display_zoom %||% 14), 16)
    leafletProxy("map") %>%
      setView(lng = params$longitude, lat = params$latitude, zoom = display_zoom)

    showNotification(
      paste0("Map restored! The frame is now locked to your saved settings. ",
             "Uncheck 'Lock frame' to navigate freely."),
      type = "message", duration = 6
    )
  })

  # -----------------------------------------------------------
  # Community gallery
  # -----------------------------------------------------------

  output$gallery_ui <- renderUI({
    gallery <- getGallery()
    count   <- getMapCount()

    stat_box <- tags$div(
      style = "text-align:center;padding:20px 0 10px;",
      tags$div(style = "font-size:2.5em;font-weight:800;color:#1a5c3a;",
               format(count, big.mark = ",")),
      tags$div(style = "font-size:14px;color:#666;", "maps generated with BarrioMap")
    )

    if (length(gallery) == 0)
      return(tagList(stat_box,
                     tags$p(style = "text-align:center;color:#aaa;margin-top:20px;font-size:14px;",
                            "No shared maps yet. Download a map and check 'Share to gallery'.")))

    cards <- lapply(gallery, function(e) {
      lat_lbl <- sprintf("%.3f%s %s", abs(e$lat), "\u00b0", if (e$lat >= 0) "N" else "S")
      lon_lbl <- sprintf("%.3f%s %s", abs(e$lon), "\u00b0", if (e$lon >= 0) "E" else "W")
      tags$div(class = "bm-gallery-card",
        tags$div(class = "bm-gc-code", e$code),
        tags$div(class = "bm-gc-info",
          tags$span(paste0(lat_lbl, ", ", lon_lbl)),
          tags$span(style = "color:#1a5c3a;",
                    paste0("1:", format(as.numeric(e$scale), big.mark = ","))),
          tags$span(style = "color:#aaa;font-size:10px;", e$date)
        )
      )
    })

    tagList(stat_box,
            tags$h5(style = "color:#333;margin:20px 0 12px;font-weight:600;",
                    "Recently shared maps"),
            tags$div(class = "bm-gallery", tagList(cards)))
  })

  # -----------------------------------------------------------
  # PDF + data bundle export (two-step: generate then download)
  #
  # Step 1 runs inside observeEvent — over WebSocket, no HTTP
  # timeout. Step 2 is a downloadHandler that copies the
  # pre-built file in milliseconds, well within any timeout.
  # -----------------------------------------------------------

  # Exposes rv$export_path to conditionalPanel in ui.R.
  output$pdf_ready <- reactive({
    !is.null(rv$export_path) && file.exists(rv$export_path)
  })
  outputOptions(output, "pdf_ready", suspendWhenHidden = FALSE)

  # Clean up the export file when the session ends.
  session$onSessionEnded(function() {
    p <- isolate(rv$export_path)
    if (!is.null(p) && file.exists(p)) file.remove(p)
  })

  # STEP 1: Generate - all slow work happens here, over WebSocket.
  observeEvent(input$generate_btn, {

      req(rv$rects)
      rects     <- rv$rects
      is_single <- (rv$hpages == 1 && rv$vpages == 1)
      n_panels  <- nrow(rects)

      total_steps <- 2 + (if (!is_single) 2 else 0) + 2 * n_panels +
                     (if (isTRUE(input$show_legend)) 1 else 0) + 2
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
      lat_lbl  <- sprintf("%.4f%s %s", abs(ctr_lat), "\u00b0", if (ctr_lat >= 0) "N" else "S")
      lng_lbl  <- sprintf("%.4f%s %s", abs(ctr_lng), "\u00b0", if (ctr_lng >= 0) "E" else "W")

      map_code   <- generateMapCode()
      map_name   <- trimws(input$map_name %||% "")
      map_desc   <- trimws(input$map_desc %||% "")
      APP_URL    <- "datadiversitylab.github.io/barriomap"
      footer     <- paste0(APP_URL, "  |  OpenStreetMap (ODbL)  |  ",
                           format(Sys.Date(), "%B %d, %Y"), "  |  Code: ", map_code)

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

      # Capture user data print preferences
      print_drawn    <- isTRUE(input$print_drawn)
      print_points   <- isTRUE(input$print_points)
      print_polygons <- isTRUE(input$print_polygons)

      # ggplot builder
      buildPlot <- function(features_data, xlim, ylim, title,
                             panel_outline_sf = NULL, show_pnums = FALSE, pcenters_sf = NULL) {
        p <- ggplot()

        draw_order <- c(
          names(features_data)[sapply(names(features_data), function(f) !is.null(LAYER_DEFS[[f]]) && LAYER_DEFS[[f]]$type == "polygon")],
          names(features_data)[sapply(names(features_data), function(f) !is.null(LAYER_DEFS[[f]]) && LAYER_DEFS[[f]]$type == "line")],
          names(features_data)[sapply(names(features_data), function(f) !is.null(LAYER_DEFS[[f]]) && LAYER_DEFS[[f]]$type == "point")]
        )
        for (f in draw_order) {
          data <- features_data[[f]]
          def  <- LAYER_DEFS[[f]]
          if (is.null(data) || nrow(data) == 0) next
          if (def$type == "polygon") {
            p <- p + geom_sf(data = data,
                             fill  = layer_colors[[paste0(f,"_fill")]]   %||% def$fill,
                             color = layer_colors[[paste0(f,"_border")]] %||% def$border,
                             linewidth = def$lwd, alpha = def$alpha)
          } else if (def$type == "line") {
            p <- p + geom_sf(data = data,
                             color = layer_colors[[paste0(f,"_color")]] %||% def$color,
                             linewidth = def$lwd, alpha = def$alpha)
          } else {
            p <- p + geom_sf(data = data,
                             color = layer_colors[[paste0(f,"_color")]] %||% def$color,
                             size = 1.5, alpha = def$alpha)
          }
        }

        # Drawn features
        if (print_drawn && !is.null(rv$drawn_features) && nrow(rv$drawn_features) > 0) {
          gt          <- as.character(sf::st_geometry_type(rv$drawn_features))
          drawn_polys <- rv$drawn_features[grepl("POLYGON", gt, fixed = TRUE), ]
          drawn_pts   <- rv$drawn_features[gt == "POINT", ]
          if (nrow(drawn_polys) > 0)
            p <- p + geom_sf(data = drawn_polys, fill = "#FF980050",
                             color = "#E65100", linewidth = 0.5)
          if (nrow(drawn_pts) > 0)
            p <- p + geom_sf(data = drawn_pts, color = "#E65100", size = 2)
          d_lbl <- rv$drawn_features[nzchar(rv$drawn_features$label), ]
          if (nrow(d_lbl) > 0)
            p <- p + geom_sf_text(data = d_lbl, aes(label = label),
                                  size = 2, color = "#E65100", fontface = "bold")
        }

        # Uploaded polygons
        if (print_polygons && !is.null(rv$user_polygons) && nrow(rv$user_polygons) > 0) {
          p <- p + geom_sf(data = rv$user_polygons, fill = "#FF980040",
                           color = "#E65100", linewidth = 0.5)
          p_lbl <- rv$user_polygons[nzchar(rv$user_polygons$label), ]
          if (nrow(p_lbl) > 0)
            p <- p + geom_sf_text(data = p_lbl, aes(label = label),
                                  size = 2, color = "#E65100")
        }

        # Uploaded points
        if (print_points && !is.null(rv$user_points) && nrow(rv$user_points) > 0) {
          p <- p + geom_sf(data = rv$user_points, color = "#E91E63", size = 2)
          q_lbl <- rv$user_points[nzchar(rv$user_points$label), ]
          if (nrow(q_lbl) > 0)
            p <- p + geom_sf_text(data = q_lbl, aes(label = label),
                                  size = 2, nudge_y = 0.0003, color = "#880E4F")
        }

        if (!is.null(panel_outline_sf))
          p <- p + geom_sf(data = panel_outline_sf, fill = NA,
                           color = "#cc2200", linewidth = 0.7)
        if (show_pnums && !is.null(pcenters_sf))
          p <- p + geom_sf_text(data = pcenters_sf, aes(label = label),
                                size = 3, color = "#cc2200", fontface = "bold")

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
          theme(legend.position  = "none",
                panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.2),
                panel.grid.minor = element_blank(),
                panel.border     = element_rect(fill = NA, color = "#444", linewidth = 0.4),
                axis.title       = element_blank(),
                plot.title       = element_text(face = "bold", size = 10, hjust = 0.5,
                                                margin = margin(6,0,3,0)),
                plot.caption     = element_text(size = 5, color = "#aaa", hjust = 0.5,
                                                margin = margin(4,0,0,0)),
                plot.margin      = margin(5,5,5,5),
                plot.background  = element_rect(fill = "white", color = NA)) +
          axis_theme
      }

      # COVER PAGE
      progress$inc(amount = 1, detail = "Building the cover page")

      panel_lines <- if (is_single) {
        sprintf("Area: %.4f to %.4f E/W,  %.4f to %.4f N/S",
                min_lng, max_lng, min_lat, max_lat)
      } else {
        paste(sapply(seq_len(n_panels), function(i) {
          row_i <- floor((i-1)/rv$hpages)+1; col_i <- ((i-1) %% rv$hpages)+1
          bb    <- rects[i,]
          sprintf("Pg %d - Panel (%d,%d): %.3f, %.3f to %.3f, %.3f",
                  i+1, row_i, col_i, bb[1], bb[3], bb[2], bb[4])
        }), collapse = "\n")
      }
      panel_line_list <- strsplit(panel_lines, "\n")[[1]]

      cover_green <- "#1a5c3a"
      name_y      <- 0.83
      desc_y      <- if (nchar(map_desc) > 0) 0.775 else NULL
      info_start  <- if (nchar(map_name) > 0 && nchar(map_desc) > 0) 0.74
                     else if (nchar(map_name) > 0) 0.78
                     else 0.82

      coverPlot <- ggplot() +
        annotate("rect", xmin=0, xmax=1, ymin=0.87, ymax=1, fill=cover_green, color=NA) +
        annotate("text", x=0.5, y=0.945, label="BarrioMap",
                 color="white", size=16, fontface="bold", hjust=0.5) +
        annotate("text", x=0.5, y=0.893, label="Open-source mapping for communities",
                 color="#a8d5ba", size=4.2, hjust=0.5)

      if (nchar(map_name) > 0)
        coverPlot <- coverPlot +
          annotate("text", x=0.5, y=name_y, label=map_name,
                   color="#1a5c3a", size=7, fontface="bold", hjust=0.5)

      if (!is.null(desc_y) && nchar(map_desc) > 0)
        coverPlot <- coverPlot +
          annotate("text", x=0.5, y=desc_y, label=map_desc,
                   color="#555", size=4, hjust=0.5)

      coverPlot <- coverPlot +
        annotate("segment", x=0.08, xend=0.92, y=info_start, yend=info_start,
                 color="#ddd", linewidth=0.4) +
        annotate("text", x=0.08, y=info_start-0.035, label="Location",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.08, y=info_start-0.065, label=paste0(lat_lbl, ",  ", lng_lbl),
                 color="#222", size=4.5, hjust=0) +
        annotate("text", x=0.58, y=info_start-0.035, label="Scale",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.58, y=info_start-0.065,
                 label=paste0("1:", format(as.numeric(rv$scale), big.mark=",")),
                 color="#222", size=4.5, hjust=0) +
        annotate("text", x=0.08, y=info_start-0.105, label="Generated",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.08, y=info_start-0.135,
                 label=format(Sys.time(), "%B %d, %Y at %I:%M %p"),
                 color="#222", size=4, hjust=0) +
        annotate("text", x=0.58, y=info_start-0.105, label="Page",
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text", x=0.58, y=info_start-0.135,
                 label=paste0(toupper(rv$page), " | ", rv$dpi, " DPI"),
                 color="#222", size=4, hjust=0) +
        annotate("segment", x=0.08, xend=0.92, y=info_start-0.165, yend=info_start-0.165,
                 color="#ddd", linewidth=0.4) +
        annotate("text", x=0.5, y=info_start-0.195, label="Map code",
                 color="#888", size=3.2, hjust=0.5, fontface="bold") +
        annotate("rect", xmin=0.27, xmax=0.73, ymin=info_start-0.255, ymax=info_start-0.205,
                 fill="#f0f8f4", color=cover_green, linewidth=0.7) +
        annotate("text", x=0.5, y=info_start-0.228, label=map_code,
                 color=cover_green, size=12, fontface="bold", hjust=0.5) +
        annotate("text", x=0.5, y=info_start-0.275,
                 label=paste0("Enter at ", APP_URL, " to restore this map for 30 days."),
                 color="#555", size=3, hjust=0.5) +
        annotate("segment", x=0.08, xend=0.92,
                 y=info_start-0.300, yend=info_start-0.300, color="#ddd", linewidth=0.4) +
        annotate("text", x=0.08, y=info_start-0.322,
                 label=if (is_single) "Area" else paste0(n_panels, " panels"),
                 color="#888", size=3.2, hjust=0, fontface="bold") +
        annotate("text",
                 x     = rep(0.08, length(panel_line_list)),
                 y     = info_start - 0.343 - 0.020 * (seq_along(panel_line_list) - 1),
                 label = panel_line_list, color="#333", size=2.8, hjust=0) +
        annotate("segment", x=0, xend=1, y=0.09, yend=0.09, color="#ddd", linewidth=0.3) +
        annotate("text", x=0.5, y=0.05,
                 label=paste0(APP_URL, "  |  Data from OpenStreetMap (ODbL)"),
                 color="#bbb", size=2.8, hjust=0.5, fontface="italic") +
        xlim(0,1) + ylim(0,1) + theme_void() +
        theme(plot.background = element_rect(fill="white", color=NA),
              plot.margin = margin(0,0,0,0))

      ggsave(file.path(export_dir,"cover.pdf"), plot=coverPlot,
             device="pdf", width=width_in, height=height_in, units="in")

      # FETCH OSM DATA
      progress$inc(amount = 1,
                   detail = "Fetching map data (first visit to a region takes longer)")
      full_bbox    <- c(min_lng, min_lat, max_lng, max_lat)
      all_features <- tryCatch(
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
        }), crs=4326))
        centers_sf <- sf::st_sf(
          label    = sapply(seq_len(n_panels), function(i) {
            r <- floor((i-1)/rv$hpages)+1; co <- ((i-1)%%rv$hpages)+1
            paste0("(", r, ",", co, ")\nPg ", i+1)
          }),
          geometry = sf::st_sfc(lapply(seq_len(n_panels), function(i) {
            bb <- rects[i,]
            sf::st_point(c((bb[1]+bb[2])/2, (bb[3]+bb[4])/2))
          }), crs=4326)
        )
        ov <- buildPlot(all_features, c(min_lng,max_lng), c(min_lat,max_lat),
                        "Overview - all panels",
                        panel_outline_sf=panels_sf, show_pnums=TRUE, pcenters_sf=centers_sf)
        ggsave(file.path(export_dir,"overview.pdf"), plot=ov, device="pdf",
               width=width_in, height=height_in, units="in")
      }

      # PANEL PAGES
      panel_files <- character(0)
      for (i in seq_len(n_panels)) {
        bb    <- c(rects[i,1], rects[i,3], rects[i,2], rects[i,4])
        row_i <- floor((i-1)/rv$hpages)+1; col_i <- ((i-1)%%rv$hpages)+1

        progress$inc(amount=1, detail=paste0("Clipping data for panel ", i, " of ", n_panels))
        bbox_sfc <- sf::st_as_sfc(sf::st_bbox(c(xmin=bb[1],ymin=bb[2],xmax=bb[3],ymax=bb[4]), crs=4326))
        panel_features <- lapply(all_features, function(d) {
          if (is.null(d) || nrow(d) == 0) return(NULL)
          tryCatch(suppressWarnings(sf::st_crop(d, bbox_sfc)), error = function(e) d)
        })

        progress$inc(amount=1, detail=paste0("Drawing panel ", i, " of ", n_panels))
        title_str <- if (is_single) {
          paste0("1:", format(as.numeric(rv$scale), big.mark=","),
                 "  |  ", lat_lbl, ", ", lng_lbl)
        } else {
          paste0("Panel (", row_i, ",", col_i, ")  -  Page ", i+1)
        }
        pp <- buildPlot(panel_features, c(bb[1],bb[3]), c(bb[2],bb[4]), title_str)
        pdf_path <- file.path(export_dir, paste0("panel_",i,".pdf"))
        ggsave(pdf_path, plot=pp, device="pdf", width=width_in, height=height_in, units="in")
        panel_files <- c(panel_files, pdf_path)
      }

      # LEGEND PAGE
      legend_file     <- character(0)
      active_features <- names(all_features)[!sapply(all_features, is.null)]
      if (isTRUE(input$show_legend) && length(active_features) > 0) {
        progress$inc(amount=1, detail="Building the legend")
        n_items <- length(active_features)
        y_vals  <- seq(0.66, by=-0.09, length.out=n_items)
        lp <- ggplot() +
          annotate("rect", xmin=0, xmax=1, ymin=0.85, ymax=1, fill=cover_green, color=NA) +
          annotate("text", x=0.5, y=0.921, label="Map Legend",
                   color="white", size=10, fontface="bold", hjust=0.5)
        for (i in seq_along(active_features)) {
          f <- active_features[i]; def <- LAYER_DEFS[[f]]; y <- y_vals[i]
          if (def$type == "polygon") {
            fc <- layer_colors[[paste0(f,"_fill")]] %||% def$fill
            bc <- layer_colors[[paste0(f,"_border")]] %||% def$border
            lp <- lp + annotate("rect", xmin=0.08, xmax=0.17,
                                ymin=y-0.025, ymax=y+0.025, fill=fc, color=bc, linewidth=0.4)
          } else if (def$type == "line") {
            lc <- layer_colors[[paste0(f,"_color")]] %||% def$color
            lp <- lp + annotate("segment", x=0.08, xend=0.17, y=y, yend=y,
                                color=lc, linewidth=2)
          } else {
            pc <- layer_colors[[paste0(f,"_color")]] %||% def$color
            lp <- lp + annotate("point", x=0.125, y=y, color=pc, size=3)
          }
          lp <- lp + annotate("text", x=0.21, y=y, label=def$label,
                               hjust=0, size=5, color="#222")
        }
        lp <- lp +
          annotate("text", x=0.5, y=0.25,
                   label="Data: OpenStreetMap contributors (ODbL)",
                   size=3.5, color="#888", hjust=0.5) +
          annotate("text", x=0.5, y=0.17, label=paste0("Map code: ", map_code),
                   size=5, color=cover_green, fontface="bold", hjust=0.5) +
          annotate("text", x=0.5, y=0.09, label=APP_URL,
                   size=3.5, color="#bbb", hjust=0.5, fontface="italic") +
          xlim(0,1) + ylim(0,1) + theme_void() +
          theme(plot.background = element_rect(fill="white", color=NA))
        legend_path <- file.path(export_dir, "legend.pdf")
        ggsave(legend_path, plot=lp, device="pdf", width=width_in, height=height_in, units="in")
        legend_file <- legend_path
      }

      # MERGE PDF
      progress$inc(amount=1, detail="Assembling the PDF")
      ov_file   <- if (!is_single) file.path(export_dir,"overview.pdf") else character(0)
      tmp_files <- c(file.path(export_dir,"cover.pdf"), ov_file, panel_files, legend_file)
      merged    <- file.path(export_dir, "barrio.pdf")
      qpdf::pdf_combine(input=tmp_files, output=merged)
      if (!file.exists(merged) || file.info(merged)$size == 0)
        stop(safeError("PDF export did not complete. Please try again."))

      # SAVE MAP CODE
      saveMapCode(map_code, list(
        latitude     = rv$latitude,    longitude    = rv$longitude,
        scale        = rv$scale,       pageH        = rv$pageH,
        pageW        = rv$pageW,       page         = rv$page,
        vpages       = rv$vpages,      hpages       = rv$hpages,
        dpi          = rv$dpi,         features     = features_list,
        show_north   = rv$show_north,  show_scale   = rv$show_scale,
        show_coords  = rv$show_coords, show_legend  = rv$show_legend,
        map_name     = map_name,       map_desc     = map_desc,
        display_zoom = min(as.integer(input$map_zoom %||% 14), 16)
      ))
      incrementMapCount()
      if (isTRUE(input$share_to_gallery))
        addToGallery(map_code, rv$latitude, rv$longitude, rv$scale,
                     format(Sys.Date(), "%Y-%m-%d"))

      # Save to a permanent path (outside export_dir cleanup scope).
      progress$inc(amount = 1, detail = "Preparing your download")

      has_data <- (!is.null(rv$drawn_features) && nrow(rv$drawn_features) > 0) ||
                  !is.null(rv$user_points) ||
                  !is.null(rv$user_polygons)
      out_ext  <- if (has_data) ".zip" else ".pdf"
      out_path <- tempfile("barrio_ready_", fileext = out_ext)

      if (!has_data) {
        file.copy(merged, out_path, overwrite = TRUE)

      } else {
        bundle_dir <- tempfile("bm_bundle_")
        dir.create(bundle_dir)
        on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)

        file.copy(merged, file.path(bundle_dir, "barrio.pdf"))

        if (!is.null(rv$drawn_features) && nrow(rv$drawn_features) > 0)
          write_safe_geojson(rv$drawn_features, file.path(bundle_dir, "drawn.geojson"))
        if (!is.null(rv$user_points))
          write_safe_geojson(rv$user_points,    file.path(bundle_dir, "points.geojson"))
        if (!is.null(rv$user_polygons))
          write_safe_geojson(rv$user_polygons,  file.path(bundle_dir, "polygons.geojson"))

        writeLines(jsonlite::toJSON(list(
          map_name  = map_name,    map_desc  = map_desc,
          map_code  = map_code,    generated = format(Sys.time()),
          latitude  = rv$latitude, longitude = rv$longitude,
          scale     = rv$scale,    features  = features_list,
          tool      = "BarrioMap", url       = APP_URL
        ), auto_unbox = TRUE, pretty = TRUE), file.path(bundle_dir, "metadata.json"))

        writeLines(c(
          "BarrioMap Export Bundle",
          "=======================",
          paste("Map name:", if (nchar(map_name) > 0) map_name else "(untitled)"),
          paste("Code:    ", map_code),
          paste("Date:    ", format(Sys.time())),
          "",
          "Files:",
          "  barrio.pdf        - The printable map PDF",
          "  drawn.geojson     - Features drawn on the map (if any)",
          "  points.geojson    - Uploaded CSV points (if any)",
          "  polygons.geojson  - Uploaded shapefile polygons (if any)",
          "  metadata.json     - Map settings",
          "",
          paste("Restore at:", APP_URL, "using code", map_code),
          "Data from OpenStreetMap (c) contributors, ODbL license."
        ), file.path(bundle_dir, "README.txt"))

        utils::zip(zipfile = out_path,
                   files   = list.files(bundle_dir, full.names = TRUE),
                   flags   = "-j")
      }

      # Store the ready file; clean up any previous export.
      old_path <- isolate(rv$export_path)
      if (!is.null(old_path) && file.exists(old_path)) file.remove(old_path)
      rv$export_path <- out_path
      rv$export_ext  <- out_ext

      # SHOW CODE MODAL
      showModal(modalDialog(
        title = NULL, footer = modalButton("Got it!"), easyClose = TRUE,
        tags$div(style = "text-align:center;padding:10px 20px 20px;",
          tags$div(style = "font-size:12px;color:#888;margin-bottom:8px;",
                   "Your map is ready - save this code"),
          tags$div(style = paste0("font-size:2.2em;font-weight:800;letter-spacing:6px;",
                                  "color:#1a5c3a;padding:14px 20px;background:#f0f8f4;",
                                  "border-radius:8px;border:2px solid #1a5c3a;",
                                  "display:inline-block;"),
                   map_code),
          tags$p(style = "color:#555;font-size:13px;margin-top:10px;",
                 paste0("Enter at ", APP_URL, " to restore this map for 30 days.")),
          tags$p(style = "color:#888;font-size:11px;",
                 "Click the Download button in the sidebar to save your file.")
        )
      ))
    }
  )

  # STEP 2: Download - instant file copy, no timeout risk.
  output$download_pdf <- downloadHandler(
    filename = function() {
      nm   <- trimws(input$map_name %||% "")
      base <- if (nchar(nm) > 0) gsub("[^a-zA-Z0-9_-]", "_", nm) else "BarrioMap"
      paste0(base, "_", format(Sys.Date(), "%Y%m%d"), isolate(rv$export_ext) %||% ".pdf")
    },
    content = function(file) {
      req(!is.null(rv$export_path), file.exists(rv$export_path))
      file.copy(rv$export_path, file, overwrite = TRUE)
    }
  )
}
