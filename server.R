###################
# server.R
#
# Server controller.
###################
library(dplyr)
library(leaflet)
library(ggplot2)
library(osmdata)
library(osmextract)
library(sf)
library(ggspatial)
library(jsonlite)

source('functions/functions.R')

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    latitude     = 32.2540,
    longitude    = -110.9742,
    pageH        = 0.267,
    pageW        = 0.18,
    vpages       = 1,
    hpages       = 1,
    scale        = 5840,
    page         = "a4",
    usecoordinates = TRUE,
    dpi          = 300,
    rects        = NULL,
    roads_color  = "#555555",
    bld_fill     = "#f2f2f2",
    bld_border   = "#aaaaaa",
    show_north   = TRUE,
    show_scale   = TRUE,
    show_coords  = TRUE,
    show_legend  = TRUE
  )

  # Update reactive values when UI elements change
  observeEvent(input$latitude,       { rv$latitude       <- input$latitude       })
  observeEvent(input$longitude,      { rv$longitude      <- input$longitude      })
  observeEvent(input$pageH,          { rv$pageH          <- input$pageH          })
  observeEvent(input$pageW,          { rv$pageW          <- input$pageW          })
  observeEvent(input$vpages,         { rv$vpages         <- input$vpages         })
  observeEvent(input$hpages,         { rv$hpages         <- input$hpages         })
  observeEvent(input$page,           { rv$page           <- input$page           })
  observeEvent(input$orientation,    { rv$orientation    <- input$orientation    })
  observeEvent(input$scale,          { rv$scale          <- input$scale          })
  observeEvent(input$usecoordinates, { rv$usecoordinates <- input$usecoordinates })
  observeEvent(input$dpi,            { rv$dpi            <- input$dpi            })
  observeEvent(input$roads_color,    { rv$roads_color    <- input$roads_color    })
  observeEvent(input$bld_fill,       { rv$bld_fill       <- input$bld_fill       })
  observeEvent(input$bld_border,     { rv$bld_border     <- input$bld_border     })
  observeEvent(input$show_north,     { rv$show_north     <- input$show_north     })
  observeEvent(input$show_scale,     { rv$show_scale     <- input$show_scale     })
  observeEvent(input$show_coords,    { rv$show_coords    <- input$show_coords    })
  observeEvent(input$show_legend,    { rv$show_legend    <- input$show_legend    })

  # Render the initial map
  output$map <- leaflet::renderLeaflet({
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
  })

  # Hide/show coordinate inputs
  observeEvent(input$usecoordinates, {
    if (input$usecoordinates == FALSE) {
      shinyjs::hide("longitude")
      shinyjs::hide("latitude")
    } else {
      shinyjs::show("longitude")
      shinyjs::show("latitude")
    }
  })

  # Geocode search box
  observeEvent(input$searchbox, {
    req(!input$usecoordinates)
    req(nzchar(input$searchbox))
    coords <- tmaptools::geocode_OSM(q = input$searchbox)
    if (length(coords) > 0) {
      lon <- as.numeric(coords[[2]][1])
      lat <- as.numeric(coords[[2]][2])
      leafletProxy("map") %>% setView(lng = lon, lat = lat, zoom = 10)
    } else {
      showNotification("Could not find that location. Try a different search term.")
    }
  })

  # Hide/show page size inputs
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

  # Set default page dimensions for A4 / A3
  observeEvent(list(input$page, input$orientation), {
    if (input$page == "a4") {
      if (input$orientation == "v") { rv$pageH <- 0.267; rv$pageW <- 0.18
      } else                        { rv$pageH <- 0.18;  rv$pageW <- 0.267 }
    } else if (input$page == "a3") {
      if (input$orientation == "v") { rv$pageH <- 0.420; rv$pageW <- 0.297
      } else                        { rv$pageH <- 0.297; rv$pageW <- 0.420 }
    } else {
      rv$pageH <- input$pageH
      rv$pageW <- input$pageW
    }
  })

  # Sync map center to coordinate inputs when the user pans
  observeEvent(input$map_center, {
    req(!input$fixframe)
    updateNumericInput(session, "longitude", value = input$map_center$lng)
    updateNumericInput(session, "latitude",  value = input$map_center$lat)
  })

  # First observer: compute and draw rectangles
  observe({
    zl <- calcZoom(
      scale_meters_per_inch = as.numeric(rv$scale),
      lat = rv$latitude,
      dpi = as.numeric(rv$dpi)
    )
    mbox_scale <- as.numeric(rv$scale)
    pixel_v <- meter2screenpixel(rv$pageH * mbox_scale, orient = "v", zl, rv$latitude)
    pixel_h <- meter2screenpixel(rv$pageW * mbox_scale, orient = "h", zl, rv$latitude)

    recMap <- leaflet(width = pixel_h, height = pixel_v) %>%
      addTiles() %>%
      setView(lng = rv$longitude, lat = rv$latitude, zoom = zl)

    rv$rects <- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)

    leafletProxy("map") %>%
      clearShapes() %>%
      {
        for (i in 1:nrow(rv$rects)) {
          addRectangles(., lng1 = rv$rects[i, 1], lat1 = rv$rects[i, 3],
                        lng2 = rv$rects[i, 2], lat2 = rv$rects[i, 4],
                        fillColor = "transparent")
        }
      }
  })

  # Second observer: update rectangles if map view changes
  observe({
    zl <- calcZoom(
      scale_meters_per_inch = as.numeric(rv$scale),
      lat = rv$latitude,
      dpi = 300
    )
    mbox_scale <- as.numeric(rv$scale)
    pixel_v <- meter2screenpixel(rv$pageH * mbox_scale, orient = "v", zl, rv$latitude)
    pixel_h <- meter2screenpixel(rv$pageW * mbox_scale, orient = "h", zl, rv$latitude)

    recMap <- leaflet(width = pixel_h, height = pixel_v) %>%
      addTiles() %>%
      setView(lng = rv$longitude, lat = rv$latitude, zoom = zl)

    rv$rects <- returnRectangles(map = recMap, nRecLon = rv$hpages, nRecVert = rv$vpages)

    proxy <- leafletProxy("map") %>% clearShapes()
    for (i in 1:nrow(rv$rects)) {
      proxy %>% addRectangles(
        lng1 = rv$rects[i, 1], lat1 = rv$rects[i, 3],
        lng2 = rv$rects[i, 2], lat2 = rv$rects[i, 4],
        fillColor = "transparent"
      )
    }
  })

  # Restore map settings from a code
  observeEvent(input$restore_map_btn, {
    req(nzchar(trimws(input$map_code_input)))
    params <- loadMapCode(input$map_code_input)
    if (is.null(params)) {
      showNotification(
        "Code not found or expired. Codes are valid for 30 days.",
        type = "error", duration = 5
      )
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
    if (!is.null(params$roads_color))
      updateSelectInput(session, "roads_color", selected = params$roads_color)
    if (!is.null(params$bld_fill))
      updateSelectInput(session, "bld_fill",    selected = params$bld_fill)
    if (!is.null(params$bld_border))
      updateSelectInput(session, "bld_border",  selected = params$bld_border)
    if (!is.null(params$show_north))
      updateCheckboxInput(session, "show_north",  value = params$show_north)
    if (!is.null(params$show_scale))
      updateCheckboxInput(session, "show_scale",  value = params$show_scale)
    if (!is.null(params$show_coords))
      updateCheckboxInput(session, "show_coords", value = params$show_coords)
    if (!is.null(params$show_legend))
      updateCheckboxInput(session, "show_legend", value = params$show_legend)

    leafletProxy("map") %>%
      setView(lng = params$longitude, lat = params$latitude, zoom = 10)
    showNotification("Map restored!", type = "message", duration = 3)
  })

  # Download Handler
  output$print <- downloadHandler(
    filename = function() { "barrio.pdf" },
    content  = function(file) {

      req(rv$rects)
      rects <- rv$rects

      # A single panel (1x1) skips the overview page.
      is_single <- (rv$hpages == 1 && rv$vpages == 1)

      # Progress: cover + overview data + overview draw (skip if single) +
      # 2 per panel + legend (if shown) + merge
      n_panels    <- nrow(rects)
      total_steps <- 2 + (if (!is_single) 2 else 0) + 2 * n_panels +
                     (if (isTRUE(input$show_legend)) 1 else 0) + 1
      progress <- shiny::Progress$new(max = total_steps)
      progress$set(message = "Generating your map", value = 0)
      on.exit(progress$close(),                          add = TRUE)

      export_dir <- tempfile("barrio_export_")
      dir.create(export_dir)
      on.exit(unlink(export_dir, recursive = TRUE),      add = TRUE)

      width_in  <- rv$pageW * 39.3701
      height_in <- rv$pageH * 39.3701

      # Compute full extent coordinates (used on cover and overview)
      all_lng  <- c(rects[, 1], rects[, 2])
      all_lat  <- c(rects[, 3], rects[, 4])
      min_lng  <- min(all_lng);  max_lng <- max(all_lng)
      min_lat  <- min(all_lat);  max_lat <- max(all_lat)
      ctr_lat  <- (min_lat + max_lat) / 2
      ctr_lng  <- (min_lng + max_lng) / 2
      lat_lbl  <- sprintf("%.4f\u00b0 %s", abs(ctr_lat), ifelse(ctr_lat >= 0, "N", "S"))
      lng_lbl  <- sprintf("%.4f\u00b0 %s", abs(ctr_lng), ifelse(ctr_lng >= 0, "E", "W"))

      # Generate the map code now so it appears on the cover
      map_code <- generateMapCode()

      footer <- paste0(
        "barriomap.arizona.edu  \u00b7  Data: OpenStreetMap (ODbL)  \u00b7  ",
        format(Sys.Date(), "%B %d, %Y"),
        "  \u00b7  Code: ", map_code
      )

      # Helper: build a styled map ggplot
      buildMapPlot <- function(roads_sf, blds_sf, xlim, ylim, title,
                               panel_outline_sf = NULL,
                               show_panel_numbers = FALSE,
                               panel_centers_sf  = NULL) {
        axis_theme <- if (isTRUE(input$show_coords)) {
          theme(axis.text  = element_text(size = 6, color = "#555555"),
                axis.ticks = element_line(color = "#aaaaaa", linewidth = 0.3))
        } else {
          theme(axis.text  = element_blank(),
                axis.ticks = element_blank())
        }

        p <- ggplot() +
          (if (!is.null(roads_sf))
             geom_sf(data = roads_sf, color = rv$roads_color, linewidth = 0.35, alpha = 0.85)) +
          (if (!is.null(blds_sf))
             geom_sf(data = blds_sf, fill = rv$bld_fill, color = rv$bld_border,
                     linewidth = 0.15, alpha = 0.9)) +
          (if (!is.null(panel_outline_sf))
             geom_sf(data = panel_outline_sf, fill = NA, color = "#cc2200", linewidth = 0.7)) +
          (if (show_panel_numbers && !is.null(panel_centers_sf))
             geom_sf_text(data = panel_centers_sf, aes(label = label),
                          size = 3, color = "#cc2200", fontface = "bold")) +
          (if (isTRUE(input$show_scale))
             annotation_scale(location = "bl", width_hint = 0.25,
                              bar_cols = c("#333333", "#ffffff"),
                              text_cex = 0.65, line_col = "#333333")) +
          (if (isTRUE(input$show_north))
             annotation_north_arrow(
               location = "tr", which_north = "true",
               style    = north_arrow_nautical(
                 fill     = c("#333333", "#ffffff"),
                 line_col = "#333333",
                 text_col = "#333333"
               ),
               height = unit(1.1, "cm"), width = unit(1.1, "cm")
             )) +
          coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          labs(title = title, caption = footer) +
          theme_minimal(base_size = 9) +
          theme(
            legend.position  = "none",
            panel.grid.major = element_line(color = "#eeeeee", linewidth = 0.2),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(fill = NA, color = "#444444", linewidth = 0.4),
            axis.title       = element_blank(),
            plot.title       = element_text(face = "bold", size = 10, hjust = 0.5,
                                            margin = margin(6, 0, 3, 0)),
            plot.caption     = element_text(size = 5.5, color = "#999999", hjust = 0.5,
                                            margin = margin(4, 0, 0, 0)),
            plot.margin      = margin(5, 5, 5, 5),
            plot.background  = element_rect(fill = "white", color = NA)
          ) +
          axis_theme
        p
      }

      # PAGE 1: COVER
      progress$inc(amount = 1, detail = "Building the cover page")

      panel_lines <- if (is_single) {
        sprintf("Bounding box: %.4f, %.4f, %.4f, %.4f", min_lng, min_lat, max_lng, max_lat)
      } else {
        lines <- sapply(seq_len(n_panels), function(i) {
          row_i <- floor((i - 1) / rv$hpages) + 1
          col_i <- ((i - 1) %% rv$hpages) + 1
          bb    <- rects[i, ]
          sprintf("Page %d \u2014 Panel (%d,%d): %.3f, %.3f to %.3f, %.3f",
                  i + 1, row_i, col_i, bb[1], bb[3], bb[2], bb[4])
        })
        paste(lines, collapse = "\n")
      }

      # Cover page built with annotate calls for a clean, professional look
      cover_bg    <- "#1a5c3a"
      cover_light <- "#a8d5ba"

      coverPlot <- ggplot() +
        annotate("rect",  xmin = 0, xmax = 1, ymin = 0.87, ymax = 1,
                 fill = cover_bg, color = NA) +
        annotate("text",  x = 0.5, y = 0.945, label = "Barrio\u00a0Map",
                 color = "white", size = 16, fontface = "bold", hjust = 0.5) +
        annotate("text",  x = 0.5, y = 0.893,
                 label = "Open-source mapping for communities",
                 color = cover_light, size = 4.5, hjust = 0.5) +
        annotate("segment", x = 0.08, xend = 0.92, y = 0.84, yend = 0.84,
                 color = "#dddddd", linewidth = 0.4) +
        annotate("text",  x = 0.08, y = 0.805, label = "Location",
                 color = "#888888", size = 3.5, hjust = 0, fontface = "bold") +
        annotate("text",  x = 0.08, y = 0.772,
                 label = paste0(lat_lbl, ",  ", lng_lbl),
                 color = "#222222", size = 5, hjust = 0) +
        annotate("text",  x = 0.6, y = 0.805, label = "Scale",
                 color = "#888888", size = 3.5, hjust = 0, fontface = "bold") +
        annotate("text",  x = 0.6, y = 0.772,
                 label = paste0("1:", format(as.numeric(rv$scale), big.mark = ",")),
                 color = "#222222", size = 5, hjust = 0) +
        annotate("text",  x = 0.08, y = 0.730, label = "Generated",
                 color = "#888888", size = 3.5, hjust = 0, fontface = "bold") +
        annotate("text",  x = 0.08, y = 0.697,
                 label = format(Sys.time(), "%B %d, %Y at %I:%M %p"),
                 color = "#222222", size = 5, hjust = 0) +
        annotate("text",  x = 0.6, y = 0.730, label = "Page size",
                 color = "#888888", size = 3.5, hjust = 0, fontface = "bold") +
        annotate("text",  x = 0.6, y = 0.697,
                 label = paste0(toupper(rv$page), " \u00b7 ", rv$dpi, " DPI"),
                 color = "#222222", size = 5, hjust = 0) +
        annotate("segment", x = 0.08, xend = 0.92, y = 0.665, yend = 0.665,
                 color = "#dddddd", linewidth = 0.4) +
        annotate("text",  x = 0.5, y = 0.635, label = "Your map code",
                 color = "#888888", size = 3.5, hjust = 0.5, fontface = "bold") +
        annotate("rect",  xmin = 0.28, xmax = 0.72, ymin = 0.575, ymax = 0.625,
                 fill = "#f0f8f4", color = cover_bg, linewidth = 0.7) +
        annotate("text",  x = 0.5, y = 0.600, label = map_code,
                 color = cover_bg, size = 13, fontface = "bold", hjust = 0.5) +
        annotate("text",  x = 0.5, y = 0.550,
                 label = "Enter this code at barriomap.arizona.edu to restore this map.",
                 color = "#555555", size = 3.5, hjust = 0.5) +
        annotate("text",  x = 0.5, y = 0.527,
                 label = "Codes are valid for 30 days.",
                 color = "#888888", size = 3.2, hjust = 0.5) +
        annotate("segment", x = 0.08, xend = 0.92, y = 0.50, yend = 0.50,
                 color = "#dddddd", linewidth = 0.4) +
        annotate("text",  x = 0.08, y = 0.475,
                 label = if (is_single) "Area covered" else paste0("Panels (", n_panels, " total)"),
                 color = "#888888", size = 3.5, hjust = 0, fontface = "bold") +
        annotate("text",  x = 0.08, y = 0.455 - 0.018 * seq_len(length(strsplit(panel_lines, "\n")[[1]])),
                 label = strsplit(panel_lines, "\n")[[1]],
                 color = "#333333", size = 3, hjust = 0) +
        annotate("segment", x = 0, xend = 1, y = 0.09, yend = 0.09,
                 color = "#dddddd", linewidth = 0.3) +
        annotate("text",  x = 0.5, y = 0.05,
                 label = "barriomap.arizona.edu  \u00b7  Data from OpenStreetMap (ODbL)",
                 color = "#bbbbbb", size = 3, hjust = 0.5, fontface = "italic") +
        xlim(0, 1) + ylim(0, 1) +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = NA),
              plot.margin     = margin(0, 0, 0, 0))

      ggsave(file.path(export_dir, "cover.pdf"),
             plot = coverPlot, device = "pdf",
             width = width_in, height = height_in, units = "in")

      # FETCH OSM DATA (one call for the full extent, clip per panel later)
      progress$inc(amount = 1,
                   detail = "Fetching map data (first visit to a new region takes longer)")

      full_bbox     <- c(min_lng, min_lat, max_lng, max_lat)
      full_features <- tryCatch(
        getOsmFeatures(full_bbox, input$features),
        error = function(e) {
          stop(safeError(paste0(
            "Could not fetch map data (", conditionMessage(e), "). Please try again."
          )))
        }
      )
      roads_full <- full_features$roads
      blds_full  <- full_features$buildings

      # PAGE 2: OVERVIEW (only when multi-panel)
      if (!is_single) {
        progress$inc(amount = 1, detail = "Drawing the overview page")

        all_panels_sf <- lapply(seq_len(n_panels), function(i) {
          bb <- rects[i, ]
          st_polygon(list(matrix(c(
            bb[1], bb[3], bb[1], bb[4],
            bb[2], bb[4], bb[2], bb[3], bb[1], bb[3]
          ), ncol = 2, byrow = TRUE)))
        })
        overview_panels <- st_as_sf(st_sfc(all_panels_sf, crs = 4326))

        centers_list <- lapply(seq_len(n_panels), function(i) {
          bb <- rects[i, ]
          st_point(c((bb[1] + bb[2]) / 2, (bb[3] + bb[4]) / 2))
        })
        panelCenters_sf <- st_as_sf(
          data.frame(label = sapply(seq_len(n_panels), function(i) {
            row_i <- floor((i - 1) / rv$hpages) + 1
            col_i <- ((i - 1) %% rv$hpages) + 1
            paste0("(", row_i, ",", col_i, ")\nPg ", i + 1)
          })),
          geometry = st_sfc(centers_list, crs = 4326)
        )

        overviewPlot <- buildMapPlot(
          roads_sf          = roads_full,
          blds_sf           = blds_full,
          xlim              = c(min_lng, max_lng),
          ylim              = c(min_lat, max_lat),
          title             = "Overview — all panels",
          panel_outline_sf  = overview_panels,
          show_panel_numbers = TRUE,
          panel_centers_sf  = panelCenters_sf
        )

        ggsave(file.path(export_dir, "overview.pdf"),
               plot = overviewPlot, device = "pdf",
               width = width_in, height = height_in, units = "in")
      }

      # PANEL PAGES
      panel_files <- character(0)
      for (i in seq_len(n_panels)) {
        bb <- c(rects[i, 1], rects[i, 3], rects[i, 2], rects[i, 4])
        row_i <- floor((i - 1) / rv$hpages) + 1
        col_i <- ((i - 1) %% rv$hpages) + 1

        progress$inc(amount = 1, detail = paste0("Fetching data for panel ", i, " of ", n_panels))

        # Clip full-extent data to this panel bbox
        panel_bbox_sf <- st_as_sfc(st_bbox(
          c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4]), crs = 4326
        ))
        roads_sf <- if (!is.null(roads_full))
          suppressWarnings(st_crop(roads_full, panel_bbox_sf)) else NULL
        blds_sf  <- if (!is.null(blds_full))
          suppressWarnings(st_crop(blds_full,  panel_bbox_sf)) else NULL

        progress$inc(amount = 1, detail = paste0("Drawing panel ", i, " of ", n_panels))

        panel_title <- if (is_single) {
          paste0("1:", format(as.numeric(rv$scale), big.mark = ","),
                 "  \u00b7  ", lat_lbl, ", ", lng_lbl)
        } else {
          paste0("Panel (", row_i, ",", col_i, ")  \u2014  Page ", i + 1)
        }

        panelPlot <- buildMapPlot(
          roads_sf = roads_sf,
          blds_sf  = blds_sf,
          xlim     = c(bb[1], bb[3]),
          ylim     = c(bb[2], bb[4]),
          title    = panel_title
        )

        panel_pdf <- file.path(export_dir, paste0("panel_", i, ".pdf"))
        ggsave(panel_pdf, plot = panelPlot, device = "pdf",
               width = width_in, height = height_in, units = "in")
        panel_files <- c(panel_files, panel_pdf)
      }

      # LEGEND PAGE (optional)
      legend_file <- character(0)
      if (isTRUE(input$show_legend) && length(input$features) > 0) {
        progress$inc(amount = 1, detail = "Building the legend")

        legend_items <- data.frame(
          x     = rep(0.15, length(input$features)),
          y     = seq(0.65, by = -0.12, length.out = length(input$features)),
          fill  = c(if ("roads"     %in% input$features) rv$roads_color else NULL,
                    if ("buildings" %in% input$features) rv$bld_fill    else NULL),
          label = c(if ("roads"     %in% input$features) "Roads" else NULL,
                    if ("buildings" %in% input$features) "Buildings" else NULL)
        )

        legendPlot <- ggplot(legend_items) +
          annotate("rect",  xmin = 0, xmax = 1, ymin = 0.85, ymax = 1,
                   fill = "#1a5c3a", color = NA) +
          annotate("text",  x = 0.5, y = 0.92, label = "Map Legend",
                   color = "white", size = 10, fontface = "bold", hjust = 0.5) +
          geom_rect(aes(xmin = x - 0.06, xmax = x + 0.06,
                        ymin = y - 0.04, ymax = y + 0.04, fill = fill),
                    color = "#333333", linewidth = 0.3) +
          scale_fill_identity() +
          geom_text(aes(x = x + 0.12, y = y, label = label),
                    hjust = 0, size = 6, color = "#222222") +
          annotate("text",  x = 0.5, y = 0.35,
                   label = "Data source: OpenStreetMap contributors (ODbL)",
                   size = 4, color = "#888888", hjust = 0.5) +
          annotate("text",  x = 0.5, y = 0.28,
                   label = paste0("Map code: ", map_code),
                   size = 4.5, color = "#1a5c3a", fontface = "bold", hjust = 0.5) +
          annotate("text",  x = 0.5, y = 0.10,
                   label = "barriomap.arizona.edu",
                   size = 4, color = "#aaaaaa", hjust = 0.5, fontface = "italic") +
          xlim(0, 1) + ylim(0, 1) +
          theme_void() +
          theme(plot.background = element_rect(fill = "white", color = NA))

        legend_pdf <- file.path(export_dir, "legend.pdf")
        ggsave(legend_pdf, plot = legendPlot, device = "pdf",
               width = width_in, height = height_in, units = "in")
        legend_file <- legend_pdf
      }

      # MERGE ALL PAGES
      progress$inc(amount = 1, detail = "Putting the PDF together")

      overview_file <- if (!is_single) file.path(export_dir, "overview.pdf") else character(0)
      tmp_files <- c(
        file.path(export_dir, "cover.pdf"),
        overview_file,
        panel_files,
        legend_file
      )
      merged_pdf <- file.path(export_dir, "barrio_temp.pdf")
      qpdf::pdf_combine(input = tmp_files, output = merged_pdf)

      if (!file.exists(merged_pdf) || file.info(merged_pdf)$size == 0)
        stop(safeError("The PDF export did not complete. Please try again."))

      # SAVE MAP CODE AND SHOW MODAL
      saveMapCode(map_code, list(
        latitude    = rv$latitude,
        longitude   = rv$longitude,
        scale       = rv$scale,
        pageH       = rv$pageH,
        pageW       = rv$pageW,
        page        = rv$page,
        vpages      = rv$vpages,
        hpages      = rv$hpages,
        dpi         = rv$dpi,
        features    = input$features,
        roads_color = rv$roads_color,
        bld_fill    = rv$bld_fill,
        bld_border  = rv$bld_border,
        show_north  = rv$show_north,
        show_scale  = rv$show_scale,
        show_coords = rv$show_coords,
        show_legend = rv$show_legend
      ))

      file.copy(merged_pdf, file, overwrite = TRUE)

      showModal(modalDialog(
        title = NULL, footer = modalButton("Got it!"), easyClose = TRUE,
        tags$div(
          style = "text-align: center; padding: 10px 20px 20px;",
          tags$div(style = "font-size: 13px; color: #888; margin-bottom: 8px;",
                   "Your map is ready \u2014 save this code to come back to it"),
          tags$div(
            style = paste0(
              "font-size: 2.2em; font-weight: 800; letter-spacing: 6px;",
              " color: #1a5c3a; padding: 14px 20px;",
              " background: #f0f8f4; border-radius: 8px;",
              " border: 2px solid #1a5c3a; margin: 10px auto; display: inline-block;"
            ),
            map_code
          ),
          tags$p(style = "color: #555; font-size: 13px; margin-top: 10px;",
                 "Enter this at barriomap.arizona.edu to restore your exact map settings."),
          tags$p(style = "color: #aaa; font-size: 11px;", "Valid for 30 days.")
        )
      ))
    }
  )
}
