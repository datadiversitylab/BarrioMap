###################
# ui.R
###################
if (!exists("LAYER_DEFS")) source("functions/functions.R")

# Default selected layers
DEFAULT_FEATURES <- c("roads", "buildings")
LAYER_CHOICES    <- setNames(names(LAYER_DEFS), sapply(LAYER_DEFS, `[[`, "label"))

ui <- navbarPage(
  title    = tags$span(style = "font-weight:800;color:#1a5c3a;letter-spacing:-0.5px;", "Barrio\u00a0Map"),
  id       = "main_nav",
  selected = "Map",
  theme    = bslib::bs_theme(
    version      = 4,
    bootswatch   = "minty",
    primary      = "#1a5c3a",
    heading_font = bslib::font_google("Inter", wght = c(400, 600, 700))
  ),

  # ================================================================
  # MAP TAB
  # ================================================================
  tabPanel("Map", value = "Map",

    shinybusy::add_busy_spinner(spin = "dots", timeout = 300, height = "20px", width = "20px"),

    tags$head(
      tags$script(defer = NA,
        src = "https://umami.datadiversitylab.synology.me/script.js",
        `data-website-id` = "543cffef-a100-47af-9dd9-2cf39517077b",
        `data-domains`    = "datadiversitylab.github.io",
        `data-tag`        = "barriomap"
      ),
      tags$style(HTML("

        /* ---- Base ---- */
        body { font-family:'Inter','Helvetica Neue',Arial,sans-serif; margin:0; overflow:hidden; }
        .navbar { border-bottom:3px solid #1a5c3a; }

        /* ---- Full-height layout ---- */
        .bm-layout {
          display:flex;
          height:calc(100vh - 56px);
          overflow:hidden;
        }
        .bm-sidebar {
          width:300px;
          min-width:260px;
          flex-shrink:0;
          height:100%;
          overflow-y:auto;
          background:#fff;
          border-right:1px solid #e0e0e0;
          padding:0;
          box-shadow:2px 0 8px rgba(0,0,0,0.06);
        }
        .bm-sidebar::-webkit-scrollbar { width:4px; }
        .bm-sidebar::-webkit-scrollbar-thumb { background:#c8e6c9; border-radius:2px; }
        .bm-main {
          flex:1;
          position:relative;
          height:100%;
        }
        #map { height:100% !important; width:100% !important; }

        /* ---- Sidebar sections ---- */
        .bm-header {
          background:linear-gradient(135deg,#1a5c3a 0%,#2d7a50 100%);
          padding:18px 16px 14px;
          color:white;
        }
        .bm-header-title { font-size:17px; font-weight:700; margin:0 0 2px; }
        .bm-header-sub   { font-size:11px; color:#a8d5ba; margin:0 0 10px; }
        .bm-header-note  {
          background:rgba(255,255,255,0.12);
          border-radius:5px;
          padding:6px 10px;
          font-size:10.5px;
          color:#d0eeda;
          line-height:1.5;
        }

        .bm-section { border-bottom:1px solid #f0f0f0; }
        .bm-section-header {
          display:flex;
          align-items:center;
          justify-content:space-between;
          padding:10px 14px;
          cursor:pointer;
          user-select:none;
          background:#fafafa;
          transition:background 0.15s;
        }
        .bm-section-header:hover { background:#f0f8f4; }
        .bm-section-title { font-size:11.5px; font-weight:700; color:#1a5c3a; letter-spacing:0.3px; }
        .bm-chevron { color:#aaa; font-size:14px; transition:transform 0.2s; }
        .bm-section-header[aria-expanded='true'] .bm-chevron { transform:rotate(90deg); }
        .bm-section-body { padding:10px 14px 14px; }

        .form-group { margin-bottom:8px; }
        .form-group label { font-size:11px; color:#555; margin-bottom:2px; }
        .form-control { font-size:12px; padding:5px 8px; }
        .bm-hint { font-size:10px; color:#aaa; margin-top:3px; }

        /* ---- Download button ---- */
        #print {
          width:100%; background:#1a5c3a; border-color:#1a5c3a;
          color:white; font-weight:700; border-radius:6px;
          padding:9px 0; font-size:13px; margin-top:4px;
          letter-spacing:0.3px;
        }
        #print:hover { background:#155230; border-color:#155230; }

        /* ---- Action buttons ---- */
        .btn-bm-outline {
          width:100%; background:white; border:1.5px solid #1a5c3a;
          color:#1a5c3a; font-weight:600; border-radius:6px;
          padding:6px 0; font-size:12px; cursor:pointer;
          transition:all 0.15s;
        }
        .btn-bm-outline:hover { background:#f0f8f4; }
        .btn-bm-danger {
          width:100%; background:white; border:1.5px solid #e53935;
          color:#e53935; font-weight:600; border-radius:6px;
          padding:5px 0; font-size:11px; cursor:pointer; margin-top:4px;
        }

        /* ---- Map code ---- */
        .bm-code-section {
          background:#f0f8f4;
          border-top:2px solid #1a5c3a;
          padding:12px 14px;
        }
        .bm-code-label { font-size:10px; font-weight:700; color:#1a5c3a; letter-spacing:0.5px; margin-bottom:6px; }
        .bm-code-input-row { display:flex; gap:6px; align-items:center; }
        .bm-code-input-row input { flex:1; font-size:13px; font-weight:700; letter-spacing:2px; text-transform:uppercase; }

        /* ---- Gallery tab ---- */
        .bm-gallery { display:flex; flex-wrap:wrap; gap:12px; margin-top:12px; }
        .bm-gallery-card {
          background:white; border:1px solid #e0e0e0;
          border-radius:8px; padding:12px 14px;
          min-width:200px; flex:1 1 200px;
          box-shadow:0 1px 4px rgba(0,0,0,0.07);
          cursor:pointer; transition:box-shadow 0.15s;
        }
        .bm-gallery-card:hover { box-shadow:0 3px 10px rgba(0,0,0,0.12); }
        .bm-gc-code {
          font-size:1.4em; font-weight:800; letter-spacing:3px;
          color:#1a5c3a; margin-bottom:6px;
        }
        .bm-gc-info { display:flex; flex-direction:column; gap:2px; }
        .bm-gc-info span { font-size:11px; color:#666; }
      "))
    ),

    # Two-column layout: sidebar + map
    tags$div(class = "bm-layout",

      # ---- SIDEBAR ----
      tags$div(class = "bm-sidebar",
        shinyjs::useShinyjs(),

        # Header / welcome
        tags$div(class = "bm-header",
          tags$p(class = "bm-header-title", "Your neighborhood. Your map."),
          tags$p(class = "bm-header-sub",   "Free. Open-source. Yours to print."),
          tags$div(class = "bm-header-note",
            tags$strong("Why does it take a minute?"),
            " We pull fresh, real data from OpenStreetMap just for your area.",
            " Cached regions are much faster on repeat visits."
          )
        ),

        # LOCATION
        tags$div(class = "bm-section",
          tags$div(class = "bm-section-header", `data-toggle` = "collapse",
                   `data-target` = "#sec-location", `aria-expanded` = "true",
            tags$span(class = "bm-section-title", icon("map-marker-alt"), " Location"),
            tags$span(class = "bm-chevron", HTML("&#8250;"))
          ),
          tags$div(id = "sec-location", class = "collapse show",
            tags$div(class = "bm-section-body",
              checkboxInput("usecoordinates", "Type coordinates", TRUE),
              checkboxInput("fixframe",       "Lock frame",       FALSE),
              conditionalPanel("input.usecoordinates == true",
                fluidRow(
                  column(6, numericInput("latitude",  "Lat", 0, width = "100%")),
                  column(6, numericInput("longitude", "Lon", 0, width = "100%"))
                )
              ),
              conditionalPanel("input.usecoordinates == false",
                textInput("searchbox", NULL, placeholder = "City, neighborhood, address\u2026")
              )
            )
          )
        ),

        # PAGE & SCALE
        tags$div(class = "bm-section",
          tags$div(class = "bm-section-header", `data-toggle` = "collapse",
                   `data-target` = "#sec-page", `aria-expanded` = "true",
            tags$span(class = "bm-section-title", icon("file-alt"), " Page & Scale"),
            tags$span(class = "bm-chevron", HTML("&#8250;"))
          ),
          tags$div(id = "sec-page", class = "collapse show",
            tags$div(class = "bm-section-body",
              fluidRow(
                column(6, selectInput("page", "Size",
                           choices = c("A4" = "a4", "A3" = "a3", "Custom" = "other"))),
                column(6, selectInput("orientation", "Orientation",
                           choices = c("Portrait" = "v", "Landscape" = "h")))
              ),
              numericInput("pageH", "Height (m)", value = 0.267),
              numericInput("pageW", "Width (m)",  value = 0.18),
              selectInput("scale", "Scale",
                          choices = c("1:5,840 (neighborhood overview)" = 5840,
                                      "1:600 (1\u201d = 50\u2019, site plan)"   = 600,
                                      "1:384 (1/32\u201d = 1\u20190\u201d, design detail)" = 384)),
              fluidRow(
                column(6, numericInput("vpages", "Rows",    1, min = 1)),
                column(6, numericInput("hpages", "Columns", 1, min = 1))
              ),
              numericInput("dpi", "Resolution (DPI)", 300, min = 72, step = 1)
            )
          )
        ),

        # LAYERS
        tags$div(class = "bm-section",
          tags$div(class = "bm-section-header", `data-toggle` = "collapse",
                   `data-target` = "#sec-layers", `aria-expanded` = "true",
            tags$span(class = "bm-section-title", icon("layer-group"), " Map Layers"),
            tags$span(class = "bm-chevron", HTML("&#8250;"))
          ),
          tags$div(id = "sec-layers", class = "collapse show",
            tags$div(class = "bm-section-body",
              checkboxGroupInput("features", NULL,
                                 choices  = LAYER_CHOICES,
                                 selected = DEFAULT_FEATURES)
            )
          )
        ),

        # PRINT SETTINGS
        tags$div(class = "bm-section",
          tags$div(class = "bm-section-header", `data-toggle` = "collapse",
                   `data-target` = "#sec-print", `aria-expanded` = "false",
            tags$span(class = "bm-section-title", icon("print"), " Print Settings"),
            tags$span(class = "bm-chevron", HTML("&#8250;"))
          ),
          tags$div(id = "sec-print", class = "collapse",
            tags$div(class = "bm-section-body",
              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin-bottom:8px;", "Include in PDF"),
              checkboxInput("show_north",  "North arrow",       TRUE),
              checkboxInput("show_scale",  "Scale bar",         TRUE),
              checkboxInput("show_coords", "Coordinate labels", TRUE),
              checkboxInput("show_legend", "Legend page",       TRUE),
              checkboxInput("share_to_gallery", "Share to community gallery", FALSE),
              tags$hr(style = "margin:10px 0;"),
              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin-bottom:6px;",
                       "Layer colors"),
              tags$div(style = "font-size:10px;color:#aaa;margin-bottom:8px;",
                       "Type any 6-digit hex code. Get colors at ",
                       tags$a("g.co/colorpicker", href = "https://g.co/kgs/colorpicker",
                              target = "_blank", style = "color:#1a5c3a;"), "."),
              uiOutput("layer_color_inputs")
            )
          )
        ),

        # YOUR DATA
        tags$div(class = "bm-section",
          tags$div(class = "bm-section-header", `data-toggle` = "collapse",
                   `data-target` = "#sec-data", `aria-expanded` = "false",
            tags$span(class = "bm-section-title", icon("database"), " Your Data"),
            tags$span(class = "bm-chevron", HTML("&#8250;"))
          ),
          tags$div(id = "sec-data", class = "collapse",
            tags$div(class = "bm-section-body",

              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin-bottom:6px;",
                       "DRAW ON MAP"),
              tags$p(style = "font-size:11px;color:#666;",
                     "Use the drawing tools that appear on the map (polygon, rectangle, marker).",
                     " Click any drawn feature to add a label."),
              tags$hr(style = "margin:8px 0;"),

              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin-bottom:6px;",
                       "UPLOAD POINTS (CSV)"),
              tags$p(style = "font-size:10px;color:#aaa;margin-bottom:4px;",
                     "Needs columns: lat, lon (or latitude, longitude). Optional: label."),
              fileInput("csv_file", NULL, accept = c(".csv",".txt"), width = "100%"),

              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin:8px 0 6px;",
                       "UPLOAD POLYGONS (Shapefile)"),
              tags$p(style = "font-size:10px;color:#aaa;margin-bottom:4px;",
                     "Compress your .shp, .shx, .dbf, .prj files into a single .zip and upload it."),
              fileInput("shp_file", NULL, accept = ".zip", width = "100%"),

              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin:8px 0 6px;",
                       "UPLOAD RASTER (GeoTIFF)"),
              tags$p(style = "font-size:10px;color:#aaa;margin-bottom:4px;",
                     "Projected GeoTIFFs only. Rasters appear on the map but not in the exported PDF."),
              fileInput("raster_file", NULL, accept = c(".tif",".tiff"), width = "100%"),
              sliderInput("raster_alpha", "Raster opacity", 0.1, 1, 0.7, step = 0.05, width = "100%"),
              tags$hr(style = "margin:8px 0;"),

              tags$div(style = "font-size:10px;font-weight:700;color:#888;margin-bottom:6px;",
                       "LOADED DATA"),
              uiOutput("data_summary"),
              tags$div(style = "margin-top:8px;",
                downloadButton("download_data", "Download data bundle (.zip)",
                               class = "btn-bm-outline"),
                actionButton("clear_data_btn", "Clear all data",
                             class = "btn-bm-danger", width = "100%")
              )
            )
          )
        ),

        # DOWNLOAD
        tags$div(style = "padding:12px 14px;",
          downloadButton("print", HTML(paste0(icon("download"), " Download PDF")),
                         class = "btn btn-success btn-block")
        ),

        # MAP CODE (always visible at bottom)
        tags$div(class = "bm-code-section",
          tags$div(class = "bm-code-label", "MAP CODE"),
          tags$p(style = "font-size:10px;color:#888;margin-bottom:6px;",
                 "Codes are generated after each download and are valid for 30 days."),
          tags$div(class = "bm-code-input-row",
            textInput("map_code_input", NULL, placeholder = "e.g. HX4K2M",
                      width = "100%"),
            actionButton("restore_map_btn", "Go", class = "btn btn-outline-success btn-sm")
          )
        )
      ),

      # ---- MAP ----
      tags$div(class = "bm-main",
        leaflet::leafletOutput("map", height = "100%", width = "100%")
      )
    )
  ),

  # ================================================================
  # COMMUNITY TAB
  # ================================================================
  tabPanel("Community",
    fluidPage(
      tags$div(
        style = "max-width:860px;margin:30px auto;padding:0 20px;font-family:'Inter',Arial,sans-serif;",
        tags$h3(style = "color:#1a5c3a;font-weight:700;margin-bottom:4px;", "Community maps"),
        tags$p(style = "color:#888;font-size:14px;margin-bottom:24px;",
               "Maps people have made with BarrioMap and chosen to share."),
        uiOutput("gallery_ui"),
        tags$div(
          style = "margin-top:40px;padding-top:24px;border-top:1px solid #eee;",
          tags$h5(style = "color:#1a5c3a;margin-bottom:10px;", "What is BarrioMap?"),
          tags$p(style = "font-size:14px;color:#444;max-width:600px;line-height:1.7;",
                 "BarrioMap is a free, open-source web tool that helps communities, planners,",
                 " architects, students, and anyone else who needs a real, printable map.",
                 " No software to install. No license fees. Just open it, find your place,",
                 " and download a PDF that's ready to print, annotate, and share."),
          tags$p(style = "font-size:14px;color:#444;max-width:600px;line-height:1.7;",
                 "It's built on OpenStreetMap, so the map data belongs to the community",
                 " that made it. When you share a map here, you're helping others find",
                 " and explore the same places you care about."),
          tags$p(style = "font-size:13px;color:#aaa;margin-top:16px;",
                 tags$a("Source code on GitHub", href="https://github.com/datadiversitylab/BarrioMap",
                        target="_blank", style="color:#1a5c3a;"),
                 "  \u00b7  barriomap.arizona.edu  \u00b7  University of Arizona")
        )
      )
    )
  ),

  # ================================================================
  # ABOUT TAB
  # ================================================================
  tabPanel("About",
    fluidPage(
      tags$div(
        style = "max-width:720px;margin:30px auto;padding:0 20px;font-family:'Inter',Arial,sans-serif;color:#333;line-height:1.75;",
        tags$h3(style = "color:#1a5c3a;font-weight:700;margin-bottom:4px;", "About BarrioMap"),
        tags$p(style = "color:#888;font-size:14px;margin-bottom:28px;",
               "A tool for communities, from the University of Arizona."),

        tags$div(
          style = "background:#f0f8f4;border-left:4px solid #1a5c3a;padding:14px 18px;border-radius:0 8px 8px 0;margin-bottom:24px;",
          tags$p(style = "margin:0;font-size:14px;",
                 "BarrioMap lets anyone produce professional-quality, to-scale maps from open data.",
                 " Draw on the map, upload your own data, choose your layers and colors,",
                 " and download a vector PDF you can print, annotate, and present.")
        ),

        tags$h5(style = "color:#1a5c3a;margin-top:24px;", "What you can do"),
        tags$ul(style = "font-size:14px;padding-left:20px;",
          tags$li("Export maps at real planning scales (1\u201d=50\u2019, 1:600, 1:384, and more)"),
          tags$li("A4 and A3, portrait or landscape"),
          tags$li("Multiple OSM layers: roads, buildings, parks, water bodies, transit stops, and more"),
          tags$li("Draw your own polygons and points directly on the map"),
          tags$li("Upload CSV points, shapefiles, and rasters"),
          tags$li("Add labels to your features and download everything as a GeoJSON bundle"),
          tags$li("A map code after every download restores your exact settings for 30 days")
        ),

        tags$h5(style = "color:#1a5c3a;margin-top:24px;", "Why it takes a minute"),
        tags$p(style = "font-size:14px;",
               "Every map pulls fresh data from OpenStreetMap for exactly your area.",
               " The first export for a new region downloads a regional data extract",
               " (several hundred MB for a US state). Repeat exports are much faster."),

        tags$h5(style = "color:#1a5c3a;margin-top:24px;", "Who made this"),
        tags$ul(style = "font-size:14px;padding-left:20px;",
          tags$li(tags$strong("Sarthak Haldar"), " \u2014 Graduate Student in Data Science, School of Information, University of Arizona"),
          tags$li(tags$strong("Mackenzie Waller"), " \u2014 Landscape architect, urban designer, and assistant professor at CAPLA, University of Arizona"),
          tags$li(tags$strong("Cristian Roman-Palacios"), " \u2014 Assistant Professor of Practice, School of Information, University of Arizona")
        ),

        tags$h5(style = "color:#1a5c3a;margin-top:24px;", "Run it locally"),
        tags$ol(style = "font-size:14px;padding-left:20px;",
          tags$li("Install R and RStudio"),
          tags$li("In RStudio: File \u2192 New Project \u2192 Version Control \u2192 Git"),
          tags$li(tags$code("https://github.com/datadiversitylab/BarrioMap")),
          tags$li("Open the project and run ", tags$code("app.R"))
        ),

        tags$div(
          style = "margin-top:32px;padding-top:20px;border-top:1px solid #eee;font-size:12px;color:#aaa;",
          tags$a("GitHub", href="https://github.com/datadiversitylab/BarrioMap", target="_blank", style="color:#1a5c3a;"),
          "  \u00b7  barriomap.arizona.edu  \u00b7  Map data from OpenStreetMap (ODbL)",
          tags$br(),
          "Hosting supported by the University of Arizona."
        )
      )
    )
  )
)
