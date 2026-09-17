###################
# ui.R
#
# UI controller.
###################

# Ensure shared constants (ROAD_COLORS etc.) are available even when ui.R
# is sourced before server.R in app.R.
if (!exists("ROAD_COLORS")) source("functions/functions.R")

ui <- navbarPage(
  title = tags$span(
    style = "font-weight: 700; color: #1a5c3a; letter-spacing: -0.5px;",
    "Barrio\u00a0Map"
  ),
  id       = "main_nav",
  selected = "Map",
  theme    = bslib::bs_theme(
    version    = 4,
    bootswatch = "minty",
    primary    = "#1a5c3a",
    heading_font = bslib::font_google("Inter", wght = c(400, 600, 700))
  ),

  # MAP TAB: first thing users see
  tabPanel(
    "Map",
    value = "Map",

    shinybusy::add_busy_spinner(spin = "dots", timeout = 300,
                                height = "22px", width = "22px"),

    fluidPage(
      tags$head(
        tags$script(defer = NA,
          src = "https://umami.datadiversitylab.synology.me/script.js",
          `data-website-id` = "543cffef-a100-47af-9dd9-2cf39517077b",
          `data-domains`    = "datadiversitylab.github.io",
          `data-tag`        = "barriomap"
        ),
        tags$style(HTML("
          body { font-family: 'Inter', 'Helvetica Neue', Arial, sans-serif; }

          .navbar { border-bottom: 3px solid #1a5c3a; }
          .navbar-brand { font-size: 1.25rem; }

          /* Welcome strip */
          .bm-welcome {
            background: linear-gradient(135deg, #f0f8f4 0%, #e8f5e9 100%);
            border-bottom: 1px solid #c8e6c9;
            padding: 10px 20px;
            display: flex; align-items: center; justify-content: space-between;
            flex-wrap: wrap; gap: 6px;
          }
          .bm-welcome-main {
            font-size: 14px; color: #1a5c3a; font-weight: 600; margin: 0;
          }
          .bm-welcome-sub {
            font-size: 12px; color: #666; margin: 0;
          }
          .bm-welcome-note {
            font-size: 11px; color: #888; background: #fff;
            border: 1px solid #c8e6c9; border-radius: 4px;
            padding: 4px 10px; white-space: nowrap;
          }

          /* Map container */
          #mapContainer {
            box-shadow: 0 2px 8px rgba(0,0,0,0.12);
            position: relative;
          }

          /* Control panels */
          .bm-panel {
            background: rgba(255,255,255,0.97);
            padding: 14px 16px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.15);
            max-height: calc(100vh - 120px);
            overflow-y: auto;
          }
          .bm-panel::-webkit-scrollbar { width: 4px; }
          .bm-panel::-webkit-scrollbar-thumb {
            background: #c8e6c9; border-radius: 2px;
          }

          .bm-section-label {
            font-size: 10px; font-weight: 700; letter-spacing: 0.8px;
            text-transform: uppercase; color: #1a5c3a;
            margin: 10px 0 4px; padding-top: 8px;
            border-top: 1px solid #e8f5e9;
          }
          .bm-section-label:first-child { margin-top: 0; border-top: none; }

          .form-group { margin-bottom: 8px; }
          .form-group label { font-size: 12px; color: #444; margin-bottom: 2px; }

          /* Download button */
          #print {
            width: 100%; margin-top: 10px;
            background: #1a5c3a; border-color: #1a5c3a;
            font-weight: 600; border-radius: 6px;
            padding: 8px 0; font-size: 14px;
          }
          #print:hover { background: #155230; }

          /* Color swatches next to selects */
          .color-row { display: flex; align-items: center; gap: 8px; }
          .color-swatch {
            width: 18px; height: 18px; border-radius: 3px;
            border: 1px solid #ccc; flex-shrink: 0;
          }

          /* Map code box */
          .bm-code-box {
            background: #f0f8f4; border: 1px solid #1a5c3a;
            border-radius: 6px; padding: 8px 12px; margin: 6px 0;
            font-size: 18px; font-weight: 800; letter-spacing: 4px;
            color: #1a5c3a; text-align: center;
          }

          /* Checkbox group tighter */
          .checkbox-group-inline label { margin-bottom: 2px; }
        "))
      ),

      # Welcoming strip
      tags$div(
        class = "bm-welcome",
        tags$div(
          tags$p(class = "bm-welcome-main",
                 "Your neighborhood. Your map."),
          tags$p(class = "bm-welcome-sub",
                 "Search for a place, pick a scale, and download a PDF ready to print.")
        ),
        tags$div(
          class = "bm-welcome-note",
          tags$strong("Why is it slow?"),
          " Your map pulls real, fresh data from OpenStreetMap just for your area.",
          " Once a region is cached it\u2019s much faster."
        )
      ),

      # Map + control panels
      tags$div(
        id = "mapContainer",
        leaflet::leafletOutput("map", height = "565px"),

        # Main controls (left)
        absolutePanel(
          class    = "bm-panel",
          top = 16, left = 16, width = 270,
          draggable = TRUE,

          shinyjs::useShinyjs(),

          # LOCATION
          tags$div(class = "bm-section-label", "Location"),

          checkboxInput("usecoordinates", "Use coordinates", TRUE),
          checkboxInput("fixframe",       "Lock frame",      FALSE),

          conditionalPanel(
            condition = "input.usecoordinates == true",
            fluidRow(
              column(6, numericInput("latitude",  "Lat", value = 0, width = "100%")),
              column(6, numericInput("longitude", "Lon", value = 0, width = "100%"))
            )
          ),
          conditionalPanel(
            condition = "input.usecoordinates == false",
            textInput("searchbox", "Search", placeholder = "City, address\u2026")
          ),

          # PAGE
          tags$div(class = "bm-section-label", "Page"),

          selectInput("page", NULL,
                      choices = c("A4" = "a4", "A3" = "a3", "Custom" = "other")),
          selectInput("orientation", NULL,
                      choices = c("Portrait" = "v", "Landscape" = "h")),
          numericInput("pageH", "Height (m)", value = 0.267),
          numericInput("pageW", "Width (m)",  value = 0.18),

          # SCALE
          tags$div(class = "bm-section-label", "Scale"),

          selectInput("scale", NULL,
                      choices = c("1:5,840" = 5840, "1:600" = 600, "1:384" = 384)),

          # TOGGLES
          tags$div(class = "bm-section-label", "Settings"),

          checkboxInput("showMoreSettings",  "More settings",  FALSE),
          checkboxInput("showPrintSettings", "Print settings", FALSE),
          checkboxInput("showCodePanel",     "Map code",       FALSE),

          downloadButton("print", "\u2193 Download PDF")
        ),

        # More settings panel
        conditionalPanel(
          condition = "input.showMoreSettings == true",
          absolutePanel(
            class = "bm-panel",
            top = 16, left = 302, width = 230,
            draggable = TRUE,

            tags$div(class = "bm-section-label", "Output"),
            numericInput("dpi", "Resolution (DPI)", 300, min = 72, step = 1),

            tags$div(class = "bm-section-label", "Panels"),
            numericInput("vpages", "Rows",    value = 1, min = 1),
            numericInput("hpages", "Columns", value = 1, min = 1)
          )
        ),

        # Print settings panel
        conditionalPanel(
          condition = "input.showPrintSettings == true",
          absolutePanel(
            class = "bm-panel",
            top = 16, left = 302, width = 260,
            draggable = TRUE,

            # Layers
            tags$div(class = "bm-section-label", "Layers to export"),

            checkboxGroupInput(
              "features", NULL,
              choices  = c("Roads" = "roads", "Buildings" = "buildings"),
              selected = c("roads", "buildings")
            ),

            conditionalPanel(
              condition = "input.features && input.features.indexOf('roads') >= 0",
              tags$div(class = "bm-section-label", "Road color"),
              selectInput("roads_color", NULL,
                          choices  = ROAD_COLORS,
                          selected = "#555555")
            ),

            conditionalPanel(
              condition = "input.features && input.features.indexOf('buildings') >= 0",
              tags$div(class = "bm-section-label", "Building fill"),
              selectInput("bld_fill", NULL,
                          choices  = BLD_FILL_COLORS,
                          selected = "#f2f2f2"),
              tags$div(class = "bm-section-label", "Building outline"),
              selectInput("bld_border", NULL,
                          choices  = BLD_BORDER_COLORS,
                          selected = "#aaaaaa")
            ),

            # Map elements
            tags$div(class = "bm-section-label", "Include in PDF"),

            checkboxInput("show_north",  "North arrow",        TRUE),
            checkboxInput("show_scale",  "Scale bar",          TRUE),
            checkboxInput("show_coords", "Coordinate labels",  TRUE),
            checkboxInput("show_legend", "Legend page",        TRUE)
          )
        ),

        # Map code panel
        conditionalPanel(
          condition = "input.showCodePanel == true",
          absolutePanel(
            class = "bm-panel",
            top = 16, right = 16, width = 240,
            draggable = TRUE,

            tags$div(class = "bm-section-label", "Restore a map"),
            tags$p(style = "font-size: 11px; color: #666; margin-bottom: 6px;",
                   "Enter a 6-character code from a previous download to restore your map settings."),
            textInput("map_code_input", NULL,
                      placeholder = "e.g. HX4K2M"),
            actionButton("restore_map_btn", "Restore map",
                         class = "btn btn-outline-success btn-sm btn-block"),
            tags$p(style = "font-size: 10px; color: #aaa; margin-top: 6px;",
                   "Codes are valid for 30 days after download.")
          )
        )
      )
    )
  ),

  # ABOUT TAB
  tabPanel(
    "About",
    fluidPage(
      tags$div(
        style = paste0(
          "max-width: 720px; margin: 40px auto; padding: 0 20px;",
          " font-family: 'Inter', 'Helvetica Neue', Arial, sans-serif;",
          " color: #333; line-height: 1.7;"
        ),

        tags$h2(style = "color: #1a5c3a; font-weight: 700; margin-bottom: 4px;",
                "Barrio Map"),
        tags$p(style = "color: #888; font-size: 15px; margin-bottom: 32px;",
               "Open-source mapping for communities"),

        tags$div(
          style = "background: #f0f8f4; border-left: 4px solid #1a5c3a; padding: 16px 20px; border-radius: 0 8px 8px 0; margin-bottom: 24px;",
          tags$p(style = "margin: 0; font-size: 14px;",
                 "Barrio Map helps urban planners, architects, designers, students, and community organizers produce professional-quality maps from open data. No license fees. No software to install. Just a place to put your neighborhood on paper at a real scale.")
        ),

        tags$h4(style = "color: #1a5c3a; margin-top: 28px;", "Why it\u2019s slow"),
        tags$p(style = "font-size: 14px;",
               "Every map pulls fresh, real geometry from OpenStreetMap for exactly your area. The first time you export a region, the app downloads the regional extract (for a US state, that\u2019s hundreds of MB). Repeat exports in the same session skip that step. We think accurate, sourced data is worth the wait."),

        tags$h4(style = "color: #1a5c3a; margin-top: 28px;", "What you get"),
        tags$ul(
          style = "font-size: 14px; padding-left: 20px;",
          tags$li("Print-ready vector PDFs at real planning scales (1\u201d = 50\u2019, 1:600, and more)"),
          tags$li("A4 and A3 sheet sizes, portrait or landscape"),
          tags$li("Roads and buildings from OpenStreetMap, color-customizable"),
          tags$li("Multi-panel tiling for large sites"),
          tags$li("A unique map code to share and restore your exact settings for 30 days")
        ),

        tags$h4(style = "color: #1a5c3a; margin-top: 28px;", "Who we are"),
        tags$ul(
          style = "font-size: 14px; padding-left: 20px;",
          tags$li(tags$strong("Sarthak Haldar"), " \u2014 Graduate Student in Data Science, School of Information, University of Arizona"),
          tags$li(tags$strong("Mackenzie Waller"), " \u2014 Landscape architect, urban designer, and assistant professor at CAPLA, University of Arizona"),
          tags$li(tags$strong("Cristian Roman-Palacios"), " \u2014 Assistant Professor of Practice, School of Information, University of Arizona")
        ),

        tags$h4(style = "color: #1a5c3a; margin-top: 28px;", "Acknowledgements"),
        tags$p(style = "font-size: 14px;",
               "Hosting made possible with support from the University of Arizona. Map data from OpenStreetMap contributors, published under the Open Database License (ODbL)."),

        tags$div(
          style = "margin-top: 32px; padding-top: 20px; border-top: 1px solid #eee; font-size: 12px; color: #aaa;",
          tags$a(href = "https://github.com/datadiversitylab/BarrioMap",
                 target = "_blank", style = "color: #1a5c3a;",
                 "Source code on GitHub"),
          "  \u00b7  viz.datascience.arizona.edu/barriomap"
        )
      )
    )
  ),

  # MORE MENU
  navbarMenu(
    "More",
    tabPanel(
      "Run it locally",
      fluidPage(
        tags$div(
          style = "max-width: 720px; margin: 40px auto; padding: 0 20px; font-family: 'Inter', Arial, sans-serif; color: #333; line-height: 1.7;",
          tags$h4(style = "color: #1a5c3a;", "Run Barrio Map locally"),
          tags$p(style = "font-size: 14px;",
                 "You need R and RStudio. Once those are installed:"),
          tags$ol(
            style = "font-size: 14px; padding-left: 20px;",
            tags$li("In RStudio, go to File \u2192 New Project \u2192 Version Control \u2192 Git"),
            tags$li(tags$code("https://github.com/datadiversitylab/BarrioMap")),
            tags$li("Open the project and run ", tags$code("app.R"))
          ),
          tags$p(style = "font-size: 14px;",
                 "Pull requests, bug reports, and documentation improvements are welcome.")
        )
      )
    ),
    tabPanel(
      "Other tools",
      fluidPage(
        tags$div(
          style = "max-width: 720px; margin: 40px auto; padding: 0 20px; font-family: 'Inter', Arial, sans-serif; color: #333; line-height: 1.7;",
          tags$h4(style = "color: #1a5c3a;", "Other mapping tools"),
          tags$p(style = "font-size: 14px;",
                 "These are worth knowing, each with a different focus:"),
          tags$ul(
            style = "font-size: 14px; padding-left: 20px;",
            tags$li(tags$a("RapiD", href = "https://mapwith.ai/", target = "_blank"),
                    " \u2014 AI-assisted OSM editing, requires an OSM account"),
            tags$li(tags$a("Inkatlas", href = "https://inkatlas.com/", target = "_blank"),
                    " \u2014 print-ready maps, paid service"),
            tags$li(tags$a("PrintMaps", href = "https://www.printmaps.net/", target = "_blank"),
                    " \u2014 multi-format export, paid"),
            tags$li(tags$a("Field Papers", href = "http://fieldpapers.org/", target = "_blank"),
                    " \u2014 printable PDFs, limited planning overlap"),
            tags$li(tags$a("Milvusmap", href = "http://milvusmap.eu/", target = "_blank"),
                    " \u2014 PDF export, limited scale control")
          )
        )
      )
    )
  )
)
