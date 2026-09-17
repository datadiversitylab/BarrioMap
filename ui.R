###################
# ui.R
###################
if (!exists("LAYER_DEFS")) source("functions/functions.R")

DEFAULT_FEATURES <- c("roads", "buildings")
LAYER_CHOICES    <- setNames(names(LAYER_DEFS), sapply(LAYER_DEFS, `[[`, "label"))
APP_URL          <- "datadiversitylab.github.io/barriomap"

ui <- navbarPage(
  title    = tags$span(style = "font-weight:800;color:#1a5c3a;letter-spacing:-0.5px;",
                       "BarrioMap"),
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
      tags$script(HTML("
        $(document).on('keypress', '#searchbox', function(e) {
          if (e.which === 13) { $('#search_btn').click(); }
        });
      ")),
      tags$style(HTML("

        body { font-family:'Inter','Helvetica Neue',Arial,sans-serif; margin:0; }
        .navbar { border-bottom:3px solid #1a5c3a; }

        /* Map tab fills viewport without blocking other tabs */
        .bm-map-wrapper {
          height: calc(100vh - 56px);
          overflow: hidden;
          margin: -15px;
          padding: 0;
        }
        .bm-layout { display:flex; height:100%; overflow:hidden; }

        /* Sidebar */
        .bm-sidebar {
          width: 300px; min-width:260px; flex-shrink:0;
          height:100%; overflow-y:auto;
          background:#fff; border-right:1px solid #e0e0e0; padding:0;
          box-shadow: 2px 0 8px rgba(0,0,0,0.06);
        }
        .bm-sidebar::-webkit-scrollbar { width:4px; }
        .bm-sidebar::-webkit-scrollbar-thumb { background:#c8e6c9; border-radius:2px; }

        /* Map area */
        .bm-main { flex:1; position:relative; height:100%; }
        #map { height:100% !important; width:100% !important; }

        /* Header */
        .bm-header {
          background: linear-gradient(135deg, #1a5c3a 0%, #2d7a50 100%);
          padding: 16px 14px 12px; color:white;
        }
        .bm-header-title { font-size:16px; font-weight:700; margin:0 0 2px; }
        .bm-header-sub   { font-size:11px; color:#a8d5ba; margin:0 0 10px; }
        .bm-header-note  {
          background:rgba(255,255,255,0.12); border-radius:5px;
          padding:6px 9px; font-size:10.5px; color:#d0eeda; line-height:1.5;
        }

        /* Accordion */
        .bm-section { border-bottom:1px solid #f0f0f0; }
        .bm-section-header {
          display:flex; align-items:center; justify-content:space-between;
          padding:9px 14px; cursor:pointer; user-select:none;
          background:#fafafa; transition:background 0.15s;
        }
        .bm-section-header:hover { background:#f0f8f4; }
        .bm-section-title { font-size:11.5px; font-weight:700; color:#1a5c3a; letter-spacing:0.3px; }
        .bm-chevron { color:#aaa; font-size:14px; transition:transform 0.2s; }
        .bm-section-body { padding:10px 14px 12px; }

        /* Form - force uniform 12px across all input types */
        .form-group { margin-bottom:7px; }
        .form-group label { font-size:11px; color:#555; margin-bottom:2px; }
        .bm-sidebar .form-control { font-size:12px !important; padding:4px 7px !important; height:auto !important; }
        .bm-sidebar select,
        .bm-sidebar select option,
        .bm-sidebar .selectize-input,
        .bm-sidebar .selectize-input * { font-size:12px !important; }
        .bm-sidebar input[type=number],
        .bm-sidebar input[type=text] { font-size:12px !important; }
        .bm-sidebar textarea { font-size:12px !important; }
        .bm-hint { font-size:10px; color:#aaa; margin-top:2px; }

        /* Search row */
        .bm-search-row { display:flex; gap:6px; align-items:flex-start; margin-bottom:6px; }
        .bm-search-row .form-group { flex:1; margin-bottom:0; }
        #search_btn { flex-shrink:0; padding:5px 10px; }

        /* Autocomplete suggestions */
        .bm-suggestions-container {
          border:1px solid #ddd; border-radius:4px; overflow:hidden;
          margin-top:0; margin-bottom:8px;
          box-shadow:0 3px 10px rgba(0,0,0,0.12); background:white; z-index:1000;
        }
        .bm-suggestion {
          padding:6px 10px; font-size:11px; color:#333;
          cursor:pointer; border-bottom:1px solid #f5f5f5; line-height:1.4;
        }
        .bm-suggestion:hover { background:#f0f8f4; color:#1a5c3a; }
        .bm-suggestion:last-child { border-bottom:none; }

        /* Download button */
        #print {
          width:100%; background:#1a5c3a; border-color:#1a5c3a;
          color:white !important; font-weight:700; border-radius:6px;
          padding:9px 0; font-size:13px; margin-top:4px;
        }
        #print:hover { background:#155230; border-color:#155230; }
        #print .glyphicon { display:none; }

        /* Other buttons */
        .btn-bm-outline {
          width:100%; background:white; border:1.5px solid #1a5c3a;
          color:#1a5c3a; font-weight:600; border-radius:6px;
          padding:6px 0; font-size:12px; cursor:pointer;
        }
        .btn-bm-outline:hover { background:#f0f8f4; }
        .btn-bm-danger {
          width:100%; background:white; border:1.5px solid #e53935;
          color:#e53935; font-weight:600; border-radius:6px;
          padding:5px 0; font-size:11px; cursor:pointer; margin-top:4px;
        }

        /* Map code section */
        .bm-code-section { background:#f0f8f4; border-top:2px solid #1a5c3a; padding:12px 14px; }
        .bm-code-label { font-size:10px; font-weight:700; color:#1a5c3a; letter-spacing:0.5px; margin-bottom:5px; }
        .bm-code-input-row { display:flex; gap:6px; align-items:flex-end; }
        .bm-code-input-row .form-group { flex:1; margin-bottom:0; }

        /* Gallery */
        .bm-gallery { display:flex; flex-wrap:wrap; gap:12px; margin-top:12px; }
        .bm-gallery-card {
          background:white; border:1px solid #e0e0e0; border-radius:8px;
          padding:12px 14px; min-width:200px; flex:1 1 200px;
          box-shadow:0 1px 4px rgba(0,0,0,0.07);
        }
        .bm-gc-code { font-size:1.4em; font-weight:800; letter-spacing:3px; color:#1a5c3a; margin-bottom:6px; }
        .bm-gc-info { display:flex; flex-direction:column; gap:2px; }
        .bm-gc-info span { font-size:11px; color:#666; }

        /* Team */
        .bm-team-card { display:flex; gap:14px; align-items:flex-start; margin-bottom:20px; }
        .bm-avatar {
          width:60px; height:60px; border-radius:50%; background:#c8e6c9;
          display:flex; align-items:center; justify-content:center;
          font-size:18px; font-weight:700; color:#1a5c3a; flex-shrink:0;
        }
      "))
    ),

    tags$div(class = "bm-map-wrapper",
      tags$div(class = "bm-layout",

        # SIDEBAR
        tags$div(class = "bm-sidebar",
          shinyjs::useShinyjs(),

          # Header
          tags$div(class = "bm-header",
            tags$p(class = "bm-header-title", "Your neighborhood. Your map."),
            tags$p(class = "bm-header-sub",   "This app is free, open-source. Its yours to print."),
            tags$div(class = "bm-header-note",
              tags$strong("Why is it slow?"),
              " We pull data from OpenStreetMap just for your area.",
              " Cached regions are faster on repeat visits."
            )
          ),

          # MAP INFO
          tags$div(class = "bm-section",
            tags$div(class = "bm-section-header",
                     `data-toggle`="collapse", `data-target`="#sec-info",
                     `aria-expanded`="true",
              tags$span(class="bm-section-title", icon("tag"), " Map Info"),
              tags$span(class="bm-chevron", HTML("&#8250;"))
            ),
            tags$div(id="sec-info", class="collapse show",
              tags$div(class="bm-section-body",
                textInput("map_name", "Map name",
                          placeholder = "e.g. Downtown study area"),
                textAreaInput("map_desc", "Description",
                              placeholder = "Brief description (optional)",
                              rows = 2, width = "100%")
              )
            )
          ),

          # LOCATION
          tags$div(class = "bm-section",
            tags$div(class = "bm-section-header",
                     `data-toggle`="collapse", `data-target`="#sec-location",
                     `aria-expanded`="true",
              tags$span(class="bm-section-title", icon("map-marker-alt"), " Location"),
              tags$span(class="bm-chevron", HTML("&#8250;"))
            ),
            tags$div(id="sec-location", class="collapse show",
              tags$div(class="bm-section-body",
                tags$div(class="bm-search-row",
                  tags$div(class="form-group",
                    textInput("searchbox", NULL,
                              placeholder = "Search for a place...",
                              width = "100%")
                  ),
                  actionButton("search_btn", icon("search"),
                               class = "btn btn-outline-success btn-sm")
                ),
                uiOutput("search_suggestions"),
                checkboxInput("fixframe", "Lock frame to current view", FALSE),
                fluidRow(
                  column(6, numericInput("latitude",  "Latitude", 0, width="100%")),
                  column(6, numericInput("longitude", "Longitude", 0, width="100%"))
                ),
                tags$div(style="margin-top:4px;",
                  tags$label("Page outline color",
                             style="font-size:11px;color:#555;display:block;margin-bottom:3px;"),
                  tags$div(style="display:flex;align-items:center;gap:8px;",
                    tags$div(id="outline_color_preview",
                             style="width:22px;height:22px;border-radius:3px;background:#1a5c3a;border:1px solid #ccc;flex-shrink:0;"),
                    tags$input(id="outline_color", type="text", value="#1a5c3a",
                               placeholder="#RRGGBB",
                               style="flex:1;padding:4px 6px;border:1px solid #ddd;border-radius:4px;font-size:12px;",
                               oninput=paste0(
                                 "document.getElementById('outline_color_preview').style.background=this.value;",
                                 "Shiny.setInputValue('outline_color',this.value)"
                               ))
                  ),
                  tags$p(class="bm-hint",
                         "Get hex codes at ",
                         tags$a("g.co/colorpicker", href="https://g.co/kgs/colorpicker",
                                target="_blank", style="color:#1a5c3a;"), ".")
                )
              )
            )
          ),

          # PAGE & SCALE
          tags$div(class = "bm-section",
            tags$div(class = "bm-section-header",
                     `data-toggle`="collapse", `data-target`="#sec-page",
                     `aria-expanded`="true",
              tags$span(class="bm-section-title", icon("file-alt"), " Page and scale"),
              tags$span(class="bm-chevron", HTML("&#8250;"))
            ),
            tags$div(id="sec-page", class="collapse show",
              tags$div(class="bm-section-body",
                fluidRow(
                  column(6, selectInput("page", "Size",
                             choices = c("A4" = "a4", "A3" = "a3", "Custom" = "other"))),
                  column(6, selectInput("orientation", "Orientation",
                             choices = c("Portrait" = "v", "Landscape" = "h")))
                ),
                numericInput("pageH", "Height (m)", 0.267),
                numericInput("pageW", "Width (m)",  0.18),
                selectInput("scale", "Scale",
                            choices = c(
                              "1:31,680 - ~1/2 mile"   = 31680,
                              "1:600 - Site plan"  = 600,
                              "1:384 - Design detail"             = 384
                            )),
                fluidRow(
                  column(6, numericInput("vpages", "Rows",    1, min=1)),
                  column(6, numericInput("hpages", "Columns", 1, min=1))
                ),
                numericInput("dpi", "Resolution (DPI)", 300, min=72, step=1)
              )
            )
          ),

          # MAP LAYERS
          tags$div(class = "bm-section",
            tags$div(class = "bm-section-header",
                     `data-toggle`="collapse", `data-target`="#sec-layers",
                     `aria-expanded`="true",
              tags$span(class="bm-section-title", icon("layer-group"), " Map layers"),
              tags$span(class="bm-chevron", HTML("&#8250;"))
            ),
            tags$div(id="sec-layers", class="collapse show",
              tags$div(class="bm-section-body",
                checkboxGroupInput("features", NULL,
                                   choices=LAYER_CHOICES, selected=DEFAULT_FEATURES)
              )
            )
          ),

          # PRINT SETTINGS
          tags$div(class = "bm-section",
            tags$div(class = "bm-section-header",
                     `data-toggle`="collapse", `data-target`="#sec-print",
                     `aria-expanded`="false",
              tags$span(class="bm-section-title", icon("print"), " Print Settings"),
              tags$span(class="bm-chevron", HTML("&#8250;"))
            ),
            tags$div(id="sec-print", class="collapse",
              tags$div(class="bm-section-body",
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin-bottom:6px;",
                       "MAP ELEMENTS"),
                checkboxInput("show_north",  "North arrow",       TRUE),
                checkboxInput("show_scale",  "Scale bar",         TRUE),
                checkboxInput("show_coords", "Coordinate labels", TRUE),
                checkboxInput("show_legend", "Legend page",       TRUE),
                tags$hr(style="margin:8px 0;"),
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin-bottom:6px;",
                       "INCLUDE YOUR DATA IN PDF"),
                checkboxInput("print_drawn",    "Drawn features",    TRUE),
                checkboxInput("print_points",   "Uploaded points",   TRUE),
                checkboxInput("print_polygons", "Uploaded polygons", TRUE),
                tags$hr(style="margin:8px 0;"),
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin-bottom:4px;",
                       "LAYER COLORS"),
                tags$p(class="bm-hint", style="margin-bottom:8px;",
                       "Type any 6-digit hex code. Get colors at ",
                       tags$a("g.co/colorpicker", href="https://g.co/kgs/colorpicker",
                              target="_blank", style="color:#1a5c3a;"), "."),
                uiOutput("layer_color_inputs")
              )
            )
          ),

          # YOUR DATA
          tags$div(class = "bm-section",
            tags$div(class = "bm-section-header",
                     `data-toggle`="collapse", `data-target`="#sec-data",
                     `aria-expanded`="false",
              tags$span(class="bm-section-title", icon("database"), " Your Data"),
              tags$span(class="bm-chevron", HTML("&#8250;"))
            ),
            tags$div(id="sec-data", class="collapse",
              tags$div(class="bm-section-body",
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin-bottom:4px;",
                       "DRAW ON MAP"),
                tags$p(style="font-size:11px;color:#666;margin-bottom:8px;",
                       "Use the drawing toolbar on the map (polygon, rectangle, marker).",
                       " Click any drawn feature to open the label editor."),
                tags$hr(style="margin:8px 0;"),
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin-bottom:3px;",
                       "UPLOAD POINTS (CSV or TXT)"),
                tags$p(class="bm-hint", style="margin-bottom:4px;",
                       "Needs columns: lat, lon. Optional: label."),
                fileInput("csv_file", NULL, accept=c(".csv",".txt"), width="100%"),
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin:6px 0 3px;",
                       "UPLOAD POLYGONS (Shapefile)"),
                tags$p(class="bm-hint", style="margin-bottom:4px;",
                       "ZIP your .shp, .shx, .dbf, and .prj files together."),
                fileInput("shp_file", NULL, accept=".zip", width="100%"),
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin:6px 0 3px;",
                       "UPLOAD RASTER (GeoTIFF)"),
                tags$p(class="bm-hint", style="margin-bottom:4px;",
                       "Shown on the map but not printed in the PDF."),
                fileInput("raster_file", NULL, accept=c(".tif",".tiff"), width="100%"),
                sliderInput("raster_alpha", "Raster opacity",
                            0.1, 1, 0.7, step=0.05, width="100%"),
                tags$hr(style="margin:8px 0;"),
                tags$p(style="font-size:10px;font-weight:700;color:#888;margin-bottom:4px;",
                       "LOADED DATA"),
                uiOutput("data_summary"),
                tags$div(style="margin-top:8px;",
                  actionButton("clear_data_btn", "Clear all data",
                               class="btn-bm-danger", width="100%")
                )
              )
            )
          ),

          # DOWNLOAD + SHARE
          tags$div(style="padding:12px 14px 6px;",
            actionButton("generate_btn", "Generate map",
                         icon  = icon("cog"),
                         class = "btn btn-success btn-block",
                         width = "100%"),
            conditionalPanel(
              condition = "output.pdf_ready",
              tags$div(style = "margin-top:6px;",
                downloadButton("download_pdf", "Download",
                               icon  = NULL,
                               class = "btn btn-outline-success btn-block"),
                tags$p(class = "bm-hint", style = "margin:3px 0 0;",
                       "Your map is ready. Click to save.")
              )
            ),
            tags$p(class="bm-hint", style="margin:6px 0 8px;",
                   "Downloads as PDF. If you added your own data (drawn, uploaded points or polygons), you get a ZIP containing the PDF and all your data files."
            ),
            checkboxInput("share_to_gallery",
                          "Share this map to the community gallery",
                          value = FALSE),
            tags$p(class="bm-hint",
                   "Optional and anonymous. Shared maps appear on the Community page.")
          ),

          # MAP CODE (always visible)
          tags$div(class="bm-code-section",
            tags$div(class="bm-code-label", "MAP CODE"),
            tags$p(class="bm-hint", style="margin-bottom:6px;",
                   "You get a code after every download. Enter it here to restore any map for 30 days."),
            tags$div(class="bm-code-input-row",
              tags$div(class="form-group",
                textInput("map_code_input", NULL, placeholder="e.g. HX4K2M", width="100%")
              ),
              actionButton("restore_map_btn", "Go", class="btn btn-outline-success btn-sm")
            )
          )
        ),

        # MAP
        tags$div(class="bm-main",
          leaflet::leafletOutput("map", height="100%", width="100%")
        )
      )
    )
  ),

  # ================================================================
  # COMMUNITY TAB
  # ================================================================
  tabPanel("Community",
    tags$div(
      style="max-width:860px;margin:30px auto;padding:0 20px;font-family:'Inter',Arial,sans-serif;",
      tags$h3(style="color:#1a5c3a;font-weight:700;margin-bottom:4px;", "Community maps"),
      tags$p(style="color:#888;font-size:14px;margin-bottom:24px;",
             "Maps people made with BarrioMap and chose to share publicly."),
      uiOutput("gallery_ui"),
      tags$div(
        style="margin-top:40px;padding-top:24px;border-top:1px solid #eee;",
        tags$h5(style="color:#1a5c3a;margin-bottom:10px;font-weight:600;",
                "What is BarrioMap?"),
        tags$p(style="font-size:14px;color:#444;max-width:620px;line-height:1.7;",
               "BarrioMap is a free, open-source web tool for communities, planners,",
               " architects, students, and anyone who needs a real, printable map.",
               " No software to install. No license fees.",
               " Find your place, choose your layers, and download a PDF."),
        tags$p(style="font-size:13px;color:#aaa;margin-top:14px;",
               tags$a("Source code on GitHub",
                      href="https://github.com/datadiversitylab/BarrioMap",
                      target="_blank", style="color:#1a5c3a;"),
               "  |  ", APP_URL, "  |  University of Arizona")
      )
    )
  ),

  # ================================================================
  # ABOUT TAB
  # ================================================================
  tabPanel("About",
    tags$div(
      style=paste0("max-width:720px;margin:30px auto 60px;padding:0 20px;",
                   "font-family:'Inter',Arial,sans-serif;color:#333;line-height:1.75;"),
      tags$h3(style="color:#1a5c3a;font-weight:700;margin-bottom:4px;", "About BarrioMap"),
      tags$p(style="color:#888;font-size:14px;margin-bottom:28px;",
             "A free mapping tool for communities, from the University of Arizona."),
      tags$div(
        style="background:#f0f8f4;border-left:4px solid #1a5c3a;padding:14px 18px;border-radius:0 8px 8px 0;margin-bottom:28px;",
        tags$p(style="margin:0;font-size:14px;",
               "BarrioMap lets anyone produce professional, to-scale maps from open data.",
               " Draw your own features, upload data, pick layers and colors,",
               " and download a vector PDF you can print, annotate, and take into the field.")
      ),

      tags$h5(style="color:#1a5c3a;margin-top:28px;", "The team"),

      tags$div(class="bm-team-card",
               tags$img(
                 src   = "sarthak.jpg",
                 alt   = "Sarthak Haldar",
                 style = "width:60px;height:60px;border-radius:50%;
           object-fit:cover;border:2px solid #c8e6c9;flex-shrink:0;"
               ),
               tags$div(
                 tags$div(style="font-weight:600;font-size:14px;", "Sarthak Haldar"),
                 tags$div(style="font-size:12px;color:#666;", "Graduate Student, Data Science"),
                 tags$div(style="font-size:11px;color:#888;",
                          "College of Information Science, University of Arizona"),
                 tags$a(href="mailto:shaldar@arizona.edu",
                        style="font-size:11px;color:#1a5c3a;", "shaldar@arizona.edu")
               )
      ),
      tags$div(class="bm-team-card",
               tags$img(
                 src   = "mackenzie.jpg",
                 alt   = "Mackenzie Waller",
                 style = "width:60px;height:60px;border-radius:50%;
           object-fit:cover;border:2px solid #c8e6c9;flex-shrink:0;"
               ),
               tags$div(
                 tags$div(style="font-weight:600;font-size:14px;", "Mackenzie Waller"),
                 tags$div(style="font-size:12px;color:#666;",
                          "Landscape Architect & Assistant Professor"),
                 tags$div(style="font-size:11px;color:#888;",
                          "College of Architecture, Planning and Landscape Architecture, University of Arizona"),
                 tags$a(href="mailto:mwaller@arizona.edu",
                        style="font-size:11px;color:#1a5c3a;", "mwaller@arizona.edu")
               )
      ),
      tags$div(class="bm-team-card",
               tags$img(
                 src   = "cristian.jpg",
                 alt   = "Cristian Roman-Palacios",
                 style = "width:60px;height:60px;border-radius:50%;
           object-fit:cover;border:2px solid #c8e6c9;flex-shrink:0;"
               ),
               tags$div(
                 tags$div(style="font-weight:600;font-size:14px;", "Cristian Roman-Palacios"),
                 tags$div(style="font-size:12px;color:#666;", "Assistant Professor of Practice"),
                 tags$div(style="font-size:11px;color:#888;",
                          "College of Information Science, University of Arizona"),
                 tags$a(href="mailto:cromanpa@arizona.edu",
                        style="font-size:11px;color:#1a5c3a;", "cromanpa@arizona.edu")
               )
      ),

      tags$h5(style="color:#1a5c3a;margin-top:24px;", "What you can do"),
      tags$ul(style="font-size:14px;padding-left:20px;",
        tags$li("Export maps at real planning scales: 1 in = 50 ft, 1:600, 1:384, and more"),
        tags$li("A4 and A3, portrait or landscape, at any DPI"),
        tags$li("Nine OSM layers: roads, buildings, parks, water, transit, healthcare, and more"),
        tags$li("Draw your own polygons and points on the map and label them"),
        tags$li("Upload CSV points, shapefiles, and rasters"),
        tags$li("Download your data as a GeoJSON bundle you can open in QGIS or similar"),
        tags$li("A map code after every download lets you restore your settings for 30 days")
      ),

      tags$h5(style="color:#1a5c3a;margin-top:28px;", "How it works"),
      tags$ol(style="font-size:14px;padding-left:20px;",
        tags$li("Search for a place or enter coordinates in the sidebar"),
        tags$li("Name your map and add a description"),
        tags$li("Choose your page size, scale, rows, and columns"),
        tags$li("Select OSM layers and adjust their colors if needed"),
        tags$li("Draw features or upload your own data, add labels"),
        tags$li("Click Download to get a ZIP with your PDF and all your data"),
        tags$li("Your map code appears after download - save it to come back to the same settings")
      ),

      tags$h5(style="color:#1a5c3a;margin-top:28px;", "Built with"),
      tags$ul(style="font-size:14px;padding-left:20px;",
        tags$li(tags$a("R + Shiny", href="https://shiny.posit.co", target="_blank"),
                ": the application framework"),
        tags$li(tags$a("Leaflet + leaflet.extras",
                       href="https://leafletjs.com", target="_blank"),
                ": interactive map and drawing tools"),
        tags$li(tags$a("OpenStreetMap", href="https://www.openstreetmap.org",
                       target="_blank"), " via ",
                tags$a("osmextract", href="https://docs.ropensci.org/osmextract/",
                       target="_blank"), " - map data"),
        tags$li(tags$a("ggplot2", href="https://ggplot2.tidyverse.org", target="_blank"),
                " + ",
                tags$a("ggspatial", href="https://paleolimbot.github.io/ggspatial/",
                       target="_blank"), ": PDF rendering"),
        tags$li(tags$a("qpdf", href="https://cran.r-project.org/package=qpdf",
                       target="_blank"), ": PDF assembly")
      ),

      tags$h5(style="color:#1a5c3a;margin-top:28px;", "Why it takes a minute"),
      tags$p(style="font-size:14px;",
             "Every export pulls fresh data from OpenStreetMap for your exact area.",
             " The first time you export a region, the app downloads the regional extract",
             " (a few hundred MB for a US state). Repeat exports in the same session skip that step."),

      tags$h5(style="color:#1a5c3a;margin-top:28px;", "Run it locally"),
      tags$ol(style="font-size:14px;padding-left:20px;",
        tags$li("Install R and RStudio"),
        tags$li("File - New Project - Version Control - Git"),
        tags$li(tags$code("https://github.com/datadiversitylab/BarrioMap")),
        tags$li("Open the project and run ", tags$code("app.R"))
      ),

      tags$div(
        style="margin-top:32px;padding-top:20px;border-top:1px solid #eee;font-size:12px;color:#aaa;",
        tags$a("GitHub", href="https://github.com/datadiversitylab/BarrioMap",
               target="_blank", style="color:#1a5c3a;"),
        "  |  ", APP_URL,
        "  |  Map data from OpenStreetMap (ODbL)",
        tags$br(), "Hosting supported by the University of Arizona."
      )
    )
  )
)
