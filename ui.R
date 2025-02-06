###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

ui <- navbarPage(
  title = "Barrio Map",
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

  # HOME TAB
  tabPanel(
    shinybusy::add_busy_spinner(spin = "dots",
                     timeout = 10,
                     height = "25px",
                     width = "25px"),
    "Welcome",
    fluidPage(
      # Centered container
      tags$div(
        style = "max-width: 800px; margin: 0 auto; padding: 40px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333; line-height: 1.6;",
        
        # Title and subtitle
        tags$h1(
          style = "font-size: 48px; font-weight: 300; margin-bottom: 10px; text-align: center;",
          "Barrio Map"
        ),
        tags$h3(
          style = "font-size: 24px; font-weight: 300; margin-bottom: 40px; text-align: center; color: #555555;",
          "A Community-Focused Tool for Planning & Design"
        ),
        
        # BOX 1
        tags$div(
          style = "background-color: #f9f9f9; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
          tags$h4("Welcome to Barrio Map!", align = "center"),
          tags$p(
            "If you are searching for a simple map for community projects, this is the place. Made with urban planners, architects, and designers in mind, Barrio Map helps you print maps to typical scales used in planning (1\"=50’, etc.), print large sheet sizes (24\"x36\", etc.), and export them in easily edited formats (PDF and vectors)."
          ),
          tags$p(
            "Barrio Map bridges the gap between open-access mapping and formal planning efforts—whether you’re an architecture student or a community member leading a grassroots project. We aim to make professional-style maps more accessible while staying open-source."
          )
        ),
        
        # BOX 2
        tags$div(
          style = "background-color: #ffffff; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
          tags$h4("Technical Details"),
          tags$p(
            "Barrio Map includes fundamental functionalities like location searches or coordinate-based navigation. Users can choose predefined scales, select page sizes, and export maps at specific resolutions. Created in R, Barrio Map leverages Leaflet, Shiny, and other packages to deliver a straightforward and reliable mapping experience."
          ),
          tags$h4("Why Barrio Map? Why Open Source?"),
          tags$p(
            "Traditional planning practices often rely on licensed software to filter and interpret open-source map data. Our goal is to simplify the process by offering a web-based, open-source platform that anyone can use—reducing the barriers to producing professional-quality maps."
          )
        ),
        
        # BOX 3
        tags$div(
          style = "background-color: #f9f9f9; border-radius: 8px; padding: 20px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
          tags$h4("Ready to Explore?"),
          tags$p(
            "Navigate to the 'Create your map!' tab to start generating scalable, printable maps. Simply select a location, scale, and page size, then export or print as needed. We hope Barrio Map empowers communities, students, and professionals to share and develop spatial knowledge."
          )
        )
      )
    )
  ),
  
  # CREATE YOUR MAP TAB
  tabPanel(
    "Create your map!",
    fluidPage(
      tags$head(
        tags$style(HTML("
        /* A subtle box shadow for the map container */
        #mapContainer {
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
          position: relative;
        }
        
        /* An absolute panel with a semi-transparent white background */
        .mapControlPanel {
          background-color: rgba(255, 255, 255, 0.9);
          padding: 15px 20px;
          border-radius: 8px;
          box-shadow: 0 1px 4px rgba(0,0,0,0.2);
        }
        
        /* Steps list spacing */
        .steps-list li {
          margin-bottom: 10px;
        }
        
        /* Big box (well) for instructions */
        .instructions-box {
          max-width: 800px;
          margin: 0 auto 20px auto;
          padding: 30px;
          background-color: #f8f9fa;
          border: 1px solid #ccc;
          border-radius: 6px;
        }
        
        /* Enhanced title styling */
        .instructions-title {
          font-size: 1.75em;
          font-weight: bold;
          text-align: center;
          margin-bottom: 20px;
        }
      "))
      ),
      
      # Main map container
      tags$div(
        id = "mapContainer",
        leafletOutput("map", height = "600px"),
        
        # Primary controls (location, scale, printing, etc.)
        absolutePanel(
          id = "mapControls",
          class = "mapControlPanel",
          top = 40, left = 40, width = 300,
          draggable = TRUE,
          
          useShinyjs(),
          
          # Toggle for defining coordinates or search
          checkboxInput("usecoordinates", "Define coordinates", TRUE),
          
          # Frame fix
          checkboxInput("fixframe", "Fix frame", value = FALSE),
          
          # Lat/Long inputs OR a search box, using conditionalPanel
          conditionalPanel(
            condition = "input.usecoordinates == true",
            fluidRow(
              column(
                width = 6,
                numericInput("latitude", "Lat", value = 0, width = "100%")
              ),
              column(
                width = 6,
                numericInput("longitude", "Lon", value = 0, width = "100%")
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.usecoordinates == false",
            textInput("searchbox", "Search for a location", "")
          ),
          
          # Basic page and orientation settings
          selectInput(
            "page", "Page size",
            choices = c("A4" = "a4", "A3" = "a3", "Other" = "other")
          ),
          selectInput(
            "orientation", "Page orientation",
            choices = c("Vertical" = "v", "Horizontal" = "h")
          ),
          
          numericInput("pageH", "Page height", value = 0.267),
          numericInput("pageW", "Page width",  value = 0.18),
          
          # Scale input
          selectInput(
            "scale", "Define scale (1:x m)",
            choices = c("1:5,840" = 5840, "1:600" = 600, "1:384" = 384)
          ),
          
          # Adjusting OSM layers
          checkboxInput("showLayerSettings", "Select layers", FALSE),
          
          # More settings
          checkboxInput("showMoreSettings", "More settings", FALSE),
          
          # Download PDF button
          downloadButton("print", "Download PDF", class = "btn-primary")
        ),
        
        # The advanced printing settings panel, shown only if showMoreSettings == TRUE
        conditionalPanel(
          condition = "input.showMoreSettings == true",
          absolutePanel(
            id = "mapControlsAdvanced",
            class = "mapControlPanel",
            top = 40, left = 360, width = 300,
            draggable = TRUE,
            
            tags$h4("Advanced Settings", style = "margin-top: 0;"),
            
            # DPI input
            numericInput("dpi", "DPI (dots per inch)", 300, min = 72, step = 1),
            
            # Additional pages
            numericInput("vpages", "Number of vertical pages", value = 1, min = 1),
            numericInput("hpages", "Number of horizontal pages", value = 1, min = 1)
          )
        ),
        
        # The layers settings panel, shown only if showLayerSettings == TRUE
        conditionalPanel(
          condition = "input.showLayerSettings == true",
          absolutePanel(
            id = "mapControlsLayers",
            class = "mapControlPanel",
            top = 40, left = 680, width = 320,
            draggable = TRUE,
            
            # Main heading with minimal margin
            tags$h4("Select OSM Layers to Export", style = "margin-top: 5px; margin-bottom: 10px;"),
            
            checkboxGroupInput(
              "features", 
              "Select features:",
              choices = c("Roads" = "roads", "Buildings" = "buildings"),
              selected = c("roads", "buildings")
            )
          )
        )
        
      )
    )
  ),
  
  # ABOUT US TAB
  tabPanel(
    "About us",
    fluidPage(
      tags$div(
        style = "max-width: 800px; margin: 0 auto; padding: 40px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333;",
        tags$h4("About us", align = "center"),
        tags$ul(
          tags$li("Sarthak Haldar: Graduate Student in Data Science at the School of Information, University of Arizona—works with ML, NLP, Data Mining, and visualization."),
          tags$li("Mackenzie Waller: Landscape architect, urban designer, and assistant professor at CAPLA, University of Arizona—researches environmental/spatial justice, community-led design strategies, and interdisciplinary approaches to public space."),
          tags$li("Cristian Roman-Palacios: Assistant Professor of Practice at the School of Information, University of Arizona—uses statistics, bioinformatics, and ML for ecological and evolutionary questions, and applies those techniques to GIS and societal tools like Barrio Map.")
        ),
        tags$br(),
        tags$h5("Acknowledgements", align = "center"),
        tags$h6(
          "Hosting Barrio Map is made possible by support from the University of Arizona. We appreciate their help with funding, hosting space, or hardware resources to advance our vision for accessible mapping."
        )
      )
    )
  ),
  
  # MORE -> GITHUB TAB
  navbarMenu("More",
             tabPanel(
               "GitHub",
               fluidPage(
                 tags$div(
                   style = "max-width: 800px; margin: 0 auto; padding: 40px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333;",
                   tags$h4("GitHub", align = "center"),
                   tags$p(
                     "Barrio Map is an open-source web application for mapping and formal planning. Designed for simplicity, performance, and usability, it runs on major desktop and mobile platforms. The source code is available on GitHub, and you’ll need RStudio to run it locally."
                   ),
                   tags$ul(
                     tags$li("Create a new project in RStudio and select Version Control -> Git."),
                     tags$li("Paste the repository link to import the relevant files."),
                     tags$li("Run the application.")
                   ),
                   tags$p(
                     "We welcome pull requests, bug reports, improvements to documentation, and general feedback. Spread the word about Barrio Map to your network so we can keep expanding and improving this resource."
                   )
                 )
               )
             ),
             
             # MORE -> ADDITIONAL RESOURCES TAB
             tabPanel(
               "Additional resources",
               fluidPage(
                 tags$div(
                   style = "max-width: 800px; margin: 0 auto; padding: 40px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333;",
                   tags$h4("Additional resources", align = "center"),
                   tags$p("Below are other relevant mapping applications available online, each with different focuses and features:"),
                   tags$ul(
                     tags$li("RapiD (https://mapwith.ai/): Uses AI for feature detection in satellite imagery; requires an official OSM account to use."),
                     tags$li("Inkatlas (https://inkatlas.com/): Helps create print-ready maps but is a paid, proprietary service."),
                     tags$li("PrintMaps (https://www.printmaps.net/): Allows exporting maps in multiple formats for a fee."),
                     tags$li("Milvusmap (http://milvusmap.eu/): Exports PDFs of maps but offers limited overlap with formal planning practices."),
                     tags$li("FieldPapers (http://fieldpapers.org/compose#10/33.5345/-111.9603): Creates printable PDF maps, though less aligned with traditional planning approaches.")
                   )
                 )
               )
    )
  )
)



