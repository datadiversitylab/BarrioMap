###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

ui <- navbarPage("Barrio Map",
                 theme = bslib::bs_theme(version = 4, bootswatch = "minty"), #https://bootswatch.com/
                 tabPanel("Welcome",
                          waiter::useWaiter(),
                          br(),
                          h4("Welcome to Barrio Map!", align = "center"),
                          br(),
                          h6("If you are searching for a simple map for community projects, this is the place. Made with urban planners, architects, and designers in mind, BarrioMaps will let you print to typical scales used in planning / design (1”=50’, etc.), print large sheet sizes (24”x36”, etc.), export in easily edited formats (pdf and vectors). Barrio Map with the goal of bridging the gap between open access mapping and formal planning efforts. Whether you’re an architecture student or a community member with a grassroots project, BarrioMap was made for you!", align = "center"),
                          br(),
                          h5("Technical Details:"),
                          h6("If you want to hear about the code, Barrio Map offers fundamental functionalities like finding locations by conducting searches or selecting coordinates. Users can select from predefined scales and export  the scaled maps to particular page sizes. Maps generated using Barrio Map are formal descriptions of particular sites with defined values of resolution, scales, and explicit information on distances. Barrio Map, in conjunction with OpenStreetMaps, is written in R, with functionalities largely leveraged from packages such as Leaflet and Shiny."),
                          br(),
                          h5("Why Barrio Map? why open source?"),
                          h6("Traditional planning practices rely on licensed software to filter and interpret information from open source maps. The process of using open software for formal mapping is time-consuming and complex. Thus, only professionals have access to the necessary skills and software."),
                          h6("Barrio Map is a comprehensive open source web-based mapping toolbox designed to bridge the gap between citizens and decision makers and add quantitative geo-referenced data to grassroots knowledge. The BarrioMap team believes that information is power, and should be accessible to all.")
                 ),
                 tabPanel("Create your map!",
                          h5("Build and export your own maps in five easy steps:", align = "center"),
                          tags$ul(
                            tags$li("Select your location. Define specific coordinates (i.e. Longitude and Latitude) or use a search bar in map by unchecking ‘define coordinates’."), 
                            tags$li("Pick a scale and DPI. Working on a building size site, then 1”=30’."), 
                            tags$li("Choose a page size."), 
                            tags$li("Click refresh"),
                            tags$li("Print the map!")
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              shinyjs::useShinyjs(),
                              checkboxInput("usecoordinates", "Define coordinates?", TRUE),
                              numericInput("latitude", "Latitude", value = 0),
                              numericInput("longitude", "Longitude", value = 0), 
                              numericInput("dpi", "DPI", value = 150), 
                              selectInput("page", "Page size", 
                                          choices = c("A4" = "a4", 
                                                      "A3" = "a3",
                                                      "Other" = "other")
                                          ),
                              selectInput("orientation", "Page orientation", 
                                          choices = c("Vertical" = "v", 
                                                      "Horizontal" = "h")
                              ),
                              numericInput("pageH", "Page height",
                                           value = 3508),
                              numericInput("pageW", "Page width",
                                           value = 2480),
                              numericInput("vpages", "Number of vertical pages", value = 1, min =1),
                              numericInput("hpages", "Number of horizontal pages", value = 1, min =1),
                              # Scale selection box
                              selectInput("scale", "Define scale (1:x m)", 
                                          choices = c("1:5,840" = 5840, 
                                                      "1:600" = 600,
                                                      "1:384" = 384
                                          )
                              ),
                              #button
                              downloadButton("print", "Download PDF")
                            ),
                            mainPanel(
                              leaflet::leafletOutput("map", height = "100%", width = "100%")
                            )
                          )),
                 tabPanel("About us",
                          br(),
                          h4("About us", align = "center"),
                          br(),
                          tags$ul(
                            tags$li("Sarthak Haldar: Sarthak is currently a Graduate Student of Data Science at the School of Information at the University of Arizona. Sarthak is involved in tasks related to implementing Machine Learning, Deep Learning Algorithms and Libraries, NLP, Data Mining concepts for processing datasets, and also performing data analysis by creating pivot tables and creating visualizations."),
                            tags$li("Mackenzie Waller: Mackenzie Waller is a landscape architect, urban designer and assistant professor at the College of Architecture, Planning and Landscape Architecture at the University of Arizona. Her work centers on environmental and spatial justice in the urban built environment. Her current research interests explore how the mediums of story, wildlife and play can serve as strategies to co-create desired futures. Her project experience began in environmental restoration and expanded to interdisciplinary approaches to neighborhood and urban public space design."),
                            tags$li("Cristian Roman-Palacios: Cristian is currently an Assistant Professor of Practice at the School of Information at the University of Arizona. Cristian uses statistics, bioinformatics, and machine learning to answer questions, primarily at the interface between ecology and evolution. However, he has recently started exploring more applied research that combines machine learning, GIS, and the creation of tools with direct applications on society.")
                          ),
                          br(),
                          h5("Acknowledgements"),
                          h6("Hosting Barrio Map wouldn’t be possible without the support of sponsor: University of Arizona who donate their funding, hosting space or hardware to accomplish the target.", align = "center")
                 ),
                 navbarMenu("More",
                            tabPanel("GitHub",
                                     br(),
                                     h4("GitHub", align = "center"),
                                     br(),
                                     h5("Barrio Map is an open-source web application for mapping and formal planning. Barrio Map is designed with simplicity, performance and usability in mind. It works efficiently across all major desktop and mobile platforms. The source code can be found at Github. You will need RStudio to run the application locally."), 
                                     tags$ul(
                                       tags$li("Create a New Project on RStudio and Select Version Control -> Git."),
                                       tags$li("Paste the repository link to import the relevant files."),
                                       tags$li("Run App")
                                     ),
                                     h5("You can also be a part of the project and help us by Creating Pull requests, providing feedback, reporting bugs, improving documentation and spreading the word about Barrio Map to your friends.")
                            ),
                            tabPanel("Additional resources",
                                     br(),
                                     h4("Additional resources", align = "center"),
                                     br(),
                                     h5("Please find other relevant applications available online:"),
                                     tags$ul(
                                       tags$li("RapiD (https://mapwith.ai/) : Uses artificial intelligence to predict features on high-resolution satellite imagery, these features are then populated in our RapiD map editing tool. It features AI based road layering. However users do need an official OSM account to use the tool. BarrioMap mitigates any hassle of creating an account."), 
                                       tags$li("Inkatlas (https://inkatlas.com/) : Allows to create own maps for print, whether its for planning a bike trip or publishing a book. It is a paid proprietary application unlike BarrioMap which is Open Souce and Free."), 
                                       tags$li("PrintMaps (https://www.printmaps.net/ ): The Printmaps editor lets you create high resolution maps in SVG, or PNG or PSD (Adobe Photoshop) format in 300 dpi. Printing the map needs a payment unlike BarrioMap which is Open Souce and Free."),
                                       tags$li("Milvusmap (http://milvusmap.eu/ ) : Allows to create and print maps in pdf format. This application includes very little overlap with traditional planning practices unlike BarrioMap."),
                                       tags$li("FieldPapers ( http://fieldpapers.org/compose#10/33.5345/-111.9603): Allows to create and print maps in pdf format. This application includes very little overlap with traditional planning practices unlike BarrioMap.")
                                     )
                            ))
)


