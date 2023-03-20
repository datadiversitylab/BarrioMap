###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
ui <- navbarPage("BarrioMap",
                 theme = bslib::bs_theme(version = 4, bootswatch = "minty"), #https://bootswatch.com/
                 tabPanel("Welcome",
                          br(),
                          h4("Welcome to Barrio Map!", align = "center"),
                          br(),
                          h6("Barrio Map is a web mapping application offered by University of Arizona. It bridges the gap between Open Access Mapping and Formal Planning Efforts. We aim to generate an open access tool that enables the formal description of resolution, distances and scales in static maps under an open source framework. The resulting maps are intended to further close the gap between citizens and decision makers, to add quantitative geo-referenced data to grassroots knowledge. The individuals and communities that are closest to the problems caused by environmental justice and spatial inequity are also those that can drive long term solutions.  Proprietary software does not benefit from existing citizen data and limits new contributions of the ongoing and direct community mapping efforts. Development of tools that allow for easy access and use along with high performance and functionality holds the potential to change the way planners work.", align = "center"),
                          h6("A chasm separates open source mapping and traditional planning practices. Current processes rely upon planning and design professionals to filter information from open source maps into license software for access to the information in a legible format. This extra step is both time consuming, and places the final interpretation of the information in the hands of individuals with access to the skills and software. In fact, tools that would allow for traditional planning under an open source framework are scattered across different software, making it much more difficult to use effectively by non-experts. Fee based licensed software integrates relevant tools into a single environment. This software is costly and has a steep learning curve. To our knowledge, none of the existing open source online tools integrate all the fundamental traditional planning practices. Here, we focus on developing a comprehensive open source web based mapping toolset called Barrio Map that has the functionality of traditional planning practices and a user friendly interface.", align = "center"),
                          
                          h6("Hosting Barrio Map wouldn’t be possible without the support of sponsor: University of Arizona and seed contributors: Mackenzie Waller University of Arizona, Department of Landscape Architecture; Cristian Román-Palacios University of Arizona, Assistant Professor of Practice, UArizona School of Information; Sarthak Haldar University of Arizona, Graduate Student Data Science, UArizona School of Information who donate their time, effort, hosting space or hardware to accomplish the target.", align = "center")
                          ),                 
                          tabPanel("Create your map!",
                          sidebarLayout(
                            sidebarPanel(
                              shinyjs::useShinyjs(),
                              checkboxInput("usecoordinates", "Define coordinates?", TRUE),
                              #input lat lng
                              numericInput("latitude", "Latitude", value = 0),
                              numericInput("longitude", "Longitude", value = 0), 
                              numericInput("dpi", "DPI", value = 150), 
                              
                              # Scale selection box
                              selectInput("scale", "Define scale (1:x m)", 
                                          choices = c("1:5,840" = 5840, 
                                                      "1:600" = 600,
                                                      "1:384" = 384
                                                      )
                                          ),

                              #Some text in the app for testing
                              textOutput("zoomL"),
                              textOutput("resolutionL"),
                              textOutput("scaleL"),
                              
                              #button
                              actionButton("refresh", "Refresh"),
                              
                              tags$head(tags$script(src = jsfile)),
                              
                              # js to get width/height of map div
         #                     tags$head(tags$script('
          #              var dimension = [0, 0];
          #              $(document).on("shiny:connected", function(e) {
          #              dimension[0] = document.getElementById("map").clientWidth;
          #              dimension[1] = document.getElementById("map").clientHeight;
          #              Shiny.onInputChange("dimension", dimension);
          #              });
          #              $(window).resize(function(e) {
          #              dimension[0] = document.getElementById("map").clientWidth;
          #              dimension[1] = document.getElementById("map").clientHeight;
          #              Shiny.onInputChange("dimension", dimension);
          #              });
          #              ')),
                              
                              
                              #page size and screenshot
                              #selectInput("pagesize", "Page Size:", choices = c("A4", "Letter"), selected = "A4"),
                              #downloadButton("dl", "Export PDF"),
                              #downloadButton("dl2", "Export PNG")
                              #downloadButton("dl3", "Export HTML")
                            ),
                            mainPanel(
                              leaflet::leafletOutput("map", height = "500px", width = "100%")
                            )
                          )),
                 tabPanel("About us",
                          br(),
                          h4("About us", align = "center"),
                          br(),
                          h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Laoreet suspendisse interdum consectetur libero. Sollicitudin aliquam ultrices sagittis orci a scelerisque purus semper. Integer feugiat scelerisque varius morbi enim nunc faucibus a pellentesque. Ultrices sagittis orci a scelerisque. Aliquam etiam erat velit scelerisque in dictum non consectetur. Vestibulum lectus mauris ultrices eros in cursus turpis massa tincidunt. Pharetra magna ac placerat vestibulum lectus mauris ultrices eros. Varius quam quisque id diam vel quam elementum. Egestas tellus rutrum tellus pellentesque eu tincidunt tortor aliquam. Varius quam quisque id diam vel quam elementum pulvinar. Fermentum posuere urna nec tincidunt.", align = "center"),
                          h6("Id donec ultrices tincidunt arcu non sodales neque sodales. Mi proin sed libero enim sed faucibus. Cursus sit amet dictum sit amet. Aliquam eleifend mi in nulla posuere sollicitudin. Blandit libero volutpat sed cras ornare. Duis at tellus at urna. Turpis egestas integer eget aliquet nibh praesent tristique magna. Feugiat in fermentum posuere urna. Sociis natoque penatibus et magnis dis. At in tellus integer feugiat. Luctus accumsan tortor posuere ac ut consequat semper. Quis commodo odio aenean sed. Magna etiam tempor orci eu lobortis elementum. Egestas dui id ornare arcu. Proin fermentum leo vel orci porta non pulvinar. Velit euismod in pellentesque massa placerat. Pellentesque diam volutpat commodo sed egestas egestas fringilla phasellus. Eu augue ut lectus arcu bibendum at. Pellentesque sit amet porttitor eget dolor morbi non.", align = "center"),
                          h6("Mi tempus imperdiet nulla malesuada. Adipiscing commodo elit at imperdiet dui accumsan sit. Fermentum odio eu feugiat pretium nibh ipsum consequat nisl vel. Nulla pellentesque dignissim enim sit amet venenatis urna cursus eget. Eu facilisis sed odio morbi quis commodo odio. Gravida arcu ac tortor dignissim convallis aenean. Aliquam sem et tortor consequat id porta nibh venenatis cras. Etiam erat velit scelerisque in dictum non consectetur. Dolor sit amet consectetur adipiscing elit pellentesque. Eget nullam non nisi est sit amet facilisis magna etiam. Vestibulum mattis ullamcorper velit sed ullamcorper morbi tincidunt ornare massa. Lorem donec massa sapien faucibus et molestie. Facilisis sed odio morbi quis commodo odio aenean sed. Semper risus in hendrerit gravida rutrum quisque. Tortor consequat id porta nibh venenatis cras. Volutpat blandit aliquam etiam erat velit. Commodo viverra maecenas accumsan lacus vel. Lorem mollis aliquam ut porttitor leo a diam. Nunc id cursus metus aliquam eleifend. Suspendisse faucibus interdum posuere lorem ipsum dolor sit amet.", align = "center")
                 ),
                 navbarMenu("More",
                            tabPanel("GitHub"),
                            tabPanel("Additional resources"))
)


