###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

ui <- navbarPage("ComMappeR",
                 theme = bslib::bs_theme(version = 4, bootswatch = "minty"), #https://bootswatch.com/
                 tabPanel("Welcome",
                          br(),
                          h4("Welcome to ComMappeR!", align = "center"),
                          br(),
                          h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Est pellentesque elit ullamcorper dignissim cras. Mauris pellentesque pulvinar pellentesque habitant morbi tristique senectus. Dui nunc mattis enim ut. In iaculis nunc sed augue lacus viverra vitae congue eu. Morbi tristique senectus et netus et. Ultrices eros in cursus turpis massa tincidunt dui ut. Turpis nunc eget lorem dolor sed viverra ipsum nunc. Facilisis sed odio morbi quis. Amet massa vitae tortor condimentum lacinia quis vel eros donec. Non quam lacus suspendisse faucibus interdum posuere. Tempus iaculis urna id volutpat lacus laoreet non curabitur. Pharetra massa massa ultricies mi quis hendrerit dolor magna. Faucibus et molestie ac feugiat sed lectus vestibulum mattis. Elementum nisi quis eleifend quam adipiscing. Accumsan tortor posuere ac ut. A lacus vestibulum sed arcu non odio euismod lacinia.", align = "center"),
                          h6("Turpis egestas integer eget aliquet nibh praesent tristique. Porta lorem mollis aliquam ut porttitor. Eget sit amet tellus cras adipiscing enim eu. In metus vulputate eu scelerisque. Nam at lectus urna duis convallis convallis tellus id. Ultrices dui sapien eget mi proin sed libero. Tortor condimentum lacinia quis vel eros donec ac odio. Ornare quam viverra orci sagittis eu volutpat odio. Senectus et netus et malesuada fames ac. Convallis a cras semper auctor neque vitae tempus quam. At risus viverra adipiscing at in tellus. Pellentesque adipiscing commodo elit at imperdiet dui. Placerat duis ultricies lacus sed. Risus nullam eget felis eget nunc lobortis mattis aliquam. Donec ultrices tincidunt arcu non sodales neque sodales. Sollicitudin nibh sit amet commodo nulla facilisi. Dis parturient montes nascetur ridiculus mus mauris. Eget lorem dolor sed viverra. Platea dictumst vestibulum rhoncus est pellentesque elit ullamcorper dignissim.", align = "center")
                          ),
                 tabPanel("Create your map!",
                          sidebarLayout(
                            sidebarPanel(
                              #input lat lng
                              numericInput("latitude", "Latitude", value= 0),   #removed lat long default
                              numericInput("longitude", "Longitude", value= 0), 
                              
                              # Scale selection box
                              numericInput("scale", "m/px", 10, min = 1, max = 160000),
                              
                              #Some text in the app for testing
                              textOutput("scaleL"),
                              textOutput("zoomL"),
                              
                              #button
                              actionButton("refresh", "Refresh"),
                              
                              # js to get width/height of map div
                              tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
                              
                              
                              #page size and screenshot
                              selectInput("pagesize", "Page Size:", choices = c("A4", "Letter"), selected = "A4"),
                              downloadButton("dl", "Screenshot")
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


