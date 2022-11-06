# https://gis.stackexchange.com/questions/198435/how-to-get-current-scale-of-a-leaflet-map
# https://github.com/MarcChasse/leaflet.ScaleFactor
# https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
# https://generativelandscapes.wordpress.com/2014/09/18/2d-site-model-from-open-street-maps-example-20-1/
# https://landarchbim.com/2015/12/01/elk-mapping-in-dynamo/
# https://toolbox.decodingspaces.net/urban-simulation-with-grasshopper/
# https://www.food4rhino.com/en/app/elk
# https://blogs.uoregon.edu/222s16/2016/05/19/elk-2-openstreetmap-data-and-site-analysis/
# http://fieldpapers.org/

library(shiny)
library(leaflet)
library(htmlwidgets)

ui <- fluidPage(
  leafletOutput("map"),
  br(),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(-122.4105513,37.78250256, zoom = 12) %>%
      onRender(
        "function(el,x){
        this.on('mousemove',function(e){
                        var map = e.target,
                        y = map.getSize().y,
                        x = map.getSize().x;
                        var maxMeters = map.containerPointToLatLng([0, y]).distanceTo( map.containerPointToLatLng([x,y]));
                        var MeterPerPixel = maxMeters/x ;
                        var scale = L.control.scale();    
                        Shiny.onInputChange('maxMeters', MeterPerPixel*scale.options.maxWidth);
                        console.log(MeterPerPixel*scale.options.maxWidth);   
                      });
        
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord);
                    });
                    
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', ['...','...'] )
                        Shiny.onInputChange('maxMeters', '...')
                    })
                }"
      )
  })
  
  output$out <- renderText({
 
      paste0("Lat: ", input$hover_coordinates[1], 
             "\nLng: ", input$hover_coordinates[2],
             "\nTest: ", input$maxMeters
      )
  })
}

shinyApp(ui, server)
