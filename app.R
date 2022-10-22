# https://gis.stackexchange.com/questions/198435/how-to-get-current-scale-of-a-leaflet-map
# https://github.com/MarcChasse/leaflet.ScaleFactor

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
                        var scale = L.control.scale().addTo(map);    
                        Shiny.onInputChange('maxMeters', MeterPerPixel*scale.options.maxWidth);
                      });
        
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord);
                    });
                    
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                        Shiny.onInputChange('maxMeters', null)

                    })
                }"
      )
  })
  
  output$out <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", input$hover_coordinates[1], 
             "\nLng: ", input$hover_coordinates[2],
             "\nTest: ", input$maxMeters
      )
    }
  })
}

shinyApp(ui, server)