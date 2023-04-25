library(osmdata)

q1 <- opq(bbox = c (-110, 32.1, -111, 32.2)) %>%
  add_osm_feature(key = 'highway'
                  , value = 'cycleway'
                  ) 
cway_sev <- osmdata_sp(q1)
sp::plot(cway_sev$osm_lines)



shapefiles <- opq(bbox = getbb("Limete, Kinshasa")) %>%
  add_osm_feature(key = "highway", value = "cycleway") %>%
  osmdata_sf()

mad_map <- get_map(getbb("Limete, Kinshasa"), maptype = "roadmap", source = "osm", scale = 2)
testMap <- ggmap(mad_map) +
  geom_sf(
    data = shapefiles$osm_lines,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(x = "", y = "")

ggsave(testMap, filename =  "test.pdf")
