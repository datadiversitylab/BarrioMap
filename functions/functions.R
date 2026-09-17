getBox <- function(m){
  view <- m$x$setView
  lat <- view[[1]][1]
  lng <- view[[1]][2]
  zoom <- view[[2]]
  zoom_width <- 360 / 2^zoom
  lng_width <- m$width / 256 * zoom_width
  lat_height <- m$height / 256 * zoom_width
  return(c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2))
}


adjustlong <- function(map = recMap,  nRecLon){
  
  bx1 <- getBox(map)
  center <- map$x$setView[[1]][2]
  
  #Estimate distance
  TotDist <- abs(bx1[1] - bx1[2])
  
  if((nRecLon %% 2) == 0) {
    #If nRec is even
    boundaries_west <- list(center)
    for(i in 2:(1+round(nRecLon/2))){
      boundaries_west[[i]] <- boundaries_west[[i-1]] - TotDist
    }
    
    boundaries_east <- list(center)
    for(i in 2:(1+round(nRecLon/2))){
      boundaries_east[[i]] <- boundaries_east[[i-1]] + TotDist
    }
    limits <- sort(c(unlist(boundaries_west), unlist(boundaries_east)))
    
    
  }else{ 
    #If nRecLon is odd
    boundaries_west <- list()
    for(i in 1:ceiling(nRecLon/2)){
      if(i ==1){
        boundaries_west[[i]] <- bx1[1]
      }else{
        boundaries_west[[i]] <- boundaries_west[[i-1]] - TotDist
      }
    }
    
    boundaries_east <- list()
    for(i in 1:ceiling(nRecLon/2)){
      if(i ==1){
        boundaries_east[[i]] <- bx1[2]
      }else{
        boundaries_east[[i]] <- boundaries_east[[i-1]] + TotDist
      }
    }
    limits <- sort(c(unlist(boundaries_west), unlist(boundaries_east)))
  }
  
  unique(limits)
  
}
adjustlat <- function(map = recMap,  nRecVert){
  
  bx1 <- getBox(map)
  center <- map$x$setView[[1]][1]
  
  #Estimate distance
  TotDist <- abs(bx1[3] - bx1[4])
  
  if((nRecVert %% 2) == 0) {
    #If nRecVert is even
    
    boundaries_south <- list(center)
    for(i in 2:(1+round(nRecVert/2))){
      boundaries_south[[i]] <- boundaries_south[[i-1]] - TotDist
    }
    
    boundaries_north <- list(center)
    for(i in 2:(1+round(nRecVert/2))){
      boundaries_north[[i]] <- boundaries_north[[i-1]] + TotDist
    }
    limits <- sort(c(unlist(boundaries_south), unlist(boundaries_north)))
    
    
  }else{ 
    #If nRecVert is odd
    boundaries_south <- list()
    for(i in 1:ceiling(nRecVert/2)){
      if(i ==1){
        boundaries_south[[i]] <- bx1[3]
      }else{
        boundaries_south[[i]] <- boundaries_south[[i-1]] - TotDist
      }
    }
    
    boundaries_north <- list()
    for(i in 1:ceiling(nRecVert/2)){
      if(i ==1){
        boundaries_north[[i]] <- bx1[4]
      }else{
        boundaries_north[[i]] <- boundaries_north[[i-1]] + TotDist
      }
    }
    limits <- sort(c(unlist(boundaries_south), unlist(boundaries_north)))
  }
  
  unique(limits)
  
}
returnRectangles <- function(map = recMap, nRecLon, nRecVert ){
  
  if(nRecLon == 1 & nRecVert == 1){
    coords <- getBox(map)
    rectangles <- cbind(coords[1] , coords[2], coords[3] , coords[4])
    rectangles
  }
  
  if(nRecLon > 1 & nRecVert > 1) {
    lng <- adjustlong(map = map,  nRecLon = nRecLon)
    lat <- adjustlat(map = map,  nRecVert = nRecVert)
    
    rectangles <- do.call(rbind,lapply(1:(nRecLon), function(i){
      do.call(rbind, lapply(1:nRecVert, function(j) {
        c(lng[c(i, i+1)], lat[c(j, j+1)])
      }))
    }))
    rectangles
  }
  
  if(nRecLon ==1 & nRecVert >1){
    lng <- getBox(map)[1:2]
    lat <- adjustlat(map = map,  nRecVert = nRecVert)
    
    rectangles <- do.call(rbind,lapply(1:(nRecLon), function(i){
      do.call(rbind, lapply(1:nRecVert, function(j) {
        c(lng[c(i, i+1)], lat[c(j, j+1)])
      }))
    }))
    rectangles
  }
  if(nRecLon > 1 & nRecVert == 1) {
    lng <- adjustlong(map = map,  nRecLon = nRecLon) 
    lat <- getBox(map)[3:4]
    
    rectangles <- do.call(rbind,lapply(1:(nRecLon), function(i){
      do.call(rbind, lapply(1:nRecVert, function(j) {
        c(lng[c(i, i+1)], lat[c(j, j+1)])
      }))
    }))
    rectangles
  }
  
  return(rectangles)
}



osmdata_plot <- function(bbox_df,
                         folder = "www",
                         prefix = "test",
                         width = 4,
                         height = 4) {
  pdf(
    file = paste0(folder, "/", prefix , ".pdf"),
    width = width,
    height = height,
    onefile = TRUE
  )
  for (i in 1:nrow(bbox_df)) {
    q1 <- opq(bbox = bbox_df[i,]) %>%
      add_osm_feature(key = 'highway', value = 'cycleway')
    cway_sev <- osmdata_sp(q1)
    sp::plot(cway_sev$osm_lines)
  }
  dev.off()
}


# Session-scoped cache directory for osmextract downloads. Everything
# under tempdir() is cleared automatically when the R process ends, so
# a region is only ever re-downloaded in a fresh session.
osmextract_cache_dir <- function() {
  cache_dir <- file.path(tempdir(), "barrio_osmextract_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_dir
}

# Both overview_bbox and each panel's bb in server.R are already ordered
# as (xmin, ymin, xmax, ymax), which is what sf::st_bbox expects.
bbox_to_sf_order <- function(bb) {
  c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
}

# Fetch roads and buildings for a bbox using osmextract instead of live
# Overpass queries. The first request for a region downloads and caches
# the matching Geofabrik extract for the rest of the session; every
# later call, for any bbox inside that same region, reads from the
# cached file and clips to the requested bbox.
getOsmFeatures <- function(bb, features) {
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(bbox_to_sf_order(bb), crs = 4326))
  cache_dir <- osmextract_cache_dir()

  roads <- NULL
  buildings <- NULL

  if ("roads" %in% features) {
    roads <- osmextract::oe_get(
      place               = bbox_sf,
      layer               = "lines",
      download_directory  = cache_dir,
      boundary            = bbox_sf,
      boundary_type       = "clipsrc",
      quiet               = TRUE
    )
    roads <- roads[!is.na(roads$highway), ]
  }

  if ("buildings" %in% features) {
    buildings <- osmextract::oe_get(
      place               = bbox_sf,
      layer               = "multipolygons",
      download_directory  = cache_dir,
      boundary            = bbox_sf,
      boundary_type       = "clipsrc",
      quiet               = TRUE
    )
    buildings <- buildings[!is.na(buildings$building), ]
  }

  list(roads = roads, buildings = buildings)
}

# Pre-download and cache the Geofabrik extracts for a fixed set of
# places before any user connects. Called once from app.R at startup.
# Every export that falls inside one of these regions then reads from
# the cache instead of paying the country/state download on first use.
# Each place name is resolved by osmextract's own place matching
# (see oe_match()), so "Arizona" or "Tucson" both work; pick whatever
# level covers the areas your users actually map.
preloadOsmRegions <- function(places) {
  cache_dir <- osmextract_cache_dir()

  for (place in places) {
    tryCatch({
      message("Pre-loading OSM extract for: ", place)
      osmextract::oe_get(
        place               = place,
        download_directory  = cache_dir,
        download_only       = TRUE,
        quiet               = TRUE
      )
    }, error = function(e) {
      warning("Could not pre-load OSM extract for ", place, ": ", conditionMessage(e))
    })
  }
}



#Calculate the number of screen pixels that correspond to a given distance in meters
meter2screenpixel <- function(meter, orient = "v", zoomlevel, latitude) {
  # Meters per pixel at this zoom and latitude (OSM Web Mercator convention).
  # Horizontal resolution shrinks with cos(latitude); vertical does not.
  # https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
  metresPerPixel.h <- 40075016.686 * abs(cos(latitude * pi / 180)) / 2^(zoomlevel + 8)
  metresPerPixel.v <- 40075016.686 / 2^(zoomlevel + 8)

  pixSizeGeodesic <- ifelse(orient == "v", metresPerPixel.v, metresPerPixel.h)
  pixel <- meter / pixSizeGeodesic
  return(pixel)
}


# helper function that converts "1 inch : scale_meters_per_inch" 
# into a valid Leaflet zoom level, accounting for latitude and DPI.
calcZoom <- function(scale_meters_per_inch, lat, dpi = 300) {
  # Convert latitude to radians
  phi <- lat * pi / 180
  
  # Web Mercator base resolution at zoom=0 (equator)
  baseRes <- 156543.0339
  
  # If 1 inch = scale_meters_per_inch in reality, 
  # and 1 inch = dpi pixels on the PDF,
  # then we want scale_meters_per_inch / dpi meters/pixel.
  needed_res <- scale_meters_per_inch * 0.0254 / dpi
  
  # Web Mercator approximate formula:
  # resolution(z, phi) = (baseRes * cos(phi)) / 2^z
  # needed_res         = (baseRes * cos(phi)) / 2^z
  # => 2^z = (baseRes * cos(phi)) / needed_res
  # => z   = log2((baseRes * cos(phi)) / needed_res)
  z <- log2((baseRes * cos(phi)) / needed_res)
  
  # Constrain zoom to typical Leaflet range
  z <- max(min(z, 22), 0)
  
  return(z)
}

# Named color palettes for layer selection. These are available in both
# server.R (for the PDF) and ui.R (for the selectInput choices), since
# app.R sources functions.R before sourcing either of them.
ROAD_COLORS <- c(
  "Dark gray"    = "#555555",
  "Black"        = "#000000",
  "Navy"         = "#1d3557",
  "Warm brown"   = "#774936",
  "Forest green" = "#2d6a4f"
)

BLD_FILL_COLORS <- c(
  "Light gray"   = "#f2f2f2",
  "White"        = "#ffffff",
  "Warm white"   = "#faf8f5",
  "Soft blue"    = "#e8f4f8",
  "Soft green"   = "#e8f5e9",
  "Sand"         = "#fdf3dc"
)

BLD_BORDER_COLORS <- c(
  "Medium gray"  = "#aaaaaa",
  "Dark gray"    = "#666666",
  "Black"        = "#000000",
  "Brown"        = "#774936",
  "Slate"        = "#4a5568"
)

# Generate a random 6-character alphanumeric map code.
generateMapCode <- function() {
  chars <- c(LETTERS, as.character(0:9))
  paste0(sample(chars, 6, replace = TRUE), collapse = "")
}

# Save map parameters to map_codes/{code}.json. Creates the directory
# if it does not exist. The saved_at timestamp drives expiry.
saveMapCode <- function(code, params) {
  dir <- file.path(getwd(), "map_codes")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  params$saved_at <- as.numeric(Sys.time())
  writeLines(
    jsonlite::toJSON(params, auto_unbox = TRUE),
    file.path(dir, paste0(code, ".json"))
  )
}

# Retrieve map parameters by code. Returns NULL if not found or expired.
loadMapCode <- function(code, max_days = 30) {
  code <- toupper(trimws(code))
  f <- file.path(getwd(), "map_codes", paste0(code, ".json"))
  if (!file.exists(f)) return(NULL)
  params <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(params)) return(NULL)
  age_days <- (as.numeric(Sys.time()) - params$saved_at) / 86400
  if (age_days > max_days) {
    file.remove(f)
    return(NULL)
  }
  params
}

# Delete code files older than max_days. Called once at app startup.
cleanExpiredCodes <- function(max_days = 30) {
  dir <- file.path(getwd(), "map_codes")
  if (!dir.exists(dir)) return(invisible(NULL))
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  for (f in files) {
    tryCatch({
      params <- jsonlite::fromJSON(f)
      if ((as.numeric(Sys.time()) - params$saved_at) / 86400 > max_days)
        file.remove(f)
    }, error = function(e) file.remove(f))
  }
  invisible(NULL)
}
