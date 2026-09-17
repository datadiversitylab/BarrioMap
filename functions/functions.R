# ============================================================
# functions.R - Shared functions and constants for BarrioMap
# ============================================================

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !all(is.na(x)) && nzchar(x[1])) x else y


# -----------------------------------------------------------
# Scale geometry (offscreen Leaflet approach)
# -----------------------------------------------------------

getBox <- function(m) {
  view      <- m$x$setView
  lat       <- view[[1]][1]
  lng       <- view[[1]][2]
  zoom      <- view[[2]]
  zoom_width <- 360 / 2^zoom
  lng_width  <- m$width  / 256 * zoom_width
  lat_height <- m$height / 256 * zoom_width
  c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2)
}

adjustlong <- function(map = recMap, nRecLon) {
  bx1    <- getBox(map)
  center <- map$x$setView[[1]][2]
  TotDist <- abs(bx1[1] - bx1[2])
  if ((nRecLon %% 2) == 0) {
    bw <- list(center); for (i in 2:(1 + round(nRecLon/2))) bw[[i]] <- bw[[i-1]] - TotDist
    be <- list(center); for (i in 2:(1 + round(nRecLon/2))) be[[i]] <- be[[i-1]] + TotDist
    limits <- sort(c(unlist(bw), unlist(be)))
  } else {
    bw <- list(); for (i in 1:ceiling(nRecLon/2)) bw[[i]] <- if (i==1) bx1[1] else bw[[i-1]] - TotDist
    be <- list(); for (i in 1:ceiling(nRecLon/2)) be[[i]] <- if (i==1) bx1[2] else be[[i-1]] + TotDist
    limits <- sort(c(unlist(bw), unlist(be)))
  }
  unique(limits)
}

adjustlat <- function(map = recMap, nRecVert) {
  bx1    <- getBox(map)
  center <- map$x$setView[[1]][1]
  TotDist <- abs(bx1[3] - bx1[4])
  if ((nRecVert %% 2) == 0) {
    bs <- list(center); for (i in 2:(1 + round(nRecVert/2))) bs[[i]] <- bs[[i-1]] - TotDist
    bn <- list(center); for (i in 2:(1 + round(nRecVert/2))) bn[[i]] <- bn[[i-1]] + TotDist
    limits <- sort(c(unlist(bs), unlist(bn)))
  } else {
    bs <- list(); for (i in 1:ceiling(nRecVert/2)) bs[[i]] <- if (i==1) bx1[3] else bs[[i-1]] - TotDist
    bn <- list(); for (i in 1:ceiling(nRecVert/2)) bn[[i]] <- if (i==1) bx1[4] else bn[[i-1]] + TotDist
    limits <- sort(c(unlist(bs), unlist(bn)))
  }
  unique(limits)
}

returnRectangles <- function(map = recMap, nRecLon, nRecVert) {
  if (nRecLon == 1 & nRecVert == 1) {
    coords <- getBox(map)
    return(cbind(coords[1], coords[2], coords[3], coords[4]))
  }
  lng <- if (nRecLon > 1) adjustlong(map, nRecLon) else getBox(map)[1:2]
  lat <- if (nRecVert > 1) adjustlat(map,  nRecVert) else getBox(map)[3:4]
  rectangles <- do.call(rbind, lapply(1:nRecLon, function(i) {
    do.call(rbind, lapply(1:nRecVert, function(j) {
      c(lng[c(i, i+1)], lat[c(j, j+1)])
    }))
  }))
  rectangles
}

meter2screenpixel <- function(meter, orient = "v", zoomlevel, latitude) {
  metresPerPixel.h <- 40075016.686 * abs(cos(latitude * pi / 180)) / 2^(zoomlevel + 8)
  metresPerPixel.v <- 40075016.686 / 2^(zoomlevel + 8)
  pixSizeGeodesic  <- ifelse(orient == "v", metresPerPixel.v, metresPerPixel.h)
  meter / pixSizeGeodesic
}

calcZoom <- function(scale_meters_per_inch, lat, dpi = 300) {
  phi        <- lat * pi / 180
  needed_res <- scale_meters_per_inch * 0.0254 / dpi
  z          <- log2(156543.0339 * cos(phi) / needed_res)
  max(min(z, 22), 0)
}

bbox_to_sf_order <- function(bb) {
  c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
}


# -----------------------------------------------------------
# OSM layer definitions
# Each entry: label, default (on/off), source OSM layer,
# filter function, geometry type, and default colors.
# -----------------------------------------------------------

LAYER_DEFS <- list(
  roads = list(
    label   = "Roads",
    default = TRUE,
    source  = "lines",
    filter  = function(x) !is.na(x$highway),
    type    = "line",
    color   = "#555555",
    lwd     = 0.35,
    alpha   = 0.85
  ),
  waterways = list(
    label   = "Waterways",
    default = FALSE,
    source  = "lines",
    filter  = function(x) !is.na(x$waterway),
    type    = "line",
    color   = "#3a87c8",
    lwd     = 0.4,
    alpha   = 0.9
  ),
  buildings = list(
    label   = "Buildings",
    default = TRUE,
    source  = "multipolygons",
    filter  = function(x) !is.na(x$building),
    type    = "polygon",
    fill    = "#f2f2f2",
    border  = "#aaaaaa",
    lwd     = 0.15,
    alpha   = 0.9
  ),
  parks = list(
    label   = "Parks & green spaces",
    default = FALSE,
    source  = "multipolygons",
    filter  = function(x) {
      ((!is.na(x$leisure) & x$leisure %in% c("park","garden","nature_reserve","playground")) |
       (!is.na(x$landuse) & x$landuse %in% c("grass","forest","meadow","recreation_ground","allotments")))
    },
    type    = "polygon",
    fill    = "#c8e6c0",
    border  = "#4CAF50",
    lwd     = 0.2,
    alpha   = 0.7
  ),
  water = list(
    label   = "Water bodies",
    default = FALSE,
    source  = "multipolygons",
    filter  = function(x) {
      ((!is.na(x$natural) & x$natural == "water") | !is.na(x$water) |
       (!is.na(x$landuse) & x$landuse == "reservoir"))
    },
    type    = "polygon",
    fill    = "#b3d9f7",
    border  = "#2196F3",
    lwd     = 0.3,
    alpha   = 0.85
  ),
  amenities = list(
    label   = "Amenities (points)",
    default = FALSE,
    source  = "points",
    filter  = function(x) !is.na(x$amenity),
    type    = "point",
    color   = "#e74c3c",
    alpha   = 0.9
  ),
  schools = list(
    label   = "Schools & education",
    default = FALSE,
    source  = "points",
    filter  = function(x) {
      !is.na(x$amenity) & x$amenity %in% c("school","university","college","kindergarten","library")
    },
    type    = "point",
    color   = "#9b59b6",
    alpha   = 0.9
  ),
  health = list(
    label   = "Healthcare",
    default = FALSE,
    source  = "points",
    filter  = function(x) {
      !is.na(x$amenity) & x$amenity %in% c("hospital","clinic","pharmacy","doctors","dentist","health_centre")
    },
    type    = "point",
    color   = "#e74c3c",
    alpha   = 0.9
  ),
  transit = list(
    label   = "Transit stops",
    default = FALSE,
    source  = "points",
    filter  = function(x) {
      (!is.na(x$highway) & x$highway == "bus_stop") | !is.na(x$public_transport) |
      (!is.na(x$railway) & x$railway %in% c("station","stop","halt"))
    },
    type    = "point",
    color   = "#FF9800",
    alpha   = 0.9
  )
)


# -----------------------------------------------------------
# OSM data fetching (single-fetch architecture)
# -----------------------------------------------------------

osmextract_cache_dir <- function() {
  cache_dir <- file.path(tempdir(), "barrio_osmextract_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_dir
}

# Fetch all requested features from the smallest number of .pbf reads.
# features is a character vector of LAYER_DEFS names.
getOsmFeatures <- function(bb, features) {
  if (length(features) == 0) return(list())

  bbox_sf   <- sf::st_as_sfc(sf::st_bbox(bbox_to_sf_order(bb), crs = 4326))
  cache_dir <- osmextract_cache_dir()

  # Which OSM layers need to be fetched?
  needed_sources <- unique(vapply(features, function(f) {
    def <- LAYER_DEFS[[f]]
    if (is.null(def)) NA_character_ else def$source
  }, character(1)))
  needed_sources <- needed_sources[!is.na(needed_sources)]
  needed_sources <- intersect(needed_sources, c("lines", "multipolygons", "points"))

  # Fetch each source once
  raw <- list()
  for (src in needed_sources) {
    raw[[src]] <- tryCatch(
      osmextract::oe_get(
        place              = bbox_sf,
        layer              = src,
        download_directory = cache_dir,
        boundary           = bbox_sf,
        boundary_type      = "clipsrc",
        quiet              = TRUE
      ),
      error = function(e) NULL
    )
  }

  # Extract each requested feature using its filter function
  result <- list()
  for (f in features) {
    def <- LAYER_DEFS[[f]]
    if (is.null(def)) next
    src  <- def$source
    data <- raw[[src]]
    if (is.null(data) || nrow(data) == 0) next
    filt <- tryCatch(def$filter(data), error = function(e) rep(FALSE, nrow(data)))
    filt[is.na(filt)] <- FALSE
    if (any(filt)) result[[f]] <- data[filt, ]
  }
  result
}

preloadOsmRegions <- function(places) {
  cache_dir <- osmextract_cache_dir()
  for (place in places) {
    tryCatch({
      message("Pre-loading OSM extract for: ", place)
      osmextract::oe_get(place = place, download_directory = cache_dir,
                         download_only = TRUE, quiet = TRUE)
    }, error = function(e) warning("Could not pre-load ", place, ": ", conditionMessage(e)))
  }
}

# Helper to get user-set color with validation and fallback
getColor <- function(input_val, default_val) {
  v <- trimws(input_val %||% "")
  if (grepl("^#[0-9A-Fa-f]{6}$", v)) v else default_val
}


# -----------------------------------------------------------
# Map code system
# -----------------------------------------------------------

generateMapCode <- function() {
  paste0(sample(c(LETTERS, as.character(0:9)), 6, replace = TRUE), collapse = "")
}

codesDir <- function() {
  # Reads BARRIOMAP_DATA_DIR first so the path can be set once in
  # Connect's App Settings > Environment and never needs to change
  # across redeployments. Falls back to the service account's home
  # directory, which also persists unlike getwd() (the bundle dir).
  d <- Sys.getenv("BARRIOMAP_DATA_DIR",
                  unset = file.path(path.expand("~"), ".barriomap_data"))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

saveMapCode <- function(code, params) {
  params$saved_at <- as.numeric(Sys.time())
  tryCatch(
    writeLines(jsonlite::toJSON(params, auto_unbox = TRUE),
               file.path(codesDir(), paste0(code, ".json"))),
    error = function(e) NULL
  )
}

loadMapCode <- function(code, max_days = 30) {
  code <- toupper(trimws(code))
  f    <- file.path(codesDir(), paste0(code, ".json"))
  if (!file.exists(f)) return(NULL)
  params <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(params)) return(NULL)
  if ((as.numeric(Sys.time()) - params$saved_at) / 86400 > max_days) {
    file.remove(f); return(NULL)
  }
  params
}

cleanExpiredCodes <- function(max_days = 30) {
  d <- codesDir()
  if (!dir.exists(d)) return(invisible(NULL))
  for (f in list.files(d, pattern = "\\.json$", full.names = TRUE)) {
    tryCatch({
      p <- jsonlite::fromJSON(f)
      if (!is.null(p$saved_at) && (as.numeric(Sys.time()) - p$saved_at) / 86400 > max_days)
        file.remove(f)
    }, error = function(e) if (f != file.path(d, "gallery.json") && f != file.path(d, "stats.json")) file.remove(f))
  }
  invisible(NULL)
}


# -----------------------------------------------------------
# Community gallery and map counter
# -----------------------------------------------------------

incrementMapCount <- function() {
  f <- file.path(codesDir(), "stats.json")
  s <- tryCatch(if (file.exists(f)) jsonlite::fromJSON(f) else list(total_maps = 0L),
                error = function(e) list(total_maps = 0L))
  s$total_maps <- (s$total_maps %||% 0L) + 1L
  tryCatch(writeLines(jsonlite::toJSON(s, auto_unbox = TRUE), f), error = function(e) NULL)
  s$total_maps
}

getMapCount <- function() {
  f <- file.path(codesDir(), "stats.json")
  tryCatch(
    if (file.exists(f)) jsonlite::fromJSON(f)$total_maps else 0L,
    error = function(e) 0L
  )
}

addToGallery <- function(code, lat, lon, scale, date_str) {
  f <- file.path(codesDir(), "gallery.json")
  gallery <- tryCatch(
    if (file.exists(f)) jsonlite::fromJSON(f, simplifyVector = FALSE) else list(),
    error = function(e) list()
  )
  entry <- list(code = code, lat = round(lat, 4), lon = round(lon, 4),
                scale = as.numeric(scale), date = date_str)
  gallery <- c(list(entry), gallery)
  if (length(gallery) > 12) gallery <- gallery[1:12]
  tryCatch(writeLines(jsonlite::toJSON(gallery, auto_unbox = TRUE), f), error = function(e) NULL)
}

getGallery <- function() {
  f <- file.path(codesDir(), "gallery.json")
  tryCatch(
    if (file.exists(f)) jsonlite::fromJSON(f, simplifyVector = FALSE) else list(),
    error = function(e) list()
  )
}


# -----------------------------------------------------------
# User data helpers
# -----------------------------------------------------------

# Build popup HTML for editable labels (works inside Leaflet via Shiny.setInputValue)
makeEditPopup <- function(source, feature_id, current_label) {
  lbl <- if (is.na(current_label) || is.null(current_label)) "" else as.character(current_label)
  fid <- as.integer(feature_id)
  # Build the JS call with sprintf so the onclick attribute is cleanly closed.
  js <- sprintf(
    "Shiny.setInputValue('save_label',{src:'%s',id:%d,lbl:document.getElementById('elbl_%d').value},{priority:'event'})",
    source, fid, fid
  )
  paste0(
    '<div style="min-width:190px;">',
    '<p style="font-size:11px;color:#888;margin-bottom:4px;">Label this feature:</p>',
    '<input type="text" id="elbl_', fid, '" value="', htmltools::htmlEscape(lbl), '" ',
    'style="width:100%;padding:5px;border:1px solid #ccc;border-radius:4px;font-size:13px;">',
    '<button onclick="', js, '" ',
    'style="margin-top:6px;background:#1a5c3a;color:white;border:none;',
    'border-radius:4px;padding:5px 12px;cursor:pointer;font-size:12px;width:100%;">',
    'Save label</button>',
    '</div>'
  )
}

# Convert drawn GeoJSON geometry to sf
drawnGeomToSf <- function(geom_type, coords) {
  tryCatch({
    if (geom_type == "Point") {
      sf::st_sfc(sf::st_point(c(coords[[1]], coords[[2]])), crs = 4326)
    } else if (geom_type %in% c("Polygon", "Rectangle")) {
      ring <- do.call(rbind, lapply(coords[[1]], function(c) c(c[[1]], c[[2]])))
      if (!identical(ring[1,], ring[nrow(ring),])) ring <- rbind(ring, ring[1,])
      sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

# Safely write sf object to GeoJSON (omits problematic list columns)
write_safe_geojson <- function(sf_obj, path) {
  cols_to_keep <- names(sf_obj)[vapply(names(sf_obj), function(nm) {
    !is.list(sf_obj[[nm]])
  }, logical(1))]
  sf::st_write(sf_obj[, cols_to_keep], path,
               driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
}
