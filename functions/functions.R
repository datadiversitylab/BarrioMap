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



#Calculate the number of screen pixels that correspond to a given distance in meters
meter2screenpixel <- function(meter, orient ="v",  zoomlevel, latitude) {
  #Get the resolution of the map from the "this.map" object.
  res <- 156543.03 * cos(latitude) / (2 ^ zoomlevel)
  #https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
  #metresPerPixel.v = 40075016.686 * abs(cos(latitude * pi/180)) / 2^(zoomlevel+8)
  metresPerPixel.h = 40075016.686 * abs(cos(latitude * pi/180)) / 2^(zoomlevel+8)
  #metresPerPixel.h = 40075016.686 / 2^(zoomlevel+8)
  metresPerPixel.v = 40075016.686 / 2^(zoomlevel+8)
  
  ##this.map.getGeodesicPixelSize().w
  ##this.map.getGeodesicPixelSize().h
  pixSizeGeodesic <- ifelse(orient == "v", metresPerPixel.v, metresPerPixel.h) * 1
  pixel <- meter * (res / pixSizeGeodesic)
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
  needed_res <- scale_meters_per_inch / dpi
  
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