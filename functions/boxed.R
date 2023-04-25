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


recMap <- leaflet(width = 2480, height = 3508) %>%
  addTiles() %>%
  setView(lng = -110.9742, lat = 32.2540, zoom = 10) 
recMap

nRecLon = 4
nRecVert = 3

adjustlong <- function(map = recMap,  nRecLon){
  
  bx1 <- getBox(map)
  center <- recMap$x$setView[[1]][2]
  
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
    for(i in 1:round(nRecLon/2)){
      if(i ==1){
        boundaries_west[[i]] <- bx1[1]
      }else{
        boundaries_west[[i]] <- boundaries_west[[i-1]] - TotDist
      }
    }
    
    boundaries_east <- list()
    for(i in 1:round(nRecLon/2)){
      if(i ==1){
        boundaries_east[[i]] <- bx1[2]
      }else{
        boundaries_east[[i]] <- boundaries_east[[i-1]] + TotDist
      }
    }
    limits <- sort(c(unlist(boundaries_west), unlist(boundaries_east)))
  }
  
  limits

}
adjustlat <- function(map = recMap,  nRecVert){
  
  bx1 <- getBox(map)
  center <- recMap$x$setView[[1]][1]
  
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
    for(i in 1:round(nRecVert/2)){
      if(i ==1){
        boundaries_south[[i]] <- bx1[3]
      }else{
        boundaries_south[[i]] <- boundaries_south[[i-1]] - TotDist
      }
    }
    
    boundaries_north <- list()
    for(i in 1:round(nRecVert/2)){
      if(i ==1){
        boundaries_north[[i]] <- bx1[4]
      }else{
        boundaries_north[[i]] <- boundaries_north[[i-1]] + TotDist
      }
    }
    limits <- sort(c(unlist(boundaries_south), unlist(boundaries_north)))
  }
  
  limits
  
}


lng <- adjustlong(map = recMap,  nRecLon = nRecLon)
lat <- adjustlat(map = recMap,  nRecVert = nRecVert)

rectangles <- do.call(rbind,lapply(1:(nRecLon), function(i){
 do.call(rbind, lapply(1:nRecVert, function(j) {
  c(lng[c(i, i+1)], lat[c(j, j+1)])
  }))
}))

rectangles


