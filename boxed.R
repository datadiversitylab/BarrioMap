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


adjustlong <- function(map = recMap,  nRec){
  
  bx1 <- getBox(map)
  center <- recMap$x$setView[[1]][2]
  
  #Estimate distance
  TotDist <- abs(bx1[1] - bx1[2])
  

  if((num %% 2) == 0) {
    #If nRec is even
    
    
  }else{ 
    #If nRec is odd
  boundaries_west <- list()
  for(i in 1:round(nRec/2)){
    if(i ==1){
    boundaries_west[[i]] <- bx1[1]
    }else{
    boundaries_west[[i]] <- boundaries_west[[i-1]] - TotDist
    }
  }
  
  boundaries_east <- list()
  for(i in 1:round(nRec/2)){
    if(i ==1){
      boundaries_east[[i]] <- bx1[2]
    }else{
      boundaries_east[[i]] <- boundaries_east[[i-1]] - TotDist
    }
  }
  
  }
  
}





