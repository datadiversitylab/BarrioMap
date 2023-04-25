library(leaflet)

recMap <- leaflet(width = 2480, height = 3508) %>%
  addTiles() %>%
  setView(lng = -110.9742, lat = 32.2540, zoom = 10) 
recMap


returnRectangles(map = recMap, nRecLon =2, nRecVert=1)
returnRectangles(map = recMap, nRecLon =2, nRecVert=2)
returnRectangles(map = recMap, nRecLon =1, nRecVert=2)
returnRectangles(map = recMap, nRecLon =1, nRecVert=1)
returnRectangles(map = recMap, nRecLon =8, nRecVert=4)
returnRectangles(map = recMap, nRecLon =1, nRecVert=5)
returnRectangles(map = recMap, nRecLon =5, nRecVert=1)




