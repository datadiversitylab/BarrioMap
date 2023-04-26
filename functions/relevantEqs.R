## Get the scale (m/px)
lat = 0
zoom = 0
metresPerPixel = 40075016.686 * abs(cos(lat * pi/180)) / 2^(zoom+8)

## Get the zoom level for a given number of meters per pixel
sp = 156543
lat = 0
zoomLevel = log2(( 40075016.686 * abs(cos(lat * pi/180)))/sp) - 8

## Get the resolution for a zoom level (for the scale; m/px)
zoom = 0
lat = 0
resolution = 156543.03 * cos(lat) / (2 ^ zoom)

## Get the map scale
dpi = 96
resolution = resolution
lat = 0
scale = (dpi * 1/0.0254* resolution) * cos(lat)


#So if you have a screen with 96 dpi, you get that one pixel is 1.1943 meters. 
#And you get a scale of 1 : 4 231 which means that 1 cm on your screen is 
#42.3 m in reality.

