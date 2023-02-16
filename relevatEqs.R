## Get the scale (https://wiki.openstreetmap.org/wiki/Zoom_levels)
lat = 0
zoom = 0
metresPerPixel = 40075016.686 * abs(cos(lat * pi/180)) / 2^(zoom+8)

## Get the zoom level for a given number of meters
sp = 156543
lat = 0
zoomLevel = log2(( 40075016.686 * abs(cos(lat * pi/180)))/sp) - 8
