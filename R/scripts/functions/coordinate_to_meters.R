coordinate_to_meters <- function(lat,lon)
{

require('pracma')
library('pracma')

EARTH_RADIUS = 6371.0 * 1000.0

delta_lat = deg2rad(lat)
delta_lon = deg2rad(lon)
a = (sin(delta_lat/ 2))^2 + cos(deg2rad(0)) * cos(deg2rad(lat)) * (sin(delta_lon))^2

c = 2 * atan2(sqrt(a), sqrt(1-a))
result = EARTH_RADIUS * c
return(result)
}