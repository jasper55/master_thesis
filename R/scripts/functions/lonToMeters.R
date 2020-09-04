lonToMeters <- function(latitude, longitude)
{
require('pracma')
library('pracma')

# Convert from Degrees to Radians
latitude <- deg2rad(latitude)
longitude <- deg2rad(longitude)


earthRadius = 6367000 # Radius in m
posX = earthRadius * cos(latitude) * cos(longitude)
posY = earthRadius * cos(latitude) * sin(longitude)
return(posX)
}