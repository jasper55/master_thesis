lonToMeters <- function(latitude, longitude)
{
require('pracma')
library('pracma')

# Convert from Degrees to Radians
latitude <- deg2rad(latitude)

meters <- 40075000 * cos(latitude) / 360 * longitude

return(meters)
}