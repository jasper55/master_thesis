calculate_bearing <- function(lat1, lat2, lon1, lon2)
{
  lat1 = deg2rad(lat1)
  lat2 = deg2rad(lat2)
  lon1 = deg2rad(lon1)
  lon2 = deg2rad(lon2)
  
  deltaLon = lon2 - lon1
  
  deltaPhi = log(
    tan(lat2 / 2 + pi / 4)
    /
      tan(lat1 / 2 + pi / 4)
  )
  if (abs(deltaPhi) > pi) {
    if (deltaLon > 0.0) {
      deltaLon = -(2.0 * pi - deltaLon)
    } else {
      deltaLon = deltaLon + 2 * pi
    }
  }
  return (rad2deg(
    atan2(deltaLon, deltaPhi)) + 360.0) %% 360
}