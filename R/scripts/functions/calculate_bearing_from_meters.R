calculate_bearing_from_meters <- function(lon_m_prev, lon_m, lat_m_prev, lat_m)
{


  sign_lat <- lat_m - lat_m_prev
  sign_lon <- lon_m - lon_m_prev

diff_x <- lon_m - lon_m_prev
diff_y <- lat_m - lat_m_prev

bearing <- rad2deg(atan(diff_y/diff_x))

#if(sign_lat <= 0 && sign_lon >= 0){
 #   bearing <- 270 - bearing_from_slope
#  }
 # if(sign_lat <= 0 && sign_lon <= 0){
#    bearing <- 270 - bearing_from_slope
#  }
  
#  if(sign_lat >= 0 && sign_lon <= 0){
#    bearing <- 90 - bearing_from_slope
#  }
 # if(sign_lat >= 0 && sign_lon >= 0){
 #   bearing <- 90 - bearing_from_slope
 # }
  
  return(bearing)
}
