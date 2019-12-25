calculate_bearing_from_LR <- function(lon, lat)
{

  lon <- lon * cos(deg2rad(lat))
  linearMod <- lm(lat ~ lon)
  intercept <- linearMod$coefficients[1]
  slope <- linearMod$coefficients[2]
  
  last_value <- length(lat)
  sign_lat <- lat[last_value]-lat[1]
  sign_lon <- lon[last_value]-lon[1]

  
  bearing_from_slope <- rad2deg(atan(slope))
  
  if(sign_lat <= 0 && sign_lon >= 0){
    bearing <- 270 - bearing_from_slope
  }
  if(sign_lat <= 0 && sign_lon <= 0){
    bearing <- 270 - bearing_from_slope
  }
  
  if(sign_lat >= 0 && sign_lon <= 0){
    bearing <- 90 - bearing_from_slope
  }
  if(sign_lat >= 0 && sign_lon >= 0){
    bearing <- 90 - bearing_from_slope
  }
  
  return(bearing)
}