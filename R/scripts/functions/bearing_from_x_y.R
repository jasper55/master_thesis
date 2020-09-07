bearing_from_x_y <- function(diff_x, diff_y)
{

bearing_c <- rad2deg(atan(diff_y/diff_x))

if(diff_y <= 0 && diff_x >= 0){
    bearing <- 90 - bearing_c
  }
  if(diff_y <= 0 && diff_x <= 0){
    bearing <- 270 - bearing_c
  }
  
  if(diff_y >= 0 && diff_x <= 0){
    bearing <- 270 - bearing_c
  }
  if(diff_y >= 0 && diff_x >= 0){
    bearing <- 90 - bearing_c
  }
  
  return(bearing)
}
