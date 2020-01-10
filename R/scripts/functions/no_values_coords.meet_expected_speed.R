no_values_coords.meet_expected_speed <- function(lat, lon, time_diff, expected_speed)
{
time_diff <- shift.vec(time_diff,-1)
# start indices
l <- length(lat)
speed_number_of_values_needed_from_coords <- NA

print(expected_speed)
for (i in 2:l){
  
  print(paste("i= ", i))
  for(k in (i-1):1){

    distance_calc <-   
      distm(
        c(
          as.numeric(lon[k]),
          as.numeric(lat[k])
        ),
        c(
          as.numeric(lon[i]),
          as.numeric(lat[i])
        ),
        fun = distVincentyEllipsoid)
    
    time_elapsed <- sum(time_diff[k:(i-1)])
    print(paste("k= ", k))
    speed <- distance_calc/time_elapsed
    print(paste("distance:",distance_calc,sep=""))
    print(paste("time elapsed:",time_elapsed,sep=""))
    print(paste("diff:",speed-expected_speed,sep=""))
    print("")
    
    if(abs(speed-expected_speed) <= 0.1){
      speed_number_of_values_needed_from_coords[i] <- i-k
      break
    }
    
    
  }
}
return(speed_number_of_values_needed_from_coords)
}