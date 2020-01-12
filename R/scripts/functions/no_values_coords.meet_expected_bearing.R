no_values_coords.meet_expected_bearing <- function(lat, lon, bearing_expected)
{

# start indices
l <- length(lat)
bearing_number_of_values_needed_from_coords <- NA

print(expected_bearing)
for (i in 2:l){
  
  print(paste("i= ", i))
  for(k in (i-1):1){

    bearing_calc <- calculate_bearing_from_coords(lat[k],lat[i],lon[k],lon[i])
    
    print(paste("k= ", k))
    print(paste("diff:",bearing_calc-expected_bearing,sep=""))
    print("")
    
    if(abs(bearing_calc-expected_bearing) <= 10){
      bearing_number_of_values_needed_from_coords[i] <- i-k
      break
    }
    
    
  }
}
return(bearing_number_of_values_needed_from_coords)
}