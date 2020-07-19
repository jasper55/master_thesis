no_values_coords_LR.meet_expected_bearing <- function(lat, lon, bearing_expected)
{

# start indices
l <- length(lat)
l10 <- round(l/10,0)
sep <- NA
for (x in 1:l10){
  sep[x] <- x*10
}

bearing_number_of_values_needed_from_coords <- NA

bearing_calc_10 <- NA
ii <- 1
print(expected_bearing)
for (i in 2:l){
  
  
  # if(i %in% sep)
  #   {
  #   bearing_calc_10[ii] <- calculate_bearing_from_coords(lat[i-9],lat[i],lon[i-9],lon[i])
  #   print(paste("bearing: ",bearing_calc_10[ii]))
  #   print(paste("diff:",bearing_calc_10[ii]-expected_bearing,sep=""))
  #   print("")
  #   print("")
  #   ii <- ii+1
  #   }
  
  #print(paste("i= ", i))
  for(k in (i-1):1){

    bearing_calc <- as.numeric(calculate_bearing_from_LR(lon[k:i],lat[k:i]))
    print(bearing_calc)
    print(abs(bearing_calc-expected_bearing))
    
    if(is.na(bearing_calc)) {
      
    }
    else{
    if(abs(bearing_calc-expected_bearing) <= 1){
    bearing_number_of_values_needed_from_coords[i] <- i-k
    #print(paste("k= ", k))
    #print(paste("bearing: ",bearing_calc))
    #print(paste("diff:",bearing_calc-expected_bearing,sep=""))
    #print("")
    break
    }
    }
    
  }
}
return(bearing_number_of_values_needed_from_coords)
}