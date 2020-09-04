	# calculate the change of bearing 
	bear_kalman <- calculate_bearing_from_meters(x[i-1,1],x[i,1],x[i-1,2],x[i,2]) 
	bearing_kalman <- rbind(bearing_kalman,bear_kalman)	

	diff_bearing <- abs(bearing_kalman[i]-bearing_kalman[i-1]) 
	if(diff_bearing >= 5 && diff_bearing <= 30) {

		if (messurement_error_increased == FALSE) {
			factor_mess_error <- diff_bearing 
			messurement_error_increased <- TRUE
			messurement_error_resetted <- FALSE
		} # end if: messurement_error_increased
	
	} else {

		if(messurement_error_resetted == FALSE) {
			factor_mess_error <- 1
			messurement_error_resetted <- TRUE
			messurement_error_increased <- FALSE
		} # end if messurement_error_resetted

	} # end if diff_bearing
print(paste('diff_bearing: ',diff_bearing,sep=""))
print(factor_mess_error)