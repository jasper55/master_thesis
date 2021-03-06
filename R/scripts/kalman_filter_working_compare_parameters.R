## beginning


directory <- "sailing"
data_set_name <- "12-9-2020_1"
relative_Path_PC <- "D:/Github/MasterThesis/master_thesis/R"
relative_Path_Laptop <- "D:/Backup/01_Masterarbeit/master_thesis/R"



#relative_path <- relative_Path_PC
relative_path <- relative_Path_Laptop

# set project directories


homeDir <- paste(relative_path,sep="")
wrkDir <- paste(relative_path,"/scripts",sep="")
fctDir <- paste(relative_path,"/scripts/functions",sep="")
gpxData <- paste(relative_path,"/data/",directory,"/",data_set_name,".gpx",sep="")
resDir <- paste(relative_path,"/results/",directory,"/Q/",data_set_name,sep="")

if (!dir.exists(resDir)){
  dir.create(resDir)
} else {
  print("Dir already exists!")
}




# loading a set of libraries with load_lib (functions installs library if not installed yet
# 
package_name_string <- c('XML', 'OpenStreetMap',
                         'lubridate', 'ggmap', 'ggplot2', 'raster', 'sp',
                         'geosphere', 'gridExtra')

for (i in package_name_string) {
  require(i,character.only = TRUE)
}



###############################
### read the gpx data   #######
### 				            #######
###############################

options(digits=20)
# Parse the GPX file
library("XML")

pfile <- htmlTreeParse(file = gpxData, error = function(...) {
}, useInternalNodes = T)

################
# Get all elevations, times and coordinates via the respective xpath
#######################

date <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
date <- date[1]
accuracy <- as.numeric(xpathSApply(pfile, path = "//trkpt/acc", xmlValue))
distance <- as.numeric(xpathSApply(pfile, path = "//trkpt/distance", xmlValue))
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
provider <- xpathSApply(pfile, path = "//trkpt/provider", xmlValue)
time_elapsed <- as.numeric(xpathSApply(pfile, path = "//trkpt/time_elapsed", xmlValue))
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
bearing <- xpathSApply(pfile, path = "//trkpt/bear", xmlValue)
speed <- xpathSApply(pfile, path = "//trkpt/speed", xmlValue)


lat <- as.numeric(coords["lat",])
lon <- as.numeric(coords["lon",])
speed <- as.numeric(speed)
bearing <- as.numeric(bearing)

### convert coordinates to meters
setwd(fctDir)
source("latToMeters.R")
source("lonToMeters.R")

lat_m <- latToMeters(lat)
lon_m <- lonToMeters(lat,lon)

len <- length(lat_m)
delta_t <- 1
for(i in 2:(len)){
  delta_t[i] <- (time_elapsed[i]-time_elapsed[i-1])/1000000000 
}


data_set <- data.frame(provider = provider[1:len],
                       accuracy = accuracy[1:len],
                       lat_m = lat_m[1:len],  
                       lon_m = lon_m[1:len],
                       distance = distance[1:len],
                       time_elapsed = time_elapsed[1:len],
                       bearing = bearing[1:len],
                       delta_t = delta_t,
                       speed = speed[1:len]
)

data_set <- data_set[data_set$delta_t != 0, ]


lat_m <- data_set$lat_m
lon_m <- data_set$lon_m
accuracy <- data_set$accuracy
time_elapsed <- data_set$time_elapsed
distance  <- data_set$distance 
bearing <- data_set$bearing  
delta_t<- data_set$delta_t
speed <- data_set$speed

len <- length(lon_m)


speed_lat <- cos(bearing*pi/180) * speed
speed_lon <- sin(bearing*pi/180) * speed

if (speed[1] == 0) {
  
  diff_x <- lon_m[2]-lon_m[1]
  diff_y <- lat_m[2]-lat_m[1]

  
  speed[1] <- sqrt(diff_x*diff_x/(delta_t[2]*delta_t[2]) + diff_y*diff_y/(delta_t[2]*delta_t[2]))
  speed_lat <- cos(bearing*pi/180) * speed[1]
  speed_lon <- sin(bearing*pi/180) * speed[1]
  print(paste("initial speed was 0.0, now: ",speed[1],sep=""))
}


#### with Kalman Gain
#########


bearing_kf <- bearing[1]

## state extrapolation  equation 
# x_n_next_n = A * x_n_n + B * u_n_n + w_n

# x: state vector
x_n_n_prev <- rbind(
  lon_m[1],
  lat_m[1],
  speed_lon[1],
  speed_lat[1])


# A/F state transition matrix
A <- rbind(
  c(1,0,delta_t[1],0),
  c(0,1,0,delta_t[1]),
  c(0,0,1,0),
  c(0,0,0,1)
)

# B/G control matrix/input transition matrix 
## descirbes what is controlling the movement of the object, like gravity or any other force
B <- rbind(
  c(0.5*(delta_t[1])^2,0),
  c(0,0.5*(delta_t[1])^2),
  c(delta_t[1],0),
  c(0,delta_t[1])
)
## u_n vector: control variable matrix
# for constant vel:
a_x <- 0
a_y <- 0
u_n_n <- rbind(a_x,a_y)

## ---- B also becomes zero


### w: predicted state noise matrix
w_n <- rbind(0.1,0.1,0.1,0.1)

x <- rbind(c(NA,NA,NA,NA))
x <- rbind(c(x_n_n_prev))


# set uncertainities

# error in the estimate
rate_lat_lon <- abs(lat_m[len]-lat_m[1])/abs(lon_m[len]-lon_m[1])
#factor_lat <- factor_lat_lon/(factor_lat_lon+1)
#factor_lon <- 1/(factor_lat_lon+1)
factor_lat <- rate_lat_lon
factor_lon <- 1-rate_lat_lon



obs_error <- (accuracy[1]*accuracy[1])/4 
## half of messurement error, 4 because of 2 dimensions

p_factor_error <- obs_error  # but 3 times the error as a general rule to start with
#speed_error_lat <- factor_lat * p_factor_error * mean(delta_t,na.rm=TRUE)
#speed_error_lon <- factor_lon * p_factor_error * mean(delta_t,na.rm=TRUE)





## 2 initial process covariance matrix (process variation - error of the estimation) 
P_n_n_prev <-  rbind(
  c(p_factor_error * factor_lon,0,0,0),
  c(0,p_factor_error * factor_lat,0,0),
  c(0,0,factor_lon * p_factor_error * mean(delta_t,na.rm=TRUE),0),
  c(0,0,0,factor_lat * p_factor_error * mean(delta_t,na.rm=TRUE))
)

# R_n = measurement error ----- accurarcy!!!!
## sensor noise covariance matrix

messurement_error_increased <- FALSE
messurement_error_resetted <- FALSE

r_factor_error <- p_factor_error * 27
R <- rbind(
  c(r_factor_error * factor_lon,0,0,0),
  c(0,r_factor_error * factor_lat,0,0),
  c(0,0,factor_lon * r_factor_error * mean(delta_t,na.rm=TRUE),0),
  c(0,0,0,factor_lat * r_factor_error * mean(delta_t,na.rm=TRUE))
)     

### Q/ w_n another uncertainity, environment (waves, gusts, mistakes of the sailor) --- needs to be set depending on conditions
### process noise/plant noise/driving noise/dynamics noise/model noise/system noise
# keeps P from becoming too small or going to 0
 q <- 1.0
Q <- rbind(
  c(q,0,0,0),
  c(0,q,0,0),
  c(0,0,q,0),
  c(0,0,0,q)
)

## H Helper Matrix
H <- rbind(
  c(1,0,0,0),
  c(0,1,0,0),
  c(0,0,1,0),
  c(0,0,0,1)
) 

##########################
### begin loop


for (i in 2:(len-1)) {  ## da i-1 erster index ist und i = 2 initial state ist, wegen delta_t

### 0. init Matrices
## update A + B + R
A <- rbind(
c(1,0,delta_t[i],0),
c(0,1,0,delta_t[i]),
c(0,0,1,0),
c(0,0,0,1)
)

# B/G control matrix/input transition matrix
B <- rbind(
c(0.5*(delta_t[i])^2,0),
c(0,0.5*(delta_t[i])^2),
c(delta_t[i],0),
c(0,delta_t[i])
)



#R <- rbind(
#c(9 *factor_mess_error * obs_error * factor_lon,0,0,0),
#c(0,9* factor_mess_error * obs_error * factor_lat,0,0),
#c(0,0,3*factor_mess_error * speed_error_lon ,0),
#c(0,0,0,3* factor_mess_error * speed_error_lat )
#) 


## 1.
## predict the state 
x_n_predict_n = A %*% x_n_n_prev + B %*% u_n_n # + w_n

## 3.
# predict process covarinace matrix P
P_n_n <- A %*% P_n_n_prev %*% t(A) + Q
P_n_n <- diag(diag(P_n_n))

### 4.
## calculating Kalman Gain
KG <- (P_n_n %*% H) / ( t(H) %*% P_n_n %*% H + R)
KG <- diag(diag(KG))

### 5.
## New Observation
vel_x <- (lon_m[i]-lon_m[i-1])/delta_t[i]
vel_y <- (lat_m[i]-lat_m[i-1])/delta_t[i]

#e <- accuracy[i]  # just increases the value of the estimate, doesnt bring better results
e <- 0
current_mes_error <- rbind(e,e,e,e)
#y <- rbind(lon_m[i],lat_m[i],speed_lon[i],speed_lat[i]) + current_mes_error 
y <- rbind(lon_m[i],lat_m[i],vel_x,vel_y) + current_mes_error 

### 6. 
## update state

x_n_n <- x_n_predict_n + KG %*% ( y - H %*% x_n_predict_n)

#print("------------------------")
#print(paste("previous: " ,x_n_n_prev[1,1],sep=""))
#print(paste("delta_t: " ,delta_t[i],sep=""))
#print(paste("predcition: " ,x_n_predict_n[1,1],sep=""))
##print(paste("measurment: " ,y[1,1],sep=""))
#print(paste("estimate: " ,x_n_n[1,1],sep=""))
#print(paste("KG: " ,KG[1,1],sep=""))
#print(paste("vel_x: " ,vel_x,sep=""))
#print(paste("vel_lon: " ,speed_lon[i],sep=""))
#print(paste("vel_y: " ,vel_y,sep=""))
#print(paste("vel_lat: " ,speed_lat[i],sep=""))

# Covariance Update Equation
P_n_n <- (1-KG) * P_n_n_prev




# save data to vector
x <- rbind(x,c(x_n_n))
if (i > 2){ 

  
  #### just debugging
  ########################
  
  
  diff_x <- x[i,1]-x[i-1,1]
  diff_y <- x[i,2]-x[i-1,2]
  vel_lon <- rbind((diff_x)/delta_t[i])  
  vel_lat <- rbind((diff_y)/delta_t[i]) 
  vel_x <- diff_x/delta_t[i]
  vel_y <- diff_y/delta_t[i]
  
  vel <- sqrt(vel_x*vel_x + vel_y*vel_y)
  setwd(fctDir)
  source("bearing_from_x_y.R")
  bear <- bearing_from_x_y(diff_x, diff_y)
  bearing_kf <- rbind(bearing_kf,bear)
  
  #print(paste("velocity: ", vel, sep=""))
 # print(paste("velocity_X: ", vel_x, sep=""))
 # print(paste("velocity_y: ", vel_y, sep=""))
 # print(paste("bearing: ", bearing[i], sep=""))
  ##############
  

} # end if (i > 2)

# update state
x_n_n_prev <- x_n_n
P_n_n_prev <- P_n_n + Q




}


##########################
### end loop




# print results
setwd(resDir)


graphics.off()
jpeg(filename=paste("longitude_data_series_",Q[1],"_",Q[3,3],".jpeg",sep=""),
     width = 2048, height = 2048, units = "px", pointsize = 24,
     quality = 75)
  plot(lon_m[1:len])
  lines(x[,1], lwd=2, col="blue")
  legend("topleft", legend=c("measurements","estimates", 
                             paste("R: ",r_factor_error, sep="" ),
                             paste("P: ",p_factor_error, sep="" ),
                             paste("Q: ",Q[1], sep="" ),
                             paste("Q speed: ",Q[3,3], sep="" )
                             ), lwd=c(1,2), col=c("black","blue"))
  dev.off()


  graphics.off()
  jpeg(filename=paste("latitude_data_series_",Q[1],"_",Q[3,3],".jpeg",sep=""),
       width = 2048, height = 2048, units = "px", pointsize = 24,
       quality = 75)
  plot(lat_m[1:len])
  lines(x[,2], lwd=2, col="blue")
  legend("topleft", legend=c("measurements","estimates", 
                             paste("R: ",r_factor_error, sep="" ),
                             paste("P: ",p_factor_error, sep="" ),
                             paste("Q: ",Q[1], sep="" ),
                             paste("Q speed: ",Q[3,3], sep="" )
                             ), lwd=c(1,2), col=c("black","blue"))

  dev.off()
  


  graphics.off()
  jpeg(filename=paste("trajectory_",Q[1],"_",Q[3,3],".jpeg",sep=""),
       width = 2048, height = 2048, units = "px", pointsize = 24,
       quality = 75)
  plot(lon_m,lat_m)
  par(new=TRUE)
  plot(x[,1],x[,2], type="l", lwd=2, col="blue")
  legend("topleft", legend=c("measurements","estimates",
                             paste("R: ",r_factor_error, sep="" ),
                             paste("P: ",p_factor_error, sep="" ),
                             paste("Q: ",Q[1], sep="" ),
                             paste("Q speed: ",Q[3,3], sep="" )
                             ), 
         lwd=c(1,2), col=c("black","blue"))
  dev.off()



  
  speed_kf <- sqrt((x[,3]^2)+(x[,4]^2))

  graphics.off()
  jpeg(filename=paste("speed_comparisation_",Q[1],"_",Q[3,3],".jpeg",sep=""),
       width = 2048, height = 2048, units = "px", pointsize = 24,
       quality = 75)
  
  plot(speed, lwd=2, col="black")
  lines(speed_kf, lwd=2, col="blue")
  legend("topleft", legend=c("measurements",
                             "estimates", 
                             paste("R: ",r_factor_error, sep="" ),
                             paste("P: ",p_factor_error, sep="" ),
                             paste("Q: ",Q[1], sep="" ),
                             paste("Q speed: ",Q[3,3], sep="" )
  ), 
  lwd=c(1,2), col=c("black","blue"))

  dev.off()

  
  graphics.off()
  jpeg(filename=paste("bearing_comparisation_",Q[1],"_",Q[3,3],".jpeg",sep=""),
       width = 2048, height = 2048, units = "px", pointsize = 24,
       quality = 75)
  plot(bearing, lwd=2, col="black")
  lines(bearing_kf, lwd=2, col="blue")
  legend("topleft", legend=c("measurements","estimates", 
                             paste("R: ",r_factor_error, sep="" ),
                             paste("P: ",p_factor_error, sep="" ),
                             paste("Q: ",Q[1], sep="" ),
                             paste("Q_speed: ",Q[3,3], sep="" )
  ), lwd=c(1,2), col=c("black","blue"))
  dev.off()

  
 

