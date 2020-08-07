## beginning


data_set_name <- "one_location_data/1-8-2020_3"
relative_Path_PC <- "D:/Github/MasterThesis/master_thesis/R"
relative_Path_Laptop <- "D:/Backup/01_Masterarbeit/master_thesis/R"

relative_path <- relative_Path_PC
#relative_path <- relative_Path_Laptop

# set project directories

homeDir <- paste(relative_path,sep="")
wrkDir <- paste(relative_path,"/scripts",sep="")
fctDir <- paste(relative_path,"/scripts/functions",sep="")
gpxData <- paste(relative_path,"/data/",data_set_name,".gpx",sep="")
resDir <- paste(relative_path,"/results/",data_set_name,sep="")

if (!dir.exists(resDir)){
  dir.create(resDir)
} else {
  print("Dir already exists!")
}



#### initial set up:
need_to_Download_Packages <- true 

# loading a set of libraries with load_lib (functions installs library if not installed yet
# 
package_name_string <- c('XML', 'OpenStreetMap',
                         'lubridate', 'ggmap', 'ggplot2', 'raster', 'sp',
                         'geosphere', 'gridExtra')
# 
for (i in package_name_string) {
#install.packages(i)
library(i,character.only = TRUE)
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

lat <- as.numeric(coords["lat",])
lon <- as.numeric(coords["lon",])

### convert coordinates to meters
setwd(fctDir)
source("coordinate_to_meters.R")
lat_m <- coordinate_to_meters(lat,0)
lon_m <- coordinate_to_meters(0,lon)

len <- length(lat_m)

delta_t <- 0
for(i in 2:len){
delta_t[i] <- (time_elapsed[i]-time_elapsed[i-1])/1000000000 
}
avg_speed_lat <- (lat_m[len]-lat_m[2])/sum(delta_t)
avg_speed_lon <- (lon_m[len]-lon_m[2])/sum(delta_t)



#### with Kalman Gain
#########
# calculate variance:
variance_lat_m <- var(lat_m)*(len-1)/len


## state extrapolation  equation 
# x_n_next_n = A * x_n_n + B * u_n_n + w_n

# x: state vector
x_n_n_prev <- rbind(
lon_m[2],
lat_m[2],
avg_speed_lon,
avg_speed_lat)

# A/F state transition matrix
A <- rbind(
c(1,0,delta_t[2],0),
c(0,1,0,delta_t[2]),
c(0,0,1,0),
c(0,0,0,1)
)

# B/G control matrix/input transition matrix 
## descirbes what is controlling the movement of the object, like gravity or any other force
B <- rbind(
c(0.5*(delta_t[2])^2,0),
c(0,0.5*(delta_t[2])^2),
c(delta_t[2],0),
c(0,delta_t[2])
)
## u_n vector: control variable matrix
# for constant vel:
a_x <- 0
a_y <- 0
u_n_n <- rbind(a_x,a_y)

## ---- B also becomes zero


### w: predicted state noise matrix
w_n <- rbind(0.3,0.3,0.3,0.3)

#vel_n_n_prev <- avg_vel_lat
#vel_n_n_prev <- 0
x <- c(x_n_n_prev)
#vel <- c(avg_vel_lat)


# set uncertainities

# error in the estimate
factor_lat_lon <- sum(lat_m)/sum(lon_m)
factor_lat <- factor_lat_lon/(factor_lat_lon+1)
factor_lon <- 1/(factor_lat_lon+1)

obs_error <- (accuracy[2]*accuracy[2])/4 
## half of messurement error,becuase of 2 dimensions

process_error <- obs_error * 3 * 3 # but 3 times the error as a general rule to start with
speed_error_lat <- process_error * factor_lat * mean(delta_t,na.rm=TRUE)
speed_error_lon <- process_error * factor_lon * mean(delta_t,na.rm=TRUE)

## 2 initial process covariance matrix (process variation - error of the estimation) 
P_n_n_prev <-  rbind(
c(process_error * factor_lon,0,0,0),
c(0,process_error * factor_lat,0,0),
c(0,0,speed_error_lon,0),
c(0,0,0,speed_error_lat)
)

# R_n = measurement error ----- accurarcy!!!!
## sensor noise covariance matrix
m_uncertainity <- accuracy[i]*accuracy[i]/4
# 
R <- rbind(
c(obs_error * factor_lon,0,0,0),
c(0,obs_error * factor_lat,0,0),
c(0,0,speed_error_lon/9,0),
c(0,0,0,speed_error_lat/9)
)      

### Q/ w_n another uncertainity, environment (waves, gusts, mistakes of the sailor) --- needs to be set depending on conditions
### process noise/plant noise/driving noise/dynamics noise/model noise/system noise
# keeps P from becoming too small or going to 0
# Q <- 0.3
Q <- rbind(
c(0.3,0,0,0),
c(0,0.3,0,0),
c(0,0,0.15,0),
c(0,0,0,0.15)
)

## H Helper Matrix
H <- rbind(
c(1,0,0,0),
c(0,1,0,0),
c(0,0,1,0),
c(0,0,0,1)
) 

H2 <- rbind(
c(0,0,0,1),
c(0,0,1,0),
c(0,1,0,0),
c(1,0,0,0)
) 


#P_v_n_n <- variance_lat_m_vel



##########################
### begin loop

for (i in 3:(len-2)) {


### 0.
## update A + B
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


## 1.
## predict the state 
x_n_next_n = A %*% x_n_n_prev + B %*% u_n_n + w_n


## 3.
# predict process covarinace matrix P
P_n_n <- A %*% P_n_n_prev %*% t(A) + Q
diag(diag(P_n_n))

# other option
#R_n <- accuracy[i]*accuracy[i]/4 # error in the measurement

### 4.
## calculating Kalman Gain
KG <- (P_n_n %*% H) / ( t(H) %*% P_n_n %*% H + R)
diag(diag(KG))

### 5.
## New Observation
vel_x <- (lon_m[i]-lon_m[i-1])/delta_t[i]
vel_y <- (lat_m[i]-lat_m[i-1])/delta_t[i]

current_mes_error <- rbind(0,0,0,0)
y <- rbind(lon_m[i],lat_m[i],vel_x,vel_y) + current_mes_error 

### 6. 
## update state

x_n_n <- x_n_next_n + KG %*% ( y - H %*% x_n_next_n)

x_n_n_prev <- x_n_n

KG <- P_n_n_prev / ( P_n_n_prev + R_n)
print(paste("KG: ",KG,sep=""))
z <- c(lat_m[i],lon_m[i])
# get measurement & predict estimate
x_n_n <- x_n_n_prev + KG * ( - x_n_n_prev)


# Covariance Update Equation
P_n_n <- (1-KG) * P_n_n_prev

# update estimate
x_n_n_prev <- x_n_n
P_n_n_prev <- P_n_n + Q

# save data to vector
x[i] <- c(x_n_n_prev)

}



##########################
### end loop

## not used yet
## x_n_next_n <- x_n_n + delta_t[i] * vel_n_n
## vel_n_n_prev <- vel_n_next_n
# vel_n_n <- vel_n_n_prev + beta *((lat_m[i] - x_n_n_prev)/delta_t[i])

### for not static case:
## p_x_n_next_n = p_x_n_n + delta_t[i]^2 * P_v_n_n
## P_v_n_next_n = P_v_n_n

#vel_n_next_n <- vel_n_n

#vel[i] <- vel_n_n_prev


windows()
  plot(lat_m)
  lines(x, lwd=2, col="blue")
  abline(h=mean(lat_m), col="green")
  legend("topleft", legend=c("measurements","predictions","mean"), lwd=c(1,2,2), col=c("black","blue","green"))


