## beginning


data_set_name <- "one_location_data/24-7-2020_1"
relative_Path_PC <- "D:/Github/MasterThesis/master_thesis/R"
relative_Path_Laptop <- "D:/Backup/01_Masterarbeit/master_thesis/R"

#relative_path <- relative_Path_PC
relative_path <- relative_Path_Laptop

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
need_to_Download_Packages <- FALSE 

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
source("latToMeters.R")
source("lonToMeters.R")

lat_m <- latToMeters(lat)
lon_m <- lonToMeters(lat,lon)

len <- length(lat_m)

delta_t <- 0
for(i in 2:len){
delta_t[i] <- (time_elapsed[i]-time_elapsed[i-1])/1000000000 
}
avg_speed_lat <- (lat_m[len]-lat_m[2])/sum(delta_t)
avg_speed_lon <- (lon_m[len]-lon_m[2])/sum(delta_t)



#### with Kalman Gain
#########



## state extrapolation  equation 
# x_n_next_n = A * x_n_n + B * u_n_n + w_n

# x: state vector
x_n_n_prev <- rbind(
lon_m[2],
lat_m[2],
avg_speed_lon,
avg_speed_lat)


## u_n vector: control variable matrix
# for constant vel:
a_x <- 0
a_y <- 0
u_n_n <- rbind(a_x,a_y)


### w: predicted state noise matrix
w_n <- rbind(0.3,0.3,0.3,0.3)


x <- c(x_n_n_prev)


# set uncertainities
# error in the estimate
factor_lat_lon <- sum(lat_m)/sum(lon_m)
factor_lat <- factor_lat_lon/(factor_lat_lon+1)
factor_lon <- 1/(factor_lat_lon+1)

obs_error <- (accuracy[2]*accuracy[2]) 
k <- 10
k2 <- k * 5 

process_error <- obs_error * k  # but 3 times the error as a general rule to start with
speed_error_lat <- obs_error * factor_lat * median(delta_t,na.rm=TRUE)
speed_error_lon <- obs_error * factor_lon * median(delta_t,na.rm=TRUE)

## 2 initial process covariance matrix (process variation - error of the estimation) 
P_n_n_prev <-  rbind(
c(process_error * factor_lon,0,0,0),
c(0,process_error * factor_lat,0,0),
c(0,0,speed_error_lon * k,0),
c(0,0,0,speed_error_lat * k)
)

# R_n = measurement error ----- accurarcy!!!!
## sensor noise covariance matrix
# 
R <- rbind(
c(obs_error * factor_lon,0,0,0),
c(0,obs_error * factor_lat,0,0),
c(0,0,speed_error_lon * k2,0),
c(0,0,0,speed_error_lat * k2)
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
P_n_n <- diag(diag(P_n_n))


### 4.
## calculating Kalman Gain
KG <- (P_n_n %*% H) / ( t(H) %*% P_n_n %*% H + R)
KG <- diag(diag(KG))
print(paste("KG: ",KG,sep=""))
print(KG)

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


# Covariance Update Equation
P_n_n <- (1-KG) * P_n_n_prev

# update estimate
x_n_n_prev <- x_n_n
P_n_n_prev <- P_n_n + Q

# save estimation to vector
x <- rbind(x,c(x_n_n))
}



##########################
### end loop


windows()
  plot(lat_m)
  lines(x[,2], lwd=2, col="blue")
  abline(h=mean(lat_m), col="green")
  legend("topleft", legend=c("measurements","predictions","mean"), lwd=c(1,2,2), col=c("black","blue","green"))


