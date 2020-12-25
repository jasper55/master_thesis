## beginning


data_set_name <- "24-1-2020_1"
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
lat_m <- lat_m[12:len] 
lon_m <- lon_m[12:len] 
accuracy <- accuracy[12:len] 
time_elapsed <- time_elapsed[12:len]
len <- length(lat_m) 

delta_t <- 0
for(i in 2:len){
NANO_TO_SECONDS <- 1000000000
delta_t[i] <- (time_elapsed[i]-time_elapsed[i-1])/NANO_TO_SECONDS 
}

total_time <- sum(delta_t)
total_distance_lat <- lat_m[len]-lat_m[1]
total_distance_lon <- lon_m[len]-lon_m[1]

avg_vel_lon <- total_distance_lon/total_time 


## one dimensional (lon in meters) Kalman Filter with constant vel 

k <- 0.1
k2 <- k * 1

# initial state
x0 <- lat_m[1]
P0 <- (median(accuracy,na.rm=TRUE))^2 * k

x <- NA
x_prev <- x0
P <- P0
P_p <- NA
Q <- 0.3
R <- ((median(accuracy,na.rm=TRUE))^2) * k2


for (i in 2:length(lon_m)) {


# 1. predict
P_p <- P  + Q
x_p <- x_prev + delta_t[i] * avg_vel_lon
K <- P_p / (P_p + R)

# 2. step measure
y <- lat_m[i]

# 3. estimate state
x[i] <- x_p + K * (y - x_p)

# 4. step update
P <- (1-K) * P_p
x_prev <- x[i]
print(K)

}



windows()
  plot(lat_m)
  lines(x, lwd=2, col="blue")
  legend("topright", legend=c("measurements","predictions"), lwd=c(1,2), col=c("black","blue"))






speed <- avg_vel_lon
for (i in 1:len){
speed[i+1] <- (x_p[i+1]-x_p[i])/delta_t[i+1]

}




## one dimensional Kalman Filter (static velocity) 



# parameter adjustmens that effect P and R and their relationship
k <- 0.1
k2 <- k * 1

# initial state
vel0 <- speed[1]
P0 <- (median(accuracy/delta_t,na.rm=TRUE))^2 * k

vel_prev <- vel0
vel <- NA
P <- P0
P <- P0
Q <- 0.3
R <- ((median(accuracy/delta_t,na.rm=TRUE))^2) * k2

for (i in 2:length(speed)) {


# 1. predict
P_p <- P + Q
x_p <- x_prev + delta_t[i] * avg_vel_lon
vel_p <- vel_prev 
K <- P_p / (P_p + R)


# 2. step measure
z <- speed[i]


# 3. estimate state
vel[i] <- vel_p + K * (z - vel_p)


# 4. step update
P <- (1-K) * P_p
vel_prev <- vel[i]
print(vel[i])

}



windows()
  plot(speed)
  lines(vel, lwd=2, col="blue")
  legend("topright", legend=c("measurements","predictions"), lwd=c(1,2), col=c("black","blue"))

