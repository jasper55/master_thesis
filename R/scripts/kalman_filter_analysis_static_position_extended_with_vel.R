## beginning


data_set_name <- "one_location_data/24-7-2020_4"
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

#########
# calculate variance:
variance_lat_m <- var(lat_m)*(len-1)/len



## one dimensional Kalman Filter (static position) on lat

# initial state
x0 <- lat_m[1]
P0 <- (mean(accuracy))^2

# prediction

x[1] <- x0
P <- P0

for (i in 2:length(lat_m)) {

# 1. step measure
z <- lat_m[i]
R <- (mean(accuracy[i]))^2

# 2. step update

K <- P / ( P+R)
x[i] <- x[i-1] + K * (z-x[i-1])
P <- (1-K) * P

# 3. predict
# for static phenomena prediction doesn't change
# x_p <- x
# P_p <- P

}






#### alpha____beta - Filter

x_n_n_prev <- lat_m[1]
vel_n_n_prev <- avg_vel_lat
#vel_n_n_prev <- 0
x <- c(lat_m[1])
vel <- c(avg_vel_lat)
alpha <- 0.2
beta <- 0.1
P_n_n_prev <- 
P_v_n_n <- 

for (i in 2:(len-1)) {
print(i)

#x_n_n <- x_n_n_prev + alpha * (lat_m[i] - x_n_n_prev)
x_n_n <- x_n_n_prev + KG * (lat_m[i] - x_n_n_prev)

#4. Covariance Update Equation
P_n_n <- (1-KG) * P_n_n_prev
P_n_n_prev <- P_n_n

### for not static case:
## p_x_n_next_n = p_x_n_n + delta_t[i]^2 * P_v_n_n
## P_v_n_next_n = P_v_n_n

#vel_n_n <- vel_n_n_prev + beta *((lat_m[i] - x_n_n_prev)/delta_t[i])
#stays same



x_n_next_n <- x_n_n + delta_t[i] * vel_n_n
#vel_n_next_n <- vel_n_n


x_n_n_prev <- x_n_next_n
vel_n_n_prev <- vel_n_next_n

# save data to vector
x[i] <- x_n_n_prev
vel[i] <- vel_n_n_prev
}

windows()
  plot(lat_m)
  lines(x, lwd=2, col="blue")
  abline(h=mean(lat_m), col="green")
  legend("topleft", legend=c("measurements","predictions","mean"), lwd=c(1,2,2), col=c("black","blue","green"))




