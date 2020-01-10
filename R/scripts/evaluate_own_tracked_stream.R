## beginning


data_set_name <- "04-12-2019_0"
# set project directories
homeDir <- "D:/Backup/01_Masterarbeit/master_thesis/R"
wrkDir <- "D:/Backup/01_Masterarbeit/master_thesis/R/scripts"
gpxData <- paste("D:/Backup/01_Masterarbeit/master_thesis/R/data/",data_set_name,".gpx",sep="")
resDir <- paste("D:/Backup/01_Masterarbeit/master_thesis/R/results/",data_set_name,sep="")

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
# 
for (i in package_name_string) {
#install.packages(i)
library(i,character.only = TRUE)
}



###############################
### read the gpx data   #######
### 				            #######
###############################

options(digits=10)
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

str(coords)
lat_prev <- as.numeric(coords["lat",])
lon_prev <- as.numeric(coords["lon",])




data_set <- data.frame(provider = provider,
                       accuracy = accuracy,
                       lat_prev = lat_prev,  
                       lon_prev = lon_prev,
                       distance = distance,
                       time_elapsed = time_elapsed,
                       bearing = bearing
  )
len <- nrow(data_set)
data_set <- data_set[data_set$distance != 0, ]

data_set <- unique(data_set)
# delete first 35 values as accurcy is very inaccurate
N <- 35
data_set <- data_set[-(1:N), , drop = FALSE]

len <- nrow(data_set)

provider <- data_set$provider
accuracy <- data_set$accuracy
lat_prev <- data_set$lat_prev
lon_prev <- data_set$lon_prev
distance <- data_set$distance
time_elapsed <- data_set$time_elapsed
bearing <- data_set$bearing

## set first values to NA
bearing[1] <- NA
distance[1] <- NA

setwd(wrkDir)
source("functions/shift.vec.R")
lat_next <- shift.vec(lat_prev, -1)
lon_next <- shift.vec(lon_prev, -1)
data_set$lat_next <- lat_next
data_set$lon_next <- lon_next
  
time_diff <- sapply(1:len-1, function(x) (shift.vec(time_elapsed, -1)-time_elapsed))[,1]
time_diff <- shift.vec(time_diff,1)

# calculate distance in meters between two meassurements
library("geosphere")
distance_R <- apply(data_set,1, FUN = function (row) {
  distm(
    c(as.numeric(row["lon_prev"]),as.numeric(row["lat_prev"])),
    c(as.numeric(row["lon_next"]),as.numeric(row["lat_next"])),
    fun = distVincentyEllipsoid)
})
distance_R <- shift.vec(distance_R,1)

source("functions/calculate_bearing_from_coords.R")   # needs package   pracma
library("pracma")
bearing_calc_A_R <- calculate_bearing_from_coords(lat_prev, lat_next, lon_prev, lon_next)
bearing_calc_A_R <- shift.vec(bearing_calc_A_R,1)

speed <- sapply(1:len-1, function (x) 
 (as.numeric(distance)/as.numeric(time_diff))*1000000000
)[,1]


speed_R <- sapply(1:len-1, function (x) 
  (as.numeric(distance_R)/as.numeric(time_diff))*1000000000
)[,1]


data_set <- data.frame(provider = provider,
                       accuracy = accuracy,
                       lat_prev = lat_prev,
                       lon_prev = lon_prev,
                       distance = distance,
                       distance_R = distance_R,
                       time_elapsed = time_elapsed,
                       time_diff = time_diff,
                       speed = speed,
                       speed_R = speed_R,
                       bearing = bearing,
                       bearing_calc_A_R = bearing_calc_A_R)


#################
## linear regression
#####

source("functions/calculate_bearing_from_LR.R")
bearing_from_LR_start_end <- calculate_bearing_from_LR(lon_prev,lat_prev)
                                             

########
# expected, calculated and mean,median values
#####
# expected bearing
len <- nrow(data_set)
bearing_expected <- calculate_bearing_from_coords(lat_prev[1],lat_prev[len],lon_prev[1],lon_prev[len])

# expected speed
total_distance <-   
  distm(
    c(as.numeric(lon_prev[1]),as.numeric(lat_prev[1])),
    c(as.numeric(lon_prev[len]),as.numeric(lat_prev[len])),
    fun = distVincentyEllipsoid)
total_time <- sum(time_diff,na.rm = T)/1000000000
speed_expected <- total_distance/total_time



mean_current_speed <- round(mean(speed,na.rm=T),2)
median_current_speed = round(median(speed,na.rm=T),2)
median_current_speed_R = round(median(speed_R,na.rm=T),2)

mean_bearing <- round(mean(as.numeric(bearing),na.rm=T),2)
median_bearing <- round(median(as.numeric(bearing),na.rm=T),2)
median_bearing_coords <- round(median(bearing_calc_A_R,na.rm=T),2)




#######################################
########## part 2  ++++++++++++++
#######################################

# estimate numer of values to meet expected values for speed within 0.1 accuracy

#######
## create summarize table and save it

results <- data.frame(filename = data_set_name,
                      n_values = nrow(data_set),
                      acc = round(mean(data_set$accuracy,na.rm=T),2),
                      expected_speed = round(speed_expected,2),
                      mean_current_speed <- mean_current_speed,
                      median_current_speed = median_current_speed,
                      median_current_speed_R = median_current_speed_R,
                      expected_bearing <- round(bearing_expected,2),
                      mean_bearing <- mean_bearing,
                      median_bearing <- median_bearing,
                      median_bearing_coords <- median_bearing_coords,
                      bearing_from_LR_start_end <- round(bearing_from_LR_start_end,2))

colnames(results) <- c("filename", "n_values","acc", "expected_speed",
                       "mean_current_speed", "median_current_speed",
                       "median_current_speed_R", "expected_bearing","mean_bearing", "median_bearing",
                       "median_bearing_coords", "bearing_from_LR_start_end")
rownames(results) <- NULL

##############################
#   save results
#####
source("functions/plot_and_save_results.R")

#############################
#### recursive calculation of number of previous values needed to meet expected values for speed
###################
setwd(wrkDir)
source("functions/recursive_evaluation_no_of_needed_values.R")

