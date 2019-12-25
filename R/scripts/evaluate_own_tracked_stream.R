## beginning

# set project directories
wrkDir <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/scripts"
gpxData <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/data/04-12-2019_0.gpx"

data_set_name <- "04-12-2019_0"

resDir <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/results"


setwd(wrkDir)
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

# Get all elevations, times and coordinates via the respective xpath

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

setwd(wrkDir)
source("shift.vec.R")
lat_next <- shift.vec(lat_prev, -1)
lon_next <- shift.vec(lon_prev, -1)
data_set$lat_next <- lat_next
data_set$lon_next <- lon_next


time_diff <- sapply(1:len-1, function(x) (shift.vec(time_elapsed, -1)-time_elapsed))[,1]
#time_diff <- shift.vec(time_diff,1)

# calculate distance in meters between two meassurements
library("geosphere")
distance_R <- apply(data_set,1, FUN = function (row) {
  distm(
    c(as.numeric(row["lon_prev"]),as.numeric(row["lat_prev"])),
    c(as.numeric(row["lon_next"]),as.numeric(row["lat_next"])),
    fun = distHaversine)
})


#distance_R <- shift.vec(distance_R,1)

source("calculate_bearing_from_coords.R")   # need package   pracma
library("pracma")
bearing_calc_A_R <- calculate_bearing_from_coords(lat_prev, lat_next, lon_prev, lon_next)
#bearing_calc_A_R <- shift.vec(bearing_calc_A_R,1)

speed <- sapply(1:len-1, function (x) 
 (as.numeric(distance)/as.numeric(time_diff))*1000000000
)[,1]

speed_R <- sapply(1:len-1, function (x) 
  (as.numeric(distance_R)/as.numeric(time_diff))*1000000000
)[,1]


data_set <- data.frame(provider = provider,
                       accuracy = accuracy,
                       lat_prev = lat_prev, lat_next = lat_next, 
                       lon_prev = lon_prev, lon_next = lon_next,
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

source("calculate_bearing_from_LR.R")
bearing_from_LR <- calculate_bearing_from_LR(lon_prev,lat_prev)
                                             


###### SVM
##
# SVMModel = fitcsvm(lon_prev,lat_prev)

########
# expected, calculated and mean,median values
#####
# expected bearing
len <- nrow(data_set)
expected_bearing <- calculate_bearing_from_coords(lat_prev[1],lat_prev[len],lon_prev[1],lon_prev[len])

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
median_speed_distance_R = round(median(speed_R,na.rm=T),2)

mean_bearing <- round(mean(as.numeric(bearing),na.rm=T),2)
median_bearing <- round(median(as.numeric(bearing),na.rm=T),2)
median_bearing_coords <- round(median(bearing_calc_A_R,na.rm=T),2)




#######################################
########## part 2  ++++++++++++++
#######################################

# estimate numer of values to meet expected values for speed within 0.1 accuracy

setwd(wrkDir)
library("geosphere")
source("no_values_coords.meet_expected_speed.R")
no_values_coords_for_speed <- no_values_coords.meet_expected_speed(lat_prev,lon_prev, time_diff/1000000000, speed_expected)




#######################################################
######### save the data_set

library("gridExtra")
setwd(resDir)
png(paste("data_set_",data_set_name,".png",sep=""), height = 50*nrow(data_set), width = 200*ncol(data_set))
grid.table(data_set)
dev.off()


#######
## create summarize table and save it

results <- data.frame(filename = data_set_name,
                      n_values = nrow(data_set),
                      acc = round(mean(data_set$accuracy,na.rm=T),2),
                      expected_speed = round(speed_expected,2),
                      mean_current_speed <- mean_current_speed,
                      median_current_speed = median_current_speed,
                      median_speed_distance_R = median_speed_distance_R,
                      expected_bearing <- round(expected_bearing,2),
                      mean_bearing <- mean_bearing,
                      median_bearing <- median_bearing,
                      median_bearing_coords <- median_bearing_coords,
                      bearing_from_LR <- round(bearing_from_LR,2))

colnames(results) <- c("filename", "n_values","acc", "expected_speed",
                       "mean_current_speed", "median_current_speed",
                       "median_speed_distance_R", "expected_bearing","mean_bearing", "median_bearing",
                       "median_bearing_coords", "bearing_from_LR")
rownames(results) <- NULL

setwd(resDir)
write.table(results, "results.csv", sep="\t", row.names=FALSE, col.names = !file.exists("results.csv"), append = T)
#library("gridExtra")

setwd(resDir)
graphics.off()
png(filename = paste("Linear_Regression ",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)
plot(lon_prev,lat_prev, asp=1,
     xlab="longitude",
     ylab="latitude")
lm <- lm(lat_prev ~ lon_prev)
slope <- lm$coefficients[2]
intercept <- lm$coefficients[1]

abline(intercept,slope)
grid(6,6)
legend('topleft', c(
  paste("slope: ",round(slope,2),sep=""), 
  paste("tan-1(",round(slope,2),"): ",
        round(rad2deg(atan(slope)),2),"°",sep=""
  )
))
dev.off()


plot(distance,type="l",col="red")
lines(accuracy,col="green")
legend('topleft', c("distance","accuracy"))
lines(time_diff/100000000, col="blue")

plot(lat_prev,col="red")
par(new=TRUE)
plot(lon_prev, col="blue")
par(new=TRUE)
plot(accuracy,type="l", col="green")

plot(no_values_coords_for_speed)

plot(speed_R, col="blue")
par(new=TRUE)
plot(speed, col="green")
abline(speed_expected,0)

########################################
setwd(resDir)
graphics.off()
png(filename = paste("data_intervall_1_20_coords_timeDiff_distance__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)

plot(lon_prev[1:20],lat_prev[1:20],asp=1,col="blue",pch = 19)
par(new=TRUE)
plot(time_diff[2:21]/100000000,type="l",col="black", ylim=c(0,13),lty = 1)
par(new=TRUE)
plot(distance_R[2:21],type="l",col="green",lty = 2)
legend('topleft', 
       c("coordinates","time difference","distance"),
       col=c("blue", "black", "green"),
       pch = c(19, NA, NA),
       lty=c(NA,1,2), 
       cex=0.8
 )
dev.off()


graphics.off()
png(filename = paste("data_intervall_1_20_coords_relative_distance__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)

plot(lon_prev[1:20],lat_prev[1:20],asp=1,col="blue",pch = 19)
par(new=TRUE)
plot(distance_R[2:21]/(time_diff[2:21]/1000000000),type="l",col="green",lty = 2)
legend('topleft', 
       c("coordinates","relative distance"),
       col=c("blue","green"),
       pch = c(19, NA),
       lty=c(NA,2), 
       cex=0.8
)
dev.off()



#########################################
