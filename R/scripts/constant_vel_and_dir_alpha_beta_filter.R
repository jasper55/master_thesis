## beginning


data_set_name <- "not_straight_line/26-7-2020_1"
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



# get the minim length of all columns
M <- min(lengths(list(c(provider),
        c(accuracy),
        c(lat),  
        c(lon),
        c(distance),
        c(time_elapsed),
        c(bearing)), use.names = TRUE))



data_set <- data.frame(provider = provider[1:M],
                       accuracy = accuracy[1:M],
                       lat = lat[1:M],  
                       lon = lon[1:M],
                       distance = distance[1:M],
                       time_elapsed = time_elapsed[1:M],
                       bearing = bearing[1:M]
  )


# delete rows where distance between two points is 0
#data_set <- data_set[data_set$distance != 0, ]
#data_set <- data_set[data_set$delta_t != 0.0, ]


#data_set <- unique(data_set)

M <- length(data_set$lat) 

delta_t <- 0
for(i in 2:M){
delta_t[i] <- (time_elapsed[i]-time_elapsed[i-1])/1000000000 
}

data_set <- data.frame(provider = provider[1:M],
                       accuracy = accuracy[1:M],
                       lat = lat[1:M],  
                       lon = lon[1:M],
                       distance = distance[1:M],
                       time_elapsed = time_elapsed[1:M],
                       bearing = bearing[1:M],
				delta_t = delta_t[1:M]
  )




### convert coordinates to meters
setwd(fctDir)
source("coordinate_to_meters.R")
lat_m <- coordinate_to_meters(data_set$lat,0)
lon_m <- coordinate_to_meters(0,data_set$lon)

#len <- length(lat_m)
#lat_m <- lat_m[12:len] 
#lon_m <- lon_m[12:len] 
#accuracy <- accuracy[12:len] 
#time_elapsed <- time_elapsed[12:len]

delta_t <- data_set$delta_t
len <- length(lat_m)

total_time <- sum(delta_t)
total_distance_lat <- lat_m[len]-lat_m[1]
total_distance_lon <- lon_m[len]-lon_m[1]

avg_vel_lon <- total_distance_lon/total_time 
avg_vel_lat <- total_distance_lat/total_time 


#### new version

x_n_n_prev <- lat_m[1]
vel_n_n_prev <- avg_vel_lat
x <- c(lat_m[1])
vel <- c(avg_vel_lat)
alpha <- 0.2
beta <- 0.1


for (i in 2:(len-1)) {
print(i)

x_n_n <- x_n_n_prev + alpha * (lat_m[i] - x_n_n_prev)

vel_n_n <- vel_n_n_prev + beta *((lat_m[i] - x_n_n_prev)/delta_t[i])
print(vel_n_n)

x_n_next_n <- x_n_n + delta_t[i] * vel_n_n
vel_n_next_n <- vel_n_n

x_n_n_prev <- x_n_next_n
vel_n_n_prev <- vel_n_next_n

# save data to vector
x[i] <- x_n_n_prev
vel[i] <- vel_n_n_prev
}

windows()
  plot(lat_m)
  lines(x, lwd=2, col="blue")
  legend("topright", legend=c("measurements","predictions","mean"), lwd=c(1,2,2), col=c("black","blue","green"))


 