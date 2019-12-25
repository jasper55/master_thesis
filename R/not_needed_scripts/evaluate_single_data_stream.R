## beginning

# set project directories
wrkDir <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/scripts"
gpxData_old <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/data/20191109.gpx"
gpxData <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/data/20191118.gpx"

resDir <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/results"


setwd(wrkDir)
# loading a set of libraries with load_lib (functions installs library if not installed yet
# 
 package_name_string <- c('XML', 'OpenStreetMap',
 'lubridate', 'ggmap', 'ggplot2', 'raster', 'sp',
 'geosphere', 'gridExtra')
# 
 #for (i in package_name_string) {
 #install.packages(i)
 #library(i,character.only = TRUE)
 #}


# Defining a function that shifts vectors conveniently:

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


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

elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
course <- xpathSApply(pfile, path = "//trkpt/course", xmlValue)

lat_prev <- as.numeric(coords["lat",])
lon_prev <- as.numeric(coords["lon",])

library("lubridate")
options(digits.secs=3)

# calculate time difference between two meassurements
time_formatted <- as.POSIXct(times,format = "%Y-%m-%dT%H:%M:%OS")
len <- length(time_formatted)
time_diff <- sapply(1:len, function(x) difftime(time_formatted[x+1],time_formatted[x]))

data_set <- data.frame(lat_prev = lat_prev, lon_prev = lon_prev, time_diff = time_diff, bearing = course)

data_set$lat_next <- shift.vec(data_set$lat_prev, -1)
data_set$lon_next <- shift.vec(data_set$lon_prev, -1)

# calculate distance in meters between two meassurements
library("raster")
data_set$dist_to_prev <- apply(data_set, 1, FUN = function (row) {
  pointDistance(
    c(as.numeric(row["lat_next"]),as.numeric(row["lon_next"])),
    c(as.numeric(row["lat_prev"]), as.numeric(row["lon_prev"])),
    lonlat = T)
})

head(data_set)


# list to have an overview how many datsets were created:
dataset_list <- list()
rm(list=c("dataset_1","dataset_2","dataset_7","dataset_8","dataset_3","dataset_4","dataset_5", "dataset_6"))
# minimum_required_data_amount:
min_n <- 8
## indicate the indices where to sperate the data 
sep_indices <- cbind(c(1,which(data_set$time_diff[1:len] > 10)))
sep_indices <- cbind(c(sep_indices,len))

# start looping
k <- 1  # starting data_set number
for (i in 1:length(sep_indices)-1){
  
  # n:   number of data for one measurement data set
  n <- sep_indices[i+1]-sep_indices[i]
  
  # only use data is number of data set is larger/equal than 8
  
  if(n >= min_n && length(n) != 0){
    
    name <- paste("dataset", k, sep = "_")
    assign(name, data_set[
      sep_indices[i]:sep_indices[i+1],
      ])
    dataset_list[[k]] <- name
    k <- k+1
    print(paste(name, "number of of rows:",n, sep = " "))
  }
}

dataset_count <- length(dataset_list)



#TODO seperate and clean data
# exclude values where bearing = 0.0
# seperate dataet if bearing or jumps too much between two values

# manually
dataset_1_2 <- dataset_1[5:37,]
dataset_1_2 <- dataset_1[39:46,]
dataset_1_3 <- dataset_1[49:104,]

dataset_1_2 <- dataset_1_2[3:8,]
dataset_2 <- dataset_2[4:47,]


##########################################################################
##########################################################################
##########################################################################
dataset <- dataset_2 #####################################################
##########################################################################
##########################################################################



# distance and time difference between first and last measurement of a data series
n_values <- nrow(dataset)
distance_start_end <-   
  pointDistance(
    c(as.numeric(dataset$lat_prev[1]),as.numeric(dataset$lon_prev[1])),
    c(as.numeric(dataset$lat_prev[n_values]),as.numeric(dataset$lon_prev[n_values])),
    lonlat = T)

time_different_start_end <- sum(dataset$time_diff, na.rm=TRUE)

expected_speed <- distance_start_end/time_different_start_end

library("geosphere")
bearing_start_end <- 
  bearing(
    c(
      as.numeric(dataset$lon_prev[1]),
      as.numeric(dataset$lat_prev[1])
      ),
    c(
      as.numeric(dataset$lon_prev[n_values]),
      as.numeric(dataset$lat_prev[n_values])
      )
  )

median_bearing <- median(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["bearing"])
}
))
mean_bearing <- mean(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["bearing"])
}
))



# calculate current speed
dataset$current_speed <- apply(dataset, 1, FUN = function (row) {
 as.numeric(row["dist_to_prev"])/as.numeric(row["time_diff"])
})

median_speed <-
median(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["current_speed"])
}
))



###### next data set ##############
# dataset_2
# distance and time difference between first and last measurement of a data series

n_values <- nrow(dataset)
distance_start_end <-   
  pointDistance(
    c(as.numeric(dataset$lat_prev[1]),as.numeric(dataset$lon_prev[1])),
    c(as.numeric(dataset$lat_prev[n_values]),as.numeric(dataset$lon_prev[n_values])),
    lonlat = T)
time_different_start_end <- sum(dataset$time_diff, na.rm=TRUE)

expected_average_speed <- distance_start_end/time_different_start_end

library("geosphere")
bearing_start_end <- 
  bearing(
    c(
      as.numeric(dataset$lon_prev[n_values]),
      as.numeric(dataset$lat_prev[n_values])
    ),
    c(
      as.numeric(dataset$lon_prev[1]),
      as.numeric(dataset$lat_prev[1])
    )
  )

# calculate bearing from lat/lon
dataset$bearing_calculated <- apply(dataset, 1, FUN = function (row) {
  bearing(
    c(
      as.numeric(row["lon_next"]),
      as.numeric(row["lat_next"])
    ),
    c(
      as.numeric(row["lon_prev"]),
      as.numeric(row["lat_prev"])
    )
  )
})

median_bearing <- median(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["bearing"])
}
))

mean_bearing <- mean(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["bearing"])
}
))


# calculate current speed
dataset$current_speed <- apply(dataset, 1, FUN = function (row) {
  as.numeric(row["dist_to_prev"])/as.numeric(row["time_diff"])
})

median_speed <- median(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["current_speed"])
}
))
mean_speed <- mean(apply(dataset, 1, FUN = function (row) {
  as.numeric(row["current_speed"])
}
))

# results:
head(dataset)

bearing_start_end
median_bearing
mean_bearing

median_speed
mean_speed
expected_average_speed

###########################################

###     algorithm to calculate number
###     of values needed to estimate
###     good enough values for
###     current speed and bearing

####################
###     algorithm 1
###     
###   estimating how many values are needed to estimate 
###   so that the median value is close enough to the expected value

# dataset_2 <- subset(dataset_2, select=-
#                       c(bearing_number_of_values_need,
#                         bearing_number_of_values_needed_from_calc_bear,
#                         bearing_number_of_values_needed_from_coords))

# start indices
l <- nrow(dataset)
dataset$number_of_values_needed_from_calc_bear <- NA


for (i in 2:l){
  
  for(k in (i-1):1){

  median_bearing <- median(as.numeric(dataset$bearing_calculated[k:i]))
  
  if(abs(median_bearing-expected_bearing) <= 1){
    dataset$number_of_values_needed_from_calc_bear[i] <- i-k
    break
  }
  }
}

dataset$number_of_values_needed_from_calc_speed <- NA
for (i in 2:l){
  
  for(k in (i-1):1){
  
    median_speed <- median(as.numeric(dataset$current_speed[k:i]))
    
    if(abs(median_speed-expected_speed) <= 0.1){
      dataset$number_of_values_needed_from_calc_speed[i] <- i-k
      break
    }
  }
}


####################
###     algorithm 2
###     
###   estimating how many values/distance/time are needed to estimate 
###   from two coordinates the bearing and speed 
###   that they are close enough to the expected value


expected_bearing <- bearing_start_end
expected_speed <- expected_average_speed

# start indices
l <- nrow(dataset)
dataset$number_of_values_needed_from_coords <- NA

print(expected_speed)
for (i in 2:l){
  
  print(paste("i= ", i))
  for(k in (i-1):1){

    # determine bearing
    # in k, i loop
    bearing <- 
      bearing(
        c(
          as.numeric(dataset$lon_prev[i]),
          as.numeric(dataset$lat_prev[i])
        ),
        c(
          as.numeric(dataset$lon_prev[k]),
          as.numeric(dataset$lat_prev[k])
        )
      )
    #print(paste(abs(bearing-expected_bearing),"   k:",k,"i:",i,sep=" "))
    #print(expected_bearing)
    if(abs(bearing-expected_bearing) <= 1){
      dataset$number_of_values_needed_from_coords[i] <- i-k
      break
    }
  }
}


# start indices
l <- nrow(dataset)
dataset$speed_number_of_values_needed_from_coords <- NA

print(expected_speed)
for (i in 2:l){
  
  print(paste("i= ", i))
  for(k in (i-1):1){
    
    # determine speed 
    # in k, i loop
    # for(k in (i-1):1)
    
    distance_calc <-   
      pointDistance(
        c(
          as.numeric(dataset$lat_prev[k]),
          as.numeric(dataset$lon_prev[k])
        ),
        c(
          as.numeric(dataset$lat_prev[i]),
          as.numeric(dataset$lon_prev[i])
        ),
        lonlat = T)
    
    time_elapsed <- sum(dataset$time_diff[k:i-1])
    print(paste("k= ", k))
    speed <- distance_calc/time_elapsed
    print(paste("distance:",distance_calc,sep=""))
    print(paste("time elapsed:",time_elapsed,sep=""))
    print(paste("diff:",speed-expected_speed,sep=""))
    print("")
    
    if(abs(speed-expected_speed) <= 0.1){
      dataset$speed_number_of_values_needed_from_coords[i] <- i-k
      break
    }
    
    
  }
}






###########################################
###     print results       ###############

#install.packages("gridExtra")

setwd(resDir)
png(paste(name,".png",sep=""),height = 50*nrow(dataset), width = 200*ncol(dataset))
picture <-tableGrob(dataset)
grid.arrange(picture)
dev.off()


