## beginning

# set project directories
wrkDir <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/scripts"
gpxData <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/data/20191109.gpx"
resDir <- "C:/Users/Jasper/Desktop/R projects/gps_algorithm/results"


setwd(wrkDir)
# loading a set of libraries with load_lib (functions intalls library if not installed yet
# 
# package_name_string <- c('XML', 'OpenStreetMap',
# 'lubridate', 'ggmap', 'ggplot2', 'raster', 'sp')
# 
# for (i in package_name_string) {
#  print(i)
# install.packages(package_name_string)
# library(package_name_string)
# }


# Defining a function that shifts vectors conveniently:

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# Before going, let’s check what this function is for:

col1 <- seq(0,100,5)

col2 <- seq(200, 100, -5)

my_df <- data.frame(c1= col1, c2= col2)

my_df

my_df$nc1 <- shift.vec(my_df$c1, -1)
my_df$nc2 <- shift.vec(my_df$c2, -1)
my_df



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

len <- length(elevations) 
# delete first 6 entries

elevations <- elevations[7:len]
times <- times[7:len]
coords <- coords[7:len]

# TODO
# calculate time difference between two meassurements

library("lubridate")
options(digits.secs=3)
timeFormatted <- as.POSIXct(times,format = "%Y-%m-%dT%H:%M:%OS")

len <- length(timeFormatted)
timeDiff <- sapply(1:len, function(x) difftime(timeFormatted[x+1],timeFormatted[x]))

# create vector which shows time diffeence in millis between current value to previous
# sep1 = where time diff > 0
# coords1 <- coords[1:sep1]
# coords2 <- coords[sep1+1:len]
# now hard code sep1
sep <- which(timeDiff > 10 || timeDiff < 0 )
sep_indices <- which(timeDiff[1:len] > 10)

for (i in 1:length(sep_indices)-1){
  # n:   number_of_data_for_one_measurement_data_set
  n <- sep_indices[i+1]-sep_indices[i]
  
  # only use data is number of data set is larger/equal than 8
  if(n >= 3){
  dataset_i <- times[sep_indices[i]:sep_indices[i+1]]
  }
  
}


str(coords)


# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])

# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
head(geodf)

# Shift vectors for lat and lon so that each row also contains the next position.
geodf$lat.p1 <- shift.vec(geodf$lat, -1)
geodf$lon.p1 <- shift.vec(geodf$lon, -1)
head(geodf)


# Calculate distances (in metres) using the function pointDistance from the ‘raster’ package.
# Parameter ‘lonlat’ has to be TRUE!

library("raster")
geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})


td <- sum(geodf$dist.to.prev, na.rm=TRUE)
print(paste("The distance run was ", td, " meters"))
head(geodf$dist.to.prev)


# Transform the column ‘time’ so that R knows how to interpret it.
geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
# Shift the time vector, too.
geodf$time.p1 <- shift.vec(geodf$time, -1)
# Calculate the number of seconds between two positions.
geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))

head(geodf$time.diff.to.prev, n=15) 


# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y


# Plot elevations and smoother
plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
lines(geodf$lowess.ele, col = "red", lwd = 3)
legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red"), lwd = c(1,3), bty = "n")


# Plot speeds and smoother
plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
     col = "grey40")
lines(geodf$lowess.speed, col = "blue", lwd = 3)
legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")

plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")



library(ggmap)
lat <- c(min(geodf$lat), max(geodf$lat))
lat

lon <- c(min(geodf$lon), max(geodf$lon))
lon

bbox <- make_bbox(lon,lat)
bbox 

b1 <- get_map(bbox,maptype="watercolor", source="stamen")

library(ggmap)
ggmap(b1) + geom_point(data = geodf, 
           aes(lon,lat,col = ele), size=1, alpha=0.7) +
           labs(x = "Longitude", y = "Latitude",
           title="Track of test run")