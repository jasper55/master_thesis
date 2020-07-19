## beginning


data_set_name <- "26-5-2020_1"
# set project directories
homeDir <- "D:/Backup/01_Masterarbeit/master_thesis/R"
wrkDir <- "D:/Backup/01_Masterarbeit/master_thesis/R/scripts"
gpxData <- paste("D:/Backup/01_Masterarbeit/master_thesis/R/data/one_location_data/",data_set_name,".gpx",sep="")
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




#Kalman Filter
library(KFAS)
logmodel <- SSModel(lat_prev ~ SSMtrend(1, Q = 0.01), H = 0.01)
out <- KFS(logmodel)
ts.plot(ts(out$a[2:46,],lat_prev))

