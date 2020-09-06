## beginning


data_set_name <- "sensor_data/sensor_readings_13-8-2020_1"
relative_Path_PC <- "D:/Github/MasterThesis/master_thesis/R"
relative_Path_Laptop <- "D:/Backup/01_Masterarbeit/master_thesis/R"

relative_path <- relative_Path_PC
#relative_path <- relative_Path_Laptop

# set project directories

homeDir <- paste(relative_path, sep = "")
wrkDir <- paste(relative_path, "/scripts", sep = "")
fctDir <- paste(relative_path, "/scripts/functions", sep = "")
gpxData <- paste(relative_path, "/data/", data_set_name, ".gpx", sep = "")
resDir <- paste(relative_path, "/results/", data_set_name, sep = "")

if (!dir.exists(resDir)) {
  dir.create(resDir)
} else {
  print("Dir already exists!")
}



#### initial set up:
need_to_Download_Packages <- true

# loading a set of libraries with load_lib (functions installs library if not installed yet
#
package_name_string <- c(
  'XML',
  'OpenStreetMap',
  'lubridate',
  'ggmap',
  'ggplot2',
  'raster',
  'sp',
  'geosphere',
  'gridExtra'
)
#
for (i in package_name_string) {
  #install.packages(i)
  library(i, character.only = TRUE)
}



###############################
### read the gpx data   #######
### 				            #######
###############################

options(digits = 20)
# Parse the GPX file
library("XML")

pfile <- htmlTreeParse(
  file = gpxData,
  error = function(...) {
    
  },
  useInternalNodes = T
)

################
# Get all elevations, times and coordinates via the respective xpath
#######################

sensor_name <- xpathSApply(pfile, path = "//trkpt/sensor_name", xmlValue)
type <- xpathSApply(pfile, path = "//trkpt/type", xmlValue)
time <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
timestamp <-
  as.numeric(xpathSApply(pfile, path = "//trkpt/timestamp", xmlValue))
x_value <-
  as.numeric(xpathSApply(pfile, path = "//trkpt/x_value", xmlValue))
z_value <-
  as.numeric(xpathSApply(pfile, path = "//trkpt/z_value", xmlValue))
y_value <-
  as.numeric(xpathSApply(pfile, path = "//trkpt/y_value", xmlValue))
accuracy <-
  as.numeric(xpathSApply(pfile, path = "//trkpt/accuracy", xmlValue))
resolution <-
  as.numeric(xpathSApply(pfile, path = "//trkpt/resolution", xmlValue))

len <- length(type)

data_set <- data.frame(
  sensor_name = sensor_name[1:len],
  type = type[1:len],
  time = time[1:len],
  x_value = x_value[1:len],
  y_value = y_value[1:len],
  z_value = z_value[1:len],
  accuracy = accuracy[1:len],
  resolution = resolution
)

data_set_acc <- data_set[data_set$sensor_name == "accelerometer-lsm6ds3-c",]

x_acc = data_set_acc$x_value
y_acc = data_set_acc$y_value
z_acc = data_set_acc$z_value
