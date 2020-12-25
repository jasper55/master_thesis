## beginning


data_set_name <- "one_location_data/28-7-2020_2"
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



#### with Kalman Gain
#########


x_n_n_prev <- lat_m[1]
x <- c(lat_m[1])



# set uncertainities
P_n_n_prev <- (accuracy[1]*accuracy[1])*2  # error in the estimate

#R_n = measurement error ----- accurarcy!!!!
### Q another uncertainity, environment (waves, gusts, mistakes of the sailor) --- needs to be set depending on conditions
### process noise/plant noise/driving noise/dynamics noise/model noise/system noise

#P_v_n_n <- variance_lat_m_vel

for (i in 2:(len-2)) {

# other option
R_n <- accuracy[i]*accuracy[i]*30 # error in the measurement, higher error -> smaller kalman gain -> faster adjusting to real value
KG <- P_n_n_prev / ( P_n_n_prev + R_n)
#print(paste("KG: ",KG,sep=""))

# get measurement & predict estimate
x_n_n <- x_n_n_prev + KG * (lat_m[i] - x_n_n_prev)


#4. Covariance Update Equation
P_n_n <- (1-KG) * P_n_n_prev

print(paste("P: ",P_n_n,sep=""))


# update estimate
x_n_n_prev <- x_n_n
P_n_n_prev <- P_n_n

# save data to vector
x[i] <- x_n_n_prev

}


windows()
  plot(lat_m)
  lines(x, lwd=2, col="blue")
  abline(h=mean(lat_m), col="green")
  legend("topleft", legend=c("measurements","predictions","mean"), lwd=c(1,2,2), col=c("black","blue","green"))


