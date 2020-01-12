library("geosphere")


setwd(wrkDir)
##### bearing section
############################
source("functions/no_values_coords.meet_expected_bearing.R")
no_values_coords_for_bearing <- no_values_coords.meet_expected_bearing(lat_prev,lon_prev, bearing_expected)
mean <- round(mean(no_values_coords_for_bearing,na.rm=T),0)
median <- median(no_values_coords_for_bearing,na.rm=T)

source("functions/no_values_coords_LR.meet_expected_bearing.R")
no_values_coords_LR_for_bearing <- no_values_coords_LR.meet_expected_bearing(lat_prev,lon_prev, bearing_expected)
mean_LR <- round(mean(no_values_coords_LR_for_bearing,na.rm=T),0)
median_LR <- median(no_values_coords_LR_for_bearing,na.rm=T)

setwd(resDir)

#####
# without LR
graphics.off()
png(filename = paste("no.of_values_bearing__",data_set_name,".png",sep=""), width = 16, height = 16, units = "in", res = 75)

plot(no_values_coords_for_bearing, 
     main="amount of values (coordinates) needed to meet expected bearing",
     xlab = "",
     ylab= ""
     )

### add coordinates
par(new=TRUE)
plot(lon_prev,pch=21, col="red",
     xlab = "",
     ylab= "")
par(new=TRUE)
plot(lat_prev,pch=21, col="green",
     xlab = "",
     ylab= "")


## add accuracy
par(new=TRUE)
plot(accuracy,type="l", col="blue",
     xlab = "",
     ylab= "")

legend('topleft', 
       c("no_values","lat","lon","accuracy",
         paste("mean = ",mean,sep=""),
         paste("median = ",median,sep="")),
       col=c("black","red","green","blue"),
       pch = c(19,19,19,NA),
       lty = c(NA,NA,NA,1),
       cex=0.8)

# add distance and time difference
#par(new=TRUE)
#plot(distance_R,type="l", col="blue")
#par(new=TRUE)
#plot(time_diff,type="l", col="green")

dev.off()


#####
# LinearRegression
graphics.off()
png(filename = paste("no.of_values_bearing_LR__",data_set_name,".png",sep=""), width = 16, height = 16, units = "in", res = 75)

plot(no_values_coords_LR_for_bearing, 
     main="amount of values (coordinates) after LR needed to meet expected bearing",
     xlab = "",
     ylab= ""
)

### add coordinates
par(new=TRUE)
plot(lon_prev,pch=21, col="red",
     xlab = "",
     ylab= "")
par(new=TRUE)
plot(lat_prev,pch=21, col="green",
     xlab = "",
     ylab= "")


## add accuracy
par(new=TRUE)
plot(accuracy,type="l", col="blue",
     xlab = "",
     ylab= "")

legend('topleft', 
       c("no_values","lat","lon","accuracy",
         paste("mean = ",mean_LR,sep=""),
         paste("median = ",median_LR,sep="")),
       col=c("black","red","green","blue"),
       pch = c(19,19,19,NA,NA,NA),
       lty = c(NA,NA,NA,1,NA,NA),
       cex=0.8)

# add distance and time difference
#par(new=TRUE)
#plot(distance_R,type="l", col="blue")
#par(new=TRUE)
#plot(time_diff,type="l", col="green")

dev.off()



##### speed section
############################
source("functions/no_values_coords.meet_expected_speed.R")
no_values_coords_for_speed <- no_values_coords.meet_expected_speed(lat_prev,lon_prev, time_diff/1000000000, speed_expected)
mean <- round(mean(no_values_coords_for_speed,na.rm=T),0)
median <- median(no_values_coords_for_speed,na.rm=T)

setwd(resDir)
graphics.off()
png(filename = paste("no.of_values_speed__",data_set_name,".png",sep=""), width = 16, height = 16, units = "in", res = 75)

plot(no_values_coords_for_speed, 
     main="amount of values (coordinates) needed to meet expected speed",
     xlab = "",
     ylab= ""
)

### add coordinates
par(new=TRUE)
plot(lon_prev,pch=21, col="red",
     xlab = "",
     ylab= "")
par(new=TRUE)
plot(lat_prev,pch=21, col="green",
     xlab = "",
     ylab= "")


## add accuracy
par(new=TRUE)
plot(accuracy,type="l", col="blue",
     xlab = "",
     ylab= "")

legend('topleft', 
       c("no_values","lat","lon","accuracy",
         paste("mean = ",mean,sep=""),
         paste("median = ",median,sep="")),
       col=c("black","red","green","blue"),
       pch = c(19,19,19,NA),
       lty = c(NA,NA,NA,1),
       cex=0.8)

# add distance and time difference
#par(new=TRUE)
#plot(distance_R,type="l", col="blue")
#par(new=TRUE)
#plot(time_diff,type="l", col="green")

dev.off()