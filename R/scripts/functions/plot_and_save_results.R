#######################################################
######### wirte and append xcel table

setwd(homeDir)
write.table(results, "results/results.csv", sep="\t", row.names=FALSE, 
            col.names = !file.exists("results/results.csv"), append = T)


#######################################################
######### save the data_set

library("gridExtra")
setwd(resDir)
png(paste("data_set_",data_set_name,".png",sep=""), height = 50*nrow(data_set), width = 200*ncol(data_set))
grid.table(data_set)
dev.off()


#######################################################
######### plot linear regression of path (lat, lon)

graphics.off()
png(filename = paste("Linear_Regression_of_path__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)
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

#######################################################
######### plot distance against accuracy and time_diff

graphics.off()
png(filename = paste("distance_against_accuracy__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)
plot(distance,col="red", main="plot distance against accuracy",xlab="",ylab="")
par(new=TRUE)
plot(accuracy,col="green",xlab="",ylab="")
legend('topleft', 
       c("distance","accuracy"),
       col=c("red","green"),
       pch = c(19,19),
       cex=0.8)
dev.off()

#######################################################
######### plot distance against accuracy and time_diff

graphics.off()
png(filename = paste("lat_lon_against_accuracy__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)
plot(lat_prev,col="red", main="lat and lon, against accuracy")
par(new=TRUE)
plot(lon_prev, col="blue",xlab="",ylab="")
par(new=TRUE)
plot(accuracy,type="l", col="green",xlab="",ylab="")
legend('topright', 
       c("lat","lon","accuracy"),
       col=c("red","blue","green"),
       pch = c(19,19,NA),
       lty = c(NA,NA,1),
       cex=0.8)
dev.off()

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




plot(as.numeric(bearing),col="blue",pch = 19)

plot(bearing_calc_A_R,col="black",pch = 19)
par(new=TRUE)
plot(distance,col="blue",pch = 19)
par(new=TRUE)
plot(accuracy,col="blue",pch = 19)
par(new=TRUE)
plot(lon_prev,lat_prev,asp=1,col="red",pch = 19)


# compare bearing of Andorid with R
# both estimtated with same formula

setwd(resDir)
graphics.off()
png(filename = paste("compare_bearings__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)

plot(as.numeric(bearing),col="blue",pch = 19)
lines(bearing_calc_A_R,col="black")
par(new=TRUE)
plot(accuracy,col="red",pch = 21)
title(main="compare bearing of Andorid with R", sub="both estimtated with same formula")
legend('topleft', 
       c("bearing_Android","bearing_R","accuracy"),
       col=c("blue","black","red"),
       pch = c(19, NA,21),
       lty=c(NA,1,NA), 
       cex=0.8
)
dev.off()


# compare distance, timeDiff and accuracy
# both estimtated with same formula

setwd(resDir)
graphics.off()
png(filename = paste("distance_timeDiff_accuracy__",data_set_name,".png",sep=""), width = 7, height = 7, units = "in", res = 75)

plot(as.numeric(distance),col="blue",type = "l",lty=1)
par(new=TRUE)
plot(time_diff,col="black",type = "l",lty=1)
par(new=TRUE)
plot(accuracy,col="red",type = "l",lty=1)
title(main="compare distance, timeDiff and accuracy")
legend('topleft', 
       c("distance","time_diff","accuracy"),
       col=c("blue","black","red"),
       pch = c(21,21,NA),
       lty=c(NA,NA,1), 
       cex=0.8
)
dev.off()

#########################################