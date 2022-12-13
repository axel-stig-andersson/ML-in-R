# ASSIGNMENT 1
library(geosphere)
stations <- read.csv("data/stations.csv", stringsAsFactors = T, fileEncoding = "latin1") 
temps <- read.csv("data/temps50k.csv")
st <- merge(stations,temps,by="station_number")



target_date <- as.Date("2017-05-06") # The date to predict (up to the students)# Filtering out all the dates that were before the one to predict.
temps_prev <- temps[temps$date < target_date,]

h_distance <- 100000
h_date <- 0.15
h_time <- 0.45

# Coordinates
lat <- 58.4274 
long <- 14.826
# Making two vectors of the sane lat and long, so that we can compare it to every station later.
compare_lat <- rep(lat, nrow(st))
compare_long <- rep(long, nrow(st))
target_vec <- as.matrix(cbind(compare_lat, compare_long))

# Coordinates of all observations, which means a station occurs several times. 
stat_pos <- cbind(st$latitude, st$longitude)
# Calculate distances 

distance <- distHaversine(target_vec, stat_pos)


times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))
# Students’ code here
plot(temp, type="o")

#---------------------------------------------------------------------------------
#------------------------------------FUNCTIONS---------------------------------------
#---------------------------------------------------------------------------------
gaussian <- function(input, h_value){ 
  val <- input/h_value
  output <- exp(-val*val)
  return( output )
} 

daydiff <- function(measureDay, targetDay){
  output <- as.Date(targetDay) - as.Date(measureDay)
  return( sin(pi*as.numeric(output)/365) )
}

hourdiff <- function(time2, time1) {
  timegap <- as.numeric(difftime(strptime(time1,format = "%H:%M:%S"), strptime(time2,format="%H:%M:%S")), units="hours")
  return (sin(pi*(timegap)/24))
}


#---------------------------------------------------------------------------------
#------------------------------------Task 1---------------------------------------
#---------------------------------------------------------------------------------

hourdiff("20:00:00", "08:00:00")

pred_temps_sum = c()
pred_temps_mult <- c()
pred_temps_2 = c()
kernel_distance <- gaussian(distance, h_distance)
kernel_days <- gaussian(daydiff(temps_prev$date,target_date),h_date)

#---------------------------------------------------------------------------------
#------------------------------------Task 1---------------------------------------
#---------------------------------------------------------------------------------

for (i in 1:length(times)) {
  kernel_hours <- gaussian(hourdiff(times[i],temps_prev$time),h_time)
  all_kernels_sum <- (kernel_distance + kernel_days + kernel_hours)
  tmp_sum <- sum(all_kernels_sum%*%temps_prev$air_temperature)/sum(all_kernels_sum)
  pred_temps_sum[i] <- tmp_sum
  
  all_kernels_mult <- (kernel_distance * kernel_days * kernel_hours)
  tmp_mult <- sum(all_kernels_mult%*%temps_prev$air_temperature)/sum(all_kernels_mult)
  pred_temps_mult[i] <- tmp_mult
}

pred_temps_mult
check <- (as.numeric(as.Date(target_date) - as.Date(st$date),
                     unit="days") %% 365) 
close.dates <- which(c(check, 365-check)<4)
close.dates <- st[close.dates,]
means <- tapply(close.dates$air_temperature, as.factor(close.dates$time), mean)
means <- means[times]
plot(pred_temps_sum, ylab="Temperature (C)", xlab="Time", 
     main = "Temperature estimation", col="blue", type="o", 
     ylim=c(-10, 30),
     xaxt = "n")
axis(1, at=1:length(times), labels=substring(times,1,2))
lines(pred_temps_mult, col="green", type="o")
lines(means, col="red", type="o")
legend(x = "topleft", legend = c("Sum Kernel", "Product Kernel","Mean temp history", target_date),
       col = c("blue", "green","red", "Black"), pch = 20, cex = 0.6)
