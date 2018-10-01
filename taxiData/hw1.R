library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)
library(data.table)
library(foreach)

################## Helper functions ##################

clean_data <- function(trip_data, trip_fare){
  #to remove possible illegal characters
  data_names <- make.names(names(trip_data))
  fare_names <- make.names(names(trip_fare))
  setnames(trip_data, data_names)
  setnames(trip_fare, fare_names)
  setkey(trip_data, medallion, hack_license, vendor_id, pickup_datetime)
  setkey(trip_fare, medallion, hack_license, vendor_id, pickup_datetime)
  print("Removing duplicates")
  trip_data <- trip_data[!duplicated(trip_data),]
  trip_fare <- trip_fare[!duplicated(trip_fare),]
  print("Merging trips and fares")
  taxis <- trip_data[trip_fare]#merge trips and fares
  taxis <- taxis[,.(hack_license, pickup_datetime, dropoff_datetime, trip_time_in_secs,
                    trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude,
                    dropoff_latitude, fare_amount, surcharge, mta_tax, tip_amount, tolls_amount, 
                    total_amount)]#select relevant columns
  names_taxis <- c("license", "p_time", "d_time", "trip_time", 
                   "trip_dist", "p_long", "p_lat", "d_long",
                   "d_lat", "fare_amount", "surcharge", "mta_tax",
                   "tip_amount", "tolls_amount", "total_amount")
  setnames(taxis, names_taxis)
  #remove outliers
  print("Remove outliers")
  print("Trip_time")
  n<- nrow(taxis)
  taxis <- taxis[trip_time > 0]
  max_val <- quantile(taxis$trip_time, 0.99)
  min_val <- quantile(taxis$trip_time, 0.01)
  taxis <- taxis[trip_time >= min_val & trip_time <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in trip distances
  print("Trip_dist")
  n<- nrow(taxis)
  max_val <- quantile(taxis$trip_dist, 0.999)
  min_val <- quantile(taxis$trip_dist, 0.01)
  taxis <- taxis[trip_dist >= min_val & trip_dist <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in fare amount
  print("Fare_amount")
  n<- nrow(taxis)
  taxis <- taxis[fare_amount > 0]
  max_val <- quantile(taxis$fare_amount, 0.999)
  min_val <- quantile(taxis$fare_amount, 0.01)
  taxis <- taxis[fare_amount >= min_val & fare_amount <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in surcharge
  print("Surcharge")
  n<- nrow(taxis)
  taxis <- taxis[surcharge >= 0]
  max_val <- quantile(taxis$surcharge, 0.999)
  min_val <- quantile(taxis$surcharge, 0.01)
  taxis <- taxis[surcharge >= min_val & surcharge <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in mta_tax
  print("Mta_tax")
  n<- nrow(taxis)
  taxis <- taxis[mta_tax >= 0]
  max_val <- quantile(taxis$mta_tax, 0.999)
  min_val <- quantile(taxis$mta_tax, 0.01)
  taxis <- taxis[mta_tax >= min_val & mta_tax <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in tip_amount
  print("Tip_amount")
  n<- nrow(taxis)
  taxis <- taxis[tip_amount >= 0]
  max_val <- quantile(taxis$tip_amount, 0.999)
  min_val <- quantile(taxis$tip_amount, 0.01)
  taxis <- taxis[tip_amount >= min_val & tip_amount <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in tolls_amount
  print("Tolls_amount")
  n<- nrow(taxis)
  taxis <- taxis[tolls_amount >= 0]
  max_val <- quantile(taxis$tolls_amount, 0.999)
  min_val <- quantile(taxis$tolls_amount, 0.01)
  taxis <- taxis[tolls_amount >= min_val & tolls_amount <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  #remove outliers in total_amount
  print("Total_amount")
  n<- nrow(taxis)
  taxis <- taxis[total_amount >= 0]
  max_val <- quantile(taxis$total_amount, 0.999)
  min_val <- quantile(taxis$total_amount, 0.01)
  taxis <- taxis[total_amount >= min_val & total_amount <= max_val]
  print(paste(n-nrow(taxis), " lines removed ", "(",round(nrow(taxis)/n, 2), "%)", sep = ""))
  
  print("Convert dates and times")
  print("p_time")
  taxis$p_time <- ymd_hms(taxis$p_time)
  print("d_time")
  taxis$d_time <- ymd_hms(taxis$d_time)
  
  return(taxis)
}

calculate_idle_time <- function(dt, license_number){
  require(data.table)
  by_license <- dt[license == license_number, .(license, p_time, d_time)]
  n <- nrow(by_license)
  idle_time <- NULL
  for(i in 1:(n-1)){
    idle_time <- append(idle_time, 
                        as.numeric(difftime(by_license$p_time[i+1], by_license$d_time[i], units = "mins")),#idle_time in seconds
                        after = length(idle_time))
  }
  idle_time <- append(idle_time, NA, after = length(idle_time))
  return(idle_time)
}

calculate_trip_time <- function(dt, license_number){
  require(data.table)
  by_license <- dt[license == license_number, .(license, p_time)]
  n <- nrow(by_license)
  trip_time <- NULL
  for(i in 1:(n-1)){
    trip_time <- append(trip_time, 
                        as.numeric(difftime(by_license$p_time[i+1], by_license$p_time[i], units = "mins")),#idle_time in seconds
                        after = length(trip_time))
  }
  trip_time <- append(trip_time, NA, after = length(trip_time))
  return(trip_time)
}

################## Main program ##################

#read Aug15, 2013 data
trip <- read.csv("/Volumes/Transcend\ 1/Documents/University/Masters/md_ml/Data_HW1/trips_hw_1.csv")
#subset of the New York City taxi trip data. Each row corresponds to an occupied taxi trip.
head(trip)
#summary of the imported dataset
str(trip)
fare <- read.csv("/Volumes/Transcend\ 1/Documents/University/Masters/md_ml/Data_HW1/fares_hw_1.csv")
#subset of the New York City taxi fare data. Each row corresponds to an occupied taxi trip.
head(fare)
#summary of the imported dataset
str(fare)

#convert to data.table
trip_dt <- as.data.table(trip)
fare_dt <- as.data.table(fare)

#extract columnd names
data_names <- names(trip_dt)
fare_names <- names(fare_dt)

#prepare dataset
taxis <- clean_data(trip_dt, fare_dt)

#extract licenses
licenses <- unique(taxis$license)
licenses_number <- length(licenses)

#idle time: time between drop-off time and next pick-up time
taxis[, Idle_mins := foreach(i = licenses, .combine = "c") 
      %do% calculate_idle_time(taxis, i)]
#remove negatives and zeros
taxis <- taxis[!is.na(taxis$Idle_mins) & Idle_mins > 0, ]

#trip time: time between drop-off time and pick-up time 
taxis[, trip_time_mins := foreach(i = licenses, .combine = "c") 
      %do% calculate_trip_time(taxis, i)]
#remove negatives and zeros
taxis <- taxis[!is.na(taxis$trip_time_mins) & trip_time_mins > 0, ]
taxis <- taxis[trip_time_mins > quantile(taxis$trip_time_mins, 0.01) & trip_time_mins < quantile(taxis$trip_time_mins, .95),]

#calculate time between two consecutive pick-ups
taxis[,consecutive_TT := Idle_mins + trip_time_mins]

#idle_times between pick-ups longer than 3 hours are assumed to be breaks and assigned 0 time length value
#therefore breaks are not included in the calculation of the total time worked
taxis[Idle_mins > 60, consecutive_TT := 0]
summary(taxis$consecutive_TT)
nrow(taxis[consecutive_TT == 0])

################## Plots ##################
#plot reported distance disctribution from dataset
distance <- trip[,c(1,2,9,10)]
distance_summary <- summarise(group_by(distance, hack_license), avg_dist = mean(trip_distance))
distance_summary <- distance_summary[distance_summary$avg_dist < quantile(distance_summary$avg_dist, 0.99),]
g <- ggplot(distance_summary, aes(x = avg_dist, y = ..density..))
g <- g + geom_histogram(colour = "black", fill = "steelblue", binwidth = .1)
g + geom_density(colour = "red", size = 1, adjust = 4)

#plot average number of hours per day
working_hours <- taxis[,.(hours_day = sum(consecutive_TT)), by = .(license)]
working_hours <- working_hours[,.(avg_hours_day = mean(hours_day)/60), by = .(license)]

hist(working_hours$avg_hours_day, breaks = 24, col = "steelblue",
     main = "Average number of working hours", xlab = "hours/day", xlim = c(0,24), ylab = "count", ylim = c(0, 3000))

abline(v = median(working_hours$avg_hours_day), col = "red")
text(x = 20, y = 400, 
     labels = paste("Median:", round(median(working_hours$avg_hours_day),1), "hrs"), 
     col = "black", cex = 0.8)

#plot median earnings (hourly fare)
earnings_by_license <- taxis[,.(eranings = sum(fare_amount)*60/sum(consecutive_TT)), by = .(license)]
earnings_by_license <- earnings_by_license[eranings != Inf & eranings < quantile(earnings_by_license$eranings, 0.995)]
summary(earnings_by_license$eranings)
hist(earnings_by_license$eranings, breaks = 50, col = "steelblue", 
     main = "Fare distribution by license", xlab = "$/hr", xlim = c(0,100), ylab = "count", ylim = c(0, 6000))
abline(v = median(earnings_by_license$eranings), col = "red")
text(x = 50, y = 700, 
     labels = paste("median hourly fare:", round(median(earnings_by_license$eranings),2), "$/hr"), 
     col = "black", cex = 0.65, pos = 4)
