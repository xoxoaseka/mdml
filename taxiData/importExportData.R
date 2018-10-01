library(magrittr)
library(plyr) 
library(dplyr)
library(ggplot2)

# range of dates we're interested in
range_begin <- as.Date("2013-08-15")
range_end <- as.Date("2013-08-15")

# read the files, add a ymd col and get the dates in our range
trip_fare <- read.csv("/Volumes/Transcend\ 1/Documents/University/Masters/md_ml/Data_HW1/trip_fare_8.csv")
trip_fare <- trip_fare %>%
  mutate(ymd_pickup = as.Date(pickup_datetime) ) %>% 
  filter(ymd_pickup >= range_begin & ymd_pickup <= range_end)
#save output
write.csv(trip_fare, file = "fares_hw_1.csv")

trip_data <- read.csv("/Volumes/Transcend\ 1/Documents/University/Masters/md_ml/Data_HW1/trip_data_8.csv")
trip_data <- trip_data %>%
  mutate(ymd_pickup = as.Date(pickup_datetime) ) %>% 
  filter(ymd_pickup >= range_begin & ymd_pickup <= range_end)

#save output
write.csv(trip_data, file = "trips_hw_1.csv")