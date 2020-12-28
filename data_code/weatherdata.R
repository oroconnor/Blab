#Loads weather data and the airmasterk tibble and creates a new tibble ("fulldf")
#for use in other scripts

library(tidyverse)
library(lubridate)
library(feather)


#the .csv filename should be the latest and greatest weather file that you 
#want to use for weather data
weatherdata <- "Mesodata_master.csv"

weatherdf <- read_csv(weatherdata)

#change time strings to datetime type
weatherdf<-mutate(weatherdf, Time= parse_date_time(timestamp,"YmdHMS"))

weather <- weatherdf %>%
  select(Time,temp_2m,temp_9m,wind_dir_sonic,avg_wind_sonic_mph, precip_local, precip_max_hr)

#read in the tibble prepared by airmasterk.R script
airmaster <- read_feather("airmasterk.feather")


#combines airmasterk data with the Red Hook weather data
fulldf <- right_join(airmasterk, weather, by = "Time",copy = FALSE, suffix = c(".x", ".y"))

#Saving to a feather file so that other scripts can use prepared tibble. 
write_feather(fulldf,"fulldf.feather")



