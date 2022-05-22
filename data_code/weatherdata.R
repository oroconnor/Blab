# Loads weather data and the airmasterk tibble and creates a new tibble ("fulldf")
# for use in other scripts

library(tidyverse)
library(lubridate)
library(feather)


#the .csv filename should be the latest and greatest weather file that you 
#want to use for weather data
weatherdata <- "Mesodata_master_final.csv"

weatherdf <- read_csv(weatherdata)

#change time strings to datetime type
weatherdf<-mutate(weatherdf, Time= parse_date_time(timestamp,"YmdHMS"))

weather <- weatherdf %>%
  select(Time,temp_2m,temp_9m,wind_dir_sonic,avg_wind_sonic_mph, precip_local, precip_max_hr)

#read in the tibble prepared by airmasterk.R script
airmasterk <- read_feather("airmasterk.feather")


#combines airmasterk data with the Red Hook weather data
fulldf <- right_join(airmasterk, weather, by = "Time",copy = FALSE, suffix = c(".x", ".y"))

#Saving to a feather file so that other scripts can use prepared tibble. 
write_feather(fulldf,"fulldf.feather")


#Below is little plots looking at rainy days
ggplot(subset(weather, month(Time) == 3 & day(Time) >11 & day(Time) < 15), aes(Time, precip_local)) +
  geom_point()       

#March  rain gauge
ggplot(subset(weather, month(Time) == 3 &
              ((day(Time) == 12 & hour(Time) > 11) | day(Time) == 13 | (day(Time) == 14 & hour(Time) < 13))), aes(Time, precip_local)) +
  geom_point() +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%b%e %l %p") +
  labs(
    x = NULL,
    y = "Rain Gauge Level",
    title = paste(
      "Rain: March 12-14 2020"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica"),
    legend.position = "none"
  )



ggplot(subset(weather, month(Time) == 3 & day(Time) >11 & day(Time) < 15), aes(Time, precip_max_hr)) +
  geom_point() +
  annotate("rect", xmin = ymd_hms("2020-03-12 23:30:00"), xmax = ymd_hms("2020-03-13 11:30:00"), ymin = 0, ymax = 1,
           alpha = .1, fill = "blue")

ggplot(subset(weather, month(Time) == 4 & day(Time) == 9 ), aes(Time, precip_max_hr)) +
  geom_point()       

#April 9 rain gauge
ggplot(subset(weather, month(Time) == 4 & day(Time) == 9), aes(Time, precip_local)) +
  geom_point() +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%b%e %l %p") +
  labs(
    x = NULL,
    y = "Rain Gauge Level",
    title = paste(
      "Rain: April 9 2020"
    )
  ) +
 theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica"),
    legend.position = "none"
  )


