#Loads weather data and the airmasterk tibble and creates a new tibble ("fulldf")
#for use in other scripts

library(tidyverse)
library(lubridate)
library(feather)
library(openair)

#the .csv filename should be the latest and greatest weather file that you 
#want to use for weather data
weatherdata <- "Broido_Weather_Data.csv"

weatherdf <- read_csv(weatherdata)
#change time strings to datetime type

weatherdf$date <- dmy(weatherdf$Date)



wind <- weatherdf %>%
  select("date","Time","Wind Speed_current (mph)","Avg._Wind_bearing (deg)")



wind$wind_speed_current = wind$"Wind Speed_current (mph)"
wind$avg_wind_bearing = wind$"Avg._Wind_bearing (deg)"
  

hour(wind$date) <- hour(wind$Time)
minute(wind$date) <- minute(wind$Time)

wind <- wind %>%
  select(
    date, wind_speed_current, avg_wind_bearing
    )

#read in the tibble prepared by airmasterk.R script
airmasterk <- read_feather("airmasterk.feather")

airmasterk <- airmasterk %>%
  mutate(
    date = Time
  )

#combines airmasterk data with the Broido weather data
fullbroidodf <- right_join(airmasterk, wind, by = "date",copy = FALSE, suffix = c(".x", ".y"))

#Saving to a feather file so that other scripts can use prepared tibble. 
write_feather(fullbroidodf,"fullbroidodf.feather")


fullbroidodf <- fullbroidodf %>%
  mutate(
    ws = wind_speed_current,
    wd = avg_wind_bearing
  )

#excludes few outliers or problem values
fullbroidodf <- fullbroidodf %>%
  filter(
    pm2.5 >= 0,
    pm2.5 < 200
  )

#create a tibble with just July 4th data. I'm not sure that zooming
# in on a single day is actually a useful exercise, as it may indicate
#more about where the wind was blowing that day than anything useful about
#pollution sources. Probably more useful to look at over larger chuncks 
#of time.
july4df <- fullbroidodf %>%
  filter(
    month(Time) == 7,
    day(Time) == 4
  )
#creates a windrose from the dataset
windRose(fullbroidodf)

#creates the pollution rose. Arguments are the data and the pollutant.
pollutionRose(july4df, pollutant = "pm2.5")

pollutionRose(fullbroidodf, pollutant = "pm2.5", main ="Frequency of Counts by Wind Direction", sub = expression(PM2.5 (μg/~m^3))) 

#by season
pollutionRose(fullbroidodf, pollutant = "pm2.5",type = "season", main ="Frequency of Counts by Wind Direction",  sub = expression(PM2.5 (μg/~m^3))) 

july4df <- fullbroidodf %>%
  filter(
    month(Time) == 7,
  )
pollutionRose(july4df, pollutant = "pm2.5")





rhweatherdata <- "Mesodata_master.csv"

rhweatherdf <- read_csv(rhweatherdata)

#change time strings to datetime type
rhweatherdf<-mutate(rhweatherdf, date= parse_date_time(timestamp,"YmdHMS"))



rhweather <- rhweatherdf %>%
  select(date,wind_dir_sonic,avg_wind_sonic_mph)

fulldf <- inner_join(rhweather, wind, by = "date",copy = FALSE, suffix = c(".x", ".y"))


#correlation tests
res <- cor.test(fulldf$avg_wind_sonic_mph, fulldf$wind_speed_current, method = "pearson")

dirres <- cor.test(fulldf$avg_wind_bearing, fulldf$wind_dir_sonic, method = "pearson")

#Comparing Wind spee
ggplot(data = fulldf,
       aes(x = date)) +
  geom_point(aes(y = avg_wind_sonic_mph), color = "red", size= 1.4) +
 
  #geom_line(aes(y = daily_mean), color = "red") +
  #scale_x_date(limits = as.Date(c("2020-01-16","2021-01-16")), date_breaks = "months", date_labels = "%b") +
  labs(
    y = "Wind Speed MPH",
    x = "Date",
    title = paste(
      "2020 Wind Speeds - Kingston"
    ),
    subtitle = paste(
      "PM2.5 Daily Averages"
    )) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


ggplot(data = fulldf) +
  geom_point(mapping = aes(x = avg_wind_sonic_mph, y = wind_speed_current),
             color = "blue", size = .3, alpha = .05) +
  geom_smooth(mapping = aes(x = avg_wind_sonic_mph, y = wind_speed_current)) +
  labs(
    y = "Broido: avg_wind_sonic_mph",
    x = "Red Hook: wind_speed_current",
    title = paste(
      "Red Hook vs Broido Wind Speed"
    ))
