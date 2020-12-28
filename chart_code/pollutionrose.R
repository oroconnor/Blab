library(openair)
library(tidyverse)
library(lubridate)
library(feather)

#read in the tibble prepared by weatherdata.R script
fulldf <- read_feather("fulldf.feather")

#renames the columns in a way that pollutionrose() can understand
fulldf <- fulldf %>%
  mutate(
    ws = avg_wind_sonic_mph,
    wd = wind_dir_sonic
  )

#excludes few outliers or problem values
fulldf <- fulldf %>%
  filter(
    pm2.5 >= 0,
    pm2.5 < 100
  )

#create a tibble with just July 4th data. I'm not sure that zooming
# in on a single day is actually a useful exercise, as it may indicate
#more about where the wind was blowing that day than anything useful about
#pollution sources. Probably more useful to look at over larger chuncks 
#of time.
july4df <- fulldf %>%
  filter(
    month(Time) == 7,
    day(Time) == 4
  )
#creates a windrose from the dataset
windRose(fulldf)

#creates the pollution rose. Arguments are the data and the pollutant.
pollutionRose(july4df, pollutant = "pm2.5")

pollutionRose(fulldf, pollutant = "pm2.5")