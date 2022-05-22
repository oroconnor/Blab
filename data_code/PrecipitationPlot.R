library(tidyverse)
library(lubridate)
library(feather)
library(zoo)

#read in the tibble prepared by weatherdata.R script
fulldf <- read_feather("fulldf.feather")

fulldf <- fulldf %>%
  mutate(
    prec_movavg = rollmean(precip_max_hr, 24, fill = NA, align = "right")
  )

ggplot(fulldf, aes(Time,y)) +
  geom_point(aes(y = precip_max_hr))

ggplot(fulldf, aes(Time,y)) +
  geom_point(aes(y = pm2.5), color = "red" ) +
  geom_point(aes(y = precip_max_hr *300), color = "blue" )