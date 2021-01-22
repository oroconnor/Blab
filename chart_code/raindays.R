###creates some plots looking at particle counts and PM2.5/PM10 for rainy spring days


library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

airmasterk <- read_feather("airmasterk.feather")



#Counts  - Rainy March Day
ggplot(subset(gather(airmasterk,`Particle Size`, Value,c0.7:c10.0), month(Time) == 3 & day(Time) > 11 & day(Time) < 15),
       aes(Time,Value, color = `Particle Size`)) +
  geom_point(size= .3, alpha =.4) +
  scale_y_continuous(limits = c(0,3000)) +
  labs(
    y = "Counts",
    title = paste(
      "March 13 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "Rain noted by blue rectangle"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  annotate("rect", xmin = ymd_hms("2020-03-12 23:30:00"), xmax = ymd_hms("2020-03-13 11:30:00"), ymin = 0, ymax = 3000,
           alpha = .1, fill = "blue")

#Pm2.5 and PM10  - Rainy March Day
ggplot(subset(gather(airmasterk,`Particle Size`, Value, pm2.5:pm10), month(Time) == 3 & day(Time) > 11 & day(Time) < 15),
       aes(Time,Value, color = `Particle Size`)) +
  geom_point(size= .3) +
  scale_y_continuous(limits = c(0,160)) +
  labs(
    y = "mg/cubic meter",
    title = paste(
      "March 13th 2020 PM2.5 and Pm10"
    ),
    subtitle = paste(
      "Rain noted by blue rectangle"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  annotate("rect", xmin = ymd_hms("2020-03-12 23:30:00"), xmax = ymd_hms("2020-03-13 11:30:00"), ymin = 0, ymax = 160,
           alpha = .1, fill = "blue")


# #Counts  - Rainy April Day - LEGACY CODE - now using gather and legend
# ggplot(subset(airmasterk, month(Time) == 4 & day(Time) == 9),
#        aes(Time,y)) +
#   geom_point(aes(y = c0.7), size= .3, color = "red", alpha =.4) +
#   geom_point(aes(y = c1.0), size= .3, color = "orange", alpha =.4) +
#   geom_point(aes(y = c2.0), size= .3, color = "yellow", alpha =.4) +
#   geom_point(aes(y = c3.0), size= .3, color = "green", alpha =.4) +
#   geom_point(aes(y = c5.0), size= .3, color = "blue", alpha =.4) +
#   geom_point(aes(y = c10.0), size= .3, color = "purple", alpha =.4) +
#   scale_y_continuous(limits = c(0,4000)) +
#   labs(
#     y = "Counts",
#     title = paste(
#       "April 9th 2020 Counts from Particulate Matter Sensor"
#     ),
#     subtitle = paste(
#       "Rain noted by blue rectangle"
#     )
#   ) +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) +
#   annotate("rect", xmin = ymd_hms("2020-04-09 11:00:00"), xmax = ymd_hms("2020-04-09 15:00:00"), ymin = 0, ymax = 4000,
#            alpha = .1, fill = "blue")


#Counts  - Rainy April Day
ggplot(subset(gather(airmasterk,`Particle Size`, Counts, c0.3:c10.0), month(Time) == 4 & day(Time) == 9),
       aes(Time,Counts, color = `Particle Size`)) +
  geom_point(size= .3, alpha =.9) +
  #scale_y_continuous(limits = c(0,4000)) +
  labs(
    y = "Counts",
    title = paste(
      "April 9th 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "Rain noted by blue rectangle"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  annotate("rect", xmin = ymd_hms("2020-04-09 11:00:00"), xmax = ymd_hms("2020-04-09 15:00:00"), ymin = 0, ymax = 150000,
           alpha = .1, fill = "blue")

#Counts  - Rainy April Day - Without smallest bin size
ggplot(subset(gather(airmasterk,`Particle Size`, Counts, c0.7:c10.0), month(Time) == 4 & day(Time) == 9),
       aes(Time,Counts, color = `Particle Size`)) +
  geom_point(size= .3, alpha =.9) +
  scale_y_continuous(limits = c(0,4000)) +
  labs(
    y = "Counts",
    title = paste(
      "April 9th 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "Rain noted by blue rectangle"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  annotate("rect", xmin = ymd_hms("2020-04-09 11:00:00"), xmax = ymd_hms("2020-04-09 15:00:00"), ymin = 0, ymax = 4000,
           alpha = .1, fill = "blue")

#Pm2.5 and PM10  - Rainy April Day
ggplot(subset(gather(airmasterk,`Particle Size`, Value, pm2.5:pm10), month(Time) == 4 & day(Time) == 9),
       aes(Time,Value, color = `Particle Size`)) +
  geom_point(size= .3) +
  scale_y_continuous(limits = c(0,65)) +
  labs(
    y = "mg/cubic meter",
    title = paste(
      "April 9th 2020 PM2.5 and Pm10"
    ),
    subtitle = paste(
      "Rain noted by blue rectangle"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  annotate("rect", xmin = ymd_hms("2020-04-09 11:00:00"), xmax = ymd_hms("2020-04-09 15:00:00"), ymin = 0, ymax = 65,
           alpha = .1, fill = "blue")
