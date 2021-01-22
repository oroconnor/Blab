library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

airmasterk <- read_feather("airmasterk.feather")


#December 12 mass - Melted for legend
ggplot(subset(gather(airmasterk,Particle_Size, Mass, mass0.4:mass4.0), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Mass, color = Particle_Size)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d %l %p") +
  scale_y_continuous(limits = c(0,15)) +
  labs(
    y = "Mass - mg/cubic meter",
    title = paste(
      "December 2020 Mass by Particulate Size"
    ),
    subtitle = paste(
      "m.4-m4.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#Dec 12 Counts  .7-10
ggplot(subset(gather(airmasterk,Particle_Size, Counts, c0.7:c10.0), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  scale_y_continuous(limits = c(0,7000)) +
  labs(
    y = "Counts",
    title = paste(
      "December 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "sizes 0.7 - 10.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))

#PM2.5
ggplot(subset(airmasterk, month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,y)) +
  geom_point(aes(y = pm2.5), size= .3, color = "red", alpha =.4) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d %l %p") +
  #scale_y_continuous(limits = c(0,50)) +
  labs(
    y = "PM2.5 - mg/cubic meter",
    title = paste(
      "December 11/12/23 2020 "
    ),
    subtitle = paste(
      "PM2.5"
    )) +
  theme(axis.text.x=element_text(angle=60, hjust=1))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))




#December 12 mass - w/ mass7.5
ggplot(subset(gather(airmasterk,Particle_Size, Mass, mass0.4:mass7.5), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Mass, color = Particle_Size)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d %l %p") +
 # scale_y_continuous(limits = c(0,15)) +
  labs(
    y = "Mass - mg/cubic meter",
    title = paste(
      "December 2020 Mass by Particulate Size"
    ),
    subtitle = paste(
      "m.4-m7.5"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#mass m20
ggplot(subset(airmasterk, month(Time) == 12 & day(Time) > 10 & day(Time) < 15),
       aes(Time,y)) +
  geom_point(aes(y = mass20 ), size= .3, color = "turquoise", alpha =.4) +
  #scale_y_continuous(limits = c(0,50)) +
  labs(
    y = "Mass",
    title = paste(
      "December 2020 Mass"
    ),
    subtitle = paste(
      "mass20"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))


#Counts Log scale
ggplot(subset(gather(airmasterk,Particle_Size, Counts, c0.3:c10.0), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  scale_y_log10() +
  labs(
    y = "Counts",
    title = paste(
      "December 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "sizes 0.7 - 10.0 on Log10Scale"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))







