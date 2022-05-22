###creates some plots looking at particle counts and PM2.5/PM10 for rainy spring days


library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

airmasterk <- read_feather("airmasterk.feather")

rainmarchk <-airmasterk %>%
  filter(
    month(Time) == 3,
    (day(Time) == 12 & hour(Time) > 11) | day(Time) == 13 | (day(Time) == 14 & hour(Time) < 13),
  )

rainaprilk <- airmasterk %>%
  filter(
    month(Time) == 4,
    (day(Time) == 9),
  )

#Rainy March Day Mass Melted for Legend
ggplot(subset(gather(rainmarchk,`Particle Diameter (microns)`, Mass, mass0.4:mass2.5)),
       aes(Time,Mass, color = `Particle Diameter (microns)`)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,10)) +
  labs(
    x = "Mar 12                            Mar 13                              Mar 14",
    y = expression(Mass - (μg/~m^3)),
    title = paste("Effect of Rain on Airborne Particulate Circulation"),
    subtitle = paste(
      "Rain Highlighted in Blue ")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.4","0.6","0.85","1.5","2.5")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1))) +
  annotate("rect", xmin = ymd_hms("2020-03-12 23:30:00"), xmax = ymd_hms("2020-03-13 11:30:00"), ymin = 0, ymax = 10,
         alpha = .1, fill = "blue")

#Rainy March Day - Counts = faceted
ggplot(transform(gather(rainmarchk,Particle_Size, Counts, c0.3:c10.0),Particle_Size = factor(Particle_Size, levels = c("c0.3","c0.5","c0.7","c1.0","c2.0","c3.0","c5.0","c10.0"))),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "12 hours", date_labels = "%b%e %l %p") +
  labs(
    x = NULL,
    y = "Particle Counts",
    title = paste(
      "Particulate Matter Counts: March 2020"
    ),
    subtitle = paste(
      "sizes 0.3 - 10.0, Rain Highlighted in Blue")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica"),
    legend.position = "none"
  ) +
  facet_wrap(~Particle_Size, ncol = 4, scales = "free") +
  annotate("rect", xmin = ymd_hms("2020-03-12 23:30:00"), xmax = ymd_hms("2020-03-13 11:30:00"), ymin = 0, ymax = Inf,
         alpha = .1, fill = "blue")

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


### Below is For the April rain day


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

#Rainy April Day - Counts = faceted
ggplot(transform(gather(rainaprilk,Particle_Size, Counts, c0.3:c10.0),Particle_Size = factor(Particle_Size, levels = c("c0.3","c0.5","c0.7","c1.0","c2.0","c3.0","c5.0","c10.0"))),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%b%e %l %p") +
  labs(
    x = NULL,
    y = "Particle Counts",
    title = paste(
      "Particulate Matter Counts: April 2020"
    ),
    subtitle = paste(
      "sizes 0.3 - 10.0, Rain Highlighted in Blue")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica"),
    legend.position = "none"
  ) +
  facet_wrap(~Particle_Size, ncol = 4, scales = "free") +
  annotate("rect", xmin = ymd_hms("2020-04-09 11:00:00"), xmax = ymd_hms("2020-04-09 15:00:00"), ymin = 0, ymax = Inf,
           alpha = .1, fill = "blue")

#Rainy April Day Mass Melted for Legend
ggplot(subset(gather(rainaprilk,`Particle Diameter (microns)`, Mass, mass0.4:mass2.5)),
       aes(Time,Mass, color = `Particle Diameter (microns)`)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,10)) +
  labs(
    x = "Apr 9",
    y = expression(Mass - (μg/~m^3)),
    subtitle = paste(
      "Rain Highlighted in Blue ")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.4","0.6","0.85","1.5","2.5")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1))) +
  annotate("rect", xmin = ymd_hms("2020-04-09 11:00:00"), xmax = ymd_hms("2020-04-09 15:00:00"), ymin = 0, ymax = Inf,
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
