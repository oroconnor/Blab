library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

airmasterk <- read_feather("airmasterk.feather")

july4masterk <- airmasterk %>%
  filter(
    month(Time) == 7,
    (day(Time) == 3 & hour(Time) > 15) | day(Time) == 4 | (day(Time) == 5 & hour(Time) < 17),

  )

#July 4th Mass Melted for Legend
ggplot(gather(july4masterk,`Particle Diameter (microns)`, Mass, mass0.4:mass2.5),
       aes(Time,Mass, color = `Particle Diameter (microns)`)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,15)) +
  labs(
    x = "Jul 3                     Jul 4                                      Jul 5  ",
    y = expression(Mass - (μg/~m^3)),
    title = paste(
      "July 4th 2020 ")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.4","0.6","0.85","1.5","2.5")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1)))


#July 4 mass
ggplot(subset(airmasterk, month(Time) == 7 & day(Time) > 3 & day(Time) < 7),
       aes(Time,y)) +
  geom_point(aes(y = m0.4), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = m0.6), size= .3, color = "orange", alpha =.4) +
  geom_point(aes(y = m0.85), size= .3, color = "yellow", alpha =.4) +
  geom_point(aes(y = m1.5), size= .3, color = "green", alpha =.4) +
  geom_point(aes(y = m2.5), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = m4.0), size= .3, color = "purple", alpha =.4) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d %l %p") +
  scale_y_continuous(limits = c(0,15)) +
  labs(
    y = "Mass - mg/cubic meter",
    title = paste(
      "July 2020 Mass by Particulate Size"
    ),
    subtitle = paste(
      "m.4-m4.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#Counts  
ggplot(subset(airmasterk, month(Time) == 7 & day(Time) > 3 & day(Time) < 7),
       aes(Time,y)) +
  geom_point(aes(y = c0.7), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = c1.0), size= .3, color = "orange", alpha =.4) +
  geom_point(aes(y = c2.0), size= .3, color = "yellow", alpha =.4) +
  geom_point(aes(y = c3.0), size= .3, color = "green", alpha =.4) +
  geom_point(aes(y = c5.0), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = c10.0), size= .3, color = "purple", alpha =.4) +
  scale_y_continuous(limits = c(0,7000)) +
  labs(
    y = "Counts",
    title = paste(
      "July 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "sizes 0.7 - 10.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))


ggplot(subset(airmasterk, month(Time) == 7 & day(Time) > 3 & day(Time) < 7),
       aes(Time,y)) +
  geom_point(aes(y = pm2.5), size= .3, color = "red", alpha =.4) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d %l %p") +
  #scale_y_continuous(limits = c(0,50)) +
  labs(
    y = "PM2.5 - mg/cubic meter",
    title = paste(
      "July 4/5/6 2020 "
    ),
    subtitle = paste(
      "PM2.5"
    )) +
  theme(axis.text.x=element_text(angle=60, hjust=1))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
 

##Graphs below this for July 4th


ggplot(subset(airmasterk, month(Time) == 7 & day(Time) == 4 & hour(Time) > 14),
       aes(Time,y)) +
  geom_point(aes(y = c0.7), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = c1.0), size= .3, color = "orange", alpha =.4) +
  geom_point(aes(y = c2.0), size= .3, color = "yellow", alpha =.4) +
  geom_point(aes(y = c3.0), size= .3, color = "green", alpha =.4) +
  geom_point(aes(y = c5.0), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = c10.0), size= .3, color = "purple", alpha =.4) +
  scale_y_continuous(limits = c(0,7000)) +
  labs(
    y = "Counts",
    title = paste(
      "Counts from Particulate Matter Sensor - July 4th 2020"
    ),
    subtitle = paste(
      "sizes 0.7 - 10.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))


#Particle mass without 7.5
ggplot(subset(airmasterk, month(Time) == 7 & day(Time) == 4 & hour(Time) > 14),
       aes(Time,y)) +
  geom_point(aes(y = m0.4), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = m0.6), size= .3, color = "orange", alpha =.4) +
  geom_point(aes(y = m0.85), size= .3, color = "yellow", alpha =.4) +
  geom_point(aes(y = m1.5), size= .3, color = "green", alpha =.4) +
  geom_point(aes(y = m2.5), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = m4.0), size= .3, color = "purple", alpha =.4) +
  scale_y_continuous(limits = c(0,15)) +
  labs(
    y = "Mass",
    title = paste(
      "July 4 2020 2020 Mass by Particulate Size"
    ),
    subtitle = paste(
      "m.4-m4.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))  


#Mass w 7.5
ggplot(subset(airmasterk, month(Time) == 7 & day(Time) == 4 & hour(Time) > 14),
       aes(Time,y)) +
  geom_point(aes(y = m0.4), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = m0.6), size= .3, color = "orange", alpha =.4) +
  geom_point(aes(y = m0.85), size= .3, color = "yellow", alpha =.4) +
  geom_point(aes(y = m1.5), size= .3, color = "green", alpha =.4) +
  geom_point(aes(y = m2.5), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = m4.0), size= .3, color = "purple", alpha =.4) +
  geom_point(aes(y = m7.5), size= .3, color = "pink", alpha =.4) +
  #scale_y_continuous(limits = c(0,50)) +
  labs(
    y = "Mass",
    title = paste(
      "July 4th 2020 Mass by Particulate Size"
    ),
    subtitle = paste(
      "m.4-m7.5"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))

#Counts Log scale
ggplot(subset(airmasterk, month(Time) == 7 & day(Time) == 4 & hour(Time) > 14),
       aes(Time,y)) +
  geom_point(aes(y = c0.3), size= .3, color = "turquoise", alpha =.4) +
  geom_point(aes(y = c0.7), size= .3, color = "red", alpha =.4) +
  geom_point(aes(y = c1.0), size= .3, color = "orange", alpha =.4) +
  geom_point(aes(y = c2.0), size= .3, color = "yellow", alpha =.4) +
  geom_point(aes(y = c3.0), size= .3, color = "green", alpha =.4) +
  geom_point(aes(y = c5.0), size= .3, color = "blue", alpha =.4) +
  geom_point(aes(y = c10.0), size= .3, color = "purple", alpha =.4) +
  scale_y_log10() +
  #scale_y_continuous(limits = c(0,7000)) +
  labs(
    y = "Counts",
    title = paste(
      "July 4 2020 Counts from Particulate Matter Sensor"
    ),
    subtitle = paste(
      "sizes 0.3 - 10.0"
    )
  ) +
  theme(axis.text.x=element_text(angle=60, hjust=1))

#July 4 Counts  .3-10 Facetted 
ggplot(transform(gather(july4masterk,Particle_Size, Counts, c0.3:c10.0),Particle_Size = factor(Particle_Size, levels = c("c0.3","c0.5","c0.7","c1.0","c2.0","c3.0","c5.0","c10.0")) ),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "12 hours", date_labels = "%b%e %l %p") +
  labs(
    x = NULL,
    y = "Particle Counts",
    title = paste(
      "Particulate Matter Counts: July 4, 2020"
    ),
    subtitle = paste(
      "sizes 0.3 - 10.0")
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica"),
    legend.position = "none"
  ) +
  facet_wrap(~Particle_Size, ncol = 4, scales = "free")