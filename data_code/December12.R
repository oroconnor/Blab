library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)
library(wesanderson)

airmasterk <- read_feather("airmasterk.feather")


#December 12 mass - Melted for legend
ggplot(subset(gather(airmasterk,`Particle Diameter (microns)`, Mass, mass0.4:mass2.5), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Mass, color = `Particle Diameter (microns)`)) +
  geom_point(size = .3, alpha = .4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,15)) +
  labs(
    x = " Dec 11                 Dec 12                Dec 13                    ",
    y = expression(Mass - (μg/~m^3)),
    title = paste(
    "Temperature Inversion: December 12 2020 ")
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



#Dec 12 Counts  .7-5.0
ggplot(subset(gather(airmasterk,Particle_Size, Counts, c0.7:c5.0), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "6 hours", date_labels = "%l %p") +
  scale_y_continuous(limits = c(0,7000)) +
  labs(
    x = "        Dec 11                 Dec 12                Dec 13            ",
    y = "Counts",
    title = paste(
      "Particulate Matter Counts: December 2020"
    ),
    subtitle = paste(
      "sizes 0.7 - 5.0"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.7","1.0","2.0","3.0","5.0")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1)))

#Same Counts but for the whole year
ggplot(subset(gather(airmasterk,Particle_Size, Counts, c0.7:c5.0)),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  #scale_y_continuous(limits = c(0,10000)) +
  labs(
    x = NULL,
    y = expression(Mass - (μg/~m^3)),
    title = paste(
      "Particulate Matter Counts: 2020"
    ),
    subtitle = paste(
      "sizes 0.7 - 5.0"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    text = element_text(family = "Helvetica")
  ) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling1"),labels = c("0.7","1.0","2.0","3.0","5.0")) +
  guides(colour = guide_legend(title = "Particle Diameter (\U00B5m)",title.theme = element_text(size =9),override.aes = list(size=3, alpha=1)))

#Dec 12 Counts  .3-10 Facetted 
ggplot(subset(transform(gather(airmasterk,Particle_Size, Counts, c0.3:c10.0),Particle_Size = factor(Particle_Size, levels = c("c0.3","c0.5","c0.7","c1.0","c2.0","c3.0","c5.0","c10.0"))), month(Time) == 12 & day(Time) > 10 & day(Time) < 14),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  labs(
    x = NULL,
    y = "Particle Counts",
    title = paste(
      "Particulate Matter Counts: December 2020"
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


#Whole Year Counts  .3-10 Facetted 
ggplot(transform(gather(airmasterk,Particle_Size, Counts, c0.3:c10.0),Particle_Size = factor(Particle_Size, levels = c("c0.3","c0.5","c0.7","c1.0","c2.0","c3.0","c5.0","c10.0"))),
       aes(Time,Counts, color = Particle_Size)) +
  geom_point(size= .3, alpha =.4) +
  labs(
    x = NULL,
    y = "Particle Counts",
    title = paste(
      "Particulate Matter Counts - 2020"
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
