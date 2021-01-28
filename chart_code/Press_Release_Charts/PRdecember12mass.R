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
    y = "Mass - microns/cubic meter",
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

